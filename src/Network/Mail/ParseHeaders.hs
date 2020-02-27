
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Mail.ParseHeaders
  (
    parseHeaders
  , lookupHeaders
  , Header
  )
  where

import Data.Attoparsec.ByteString as Att
import qualified Data.Word8 as W
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString ( ByteString )
import Control.Monad
import qualified Data.ListLike as LL
import Data.Char

type Header = (ByteString, ByteString)

headers ::
  Parser [Header]
headers = many1 header

-- | returns (fieldName, fieldValue)
header :: Parser Header
header = do
      fn <- fieldName
      void sep
      firstLine <- chomp <$> untilLineEnd
      contLines <- many' (chomp <$> continuedHeader)
      return (fn, BS.intercalate " " (firstLine : contLines))
  where
    sep :: Parser ()
    sep = void $ word8 W._colon >> Att.takeWhile isWhiteSpace 


data LineState = Normal | SeenCR | SeenCRLF
  deriving (Eq, Show)

pattern LF :: Word8
pattern LF = 10

pattern CR :: Word8
pattern CR = 13

isWhiteSpace :: Word8 -> Bool
isWhiteSpace w =
  w == W._space || w == W._tab

fieldName :: Parser ByteString
fieldName =
      Att.takeWhile (\c -> c /= W._colon &&
                           not (isWhiteSpace c)) 

-- predicate for use w/ attoparsec's "scan" --
-- state machine to see if we hit CRLF yet.
lineEnd :: LineState -> Word8 -> Maybe LineState
lineEnd st w = 
  case (st, w) of
    (Normal, CR)   -> Just SeenCR
    (Normal, _)    -> Just Normal
    (SeenCR, LF)   -> Just SeenCRLF
    (SeenCR, _)    -> Just Normal
    (SeenCRLF, _)  -> Nothing

-- | chomp line ends.
-- (actually, any sequence of CR and LF
-- at a line-end)
chomp :: ByteString -> ByteString
chomp = LL.dropWhileEnd (\c -> c == CR || c == LF)

untilLineEnd :: Parser ByteString
untilLineEnd = scan Normal lineEnd

-- | If the next line is a part of a continued header, parse it.
-- Fail otherwise
continuedHeader :: Parser BS.ByteString
continuedHeader = satisfy isWhiteSpace *>
                  Att.takeWhile isWhiteSpace *>
                  untilLineEnd

-- | extract email headers from a bytestring
parseHeaders :: ByteString -> Either String [Header]
parseHeaders = parseOnly headers

-- | specify the name of an RFC 5322 field name,
-- and this will return all headers in the message
-- matching that field name.
lookupHeaders ::
  ByteString -> [Header] -> [ByteString]
lookupHeaders fieldName hs =
    map snd $ filter matches hs
  where
    matches h = BSC.map toLower (fst h) == BSC.map toLower fieldName

