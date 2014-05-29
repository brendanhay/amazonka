{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Test.AWS.Signing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Signing (tests) where

import           Control.Applicative
import qualified Data.CaseInsensitive      as CI
import qualified Data.Char                 as Char
import           Data.List                 (isSuffixOf)
import           Data.Maybe
import           Prelude                   hiding (takeWhile)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.Attoparsec           as P
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8     as BS
import           Data.Monoid
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Network.AWS.Signing.V4
import           Network.AWS.Types
import           Network.AWS.Data
import           Network.HTTP.Types.Method
import           System.Locale
import           Data.Time
import           Test.Tasty.HUnit

tests :: FilePath -> IO TestTree
tests dir = do
    v4 <- getRequests (dir </> "test/resources/v4")
    return $ testGroup "Signing"
        [ testGroup "Version 4" (map version4 v4)
        ]

version4 :: (TestName, Request Test, Meta V4) -> TestTree
version4 (name, rq, meta) = testGroup name
    [ testCase "Canonical Request" $ mRequest meta @?= mRequest meta'
    ]
  where
    Signed _ rq' meta' = finalise dummy rq auth NorthVirginia time

    auth = Auth "AKIDEXAMPLE" "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" Nothing Nothing
    date = BS.unpack . fromMaybe "" $ "Date" `lookup` rqHeaders rq
    time = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" date

data Test

dummy :: Service Test V4
dummy = Service "host" "S3" "2011-08" Nothing

getRequests :: FilePath -> IO [(TestName, Request Test, Meta V4)]
getRequests dir = files <$> getDirectoryContents dir >>= mapM loadRequest
  where
    files = map (combine dir . dropExtension) . filter (isSuffixOf ".req")

loadRequest :: FilePath -> IO (TestName, Request Test, Meta V4)
loadRequest path = do
    rq   <- parseRequest <$> load ".req"
    meta <- Meta
        <$> load ".creq"
        <*> load ".sreq"
        <*> load ".authz"
        <*> load ".sts"
    return (takeBaseName path, rq, meta)
  where
    load = BS.readFile . addExtension path

parseRequest :: ByteString -> Request Test
parseRequest = either error id . parseOnly parser
  where
    parser = do
        m  <- P.takeWhile1 token <* char8 ' '
        m' <- either (fail . BS.unpack) return (parseMethod m)
        p  <- takeWhile1 (not . Char.isSpace) <* http
        hs <- many1 header

        let (path, q) = BS.span (/= '?') p
            query     = mempty -- decodeQuery (urlDecode True) . maybe "" snd $ BS.uncons q

        return $! Request
            { rqMethod  = m'
            , rqPath    = path
            , rqQuery   = query
            , rqHeaders = hs
            , rqBody    = mempty
            }

    http = " http/" *> takeWhile (\c -> Char.isDigit c || c == '.') <* endOfLine

    header = (,) <$> (CI.mk <$> key) <*> val
      where
        key = P.takeWhile token <* char8 ':' <* P.skipWhile isHorizontalSpace
        val = P.takeTill isEndOfLine <* endOfLine

    token w = w <= 127 && P.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w
