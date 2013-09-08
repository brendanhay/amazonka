{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.Signing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Signing (tests) where

import           Control.Applicative
import           Data.Attoparsec                as P
import           Data.Attoparsec.Char8          (char8, endOfLine, isDigit_w8)
import qualified Data.Attoparsec.Char8          as P8
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS
import           Data.Time
import           Network.AWS.Internal
import           Network.HTTP.Types             (urlDecode)
import           System.Directory
import           System.Locale
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test, path)

tests :: [Test]
tests =
    [ testGroup "Version4"
        [ testSigner "get-header-key-duplicate"
        ]
    ]

testSigner :: String -> Test
testSigner name = testCase name $ do
    (rq, expect) <- loadMetadata SigningVersion4 name

    let hdr  = BS.unpack . fromMaybe "" $ "Date" `lookup` rqHeaders rq
        time = readTime defaultTimeLocale "%a, %0d %b %Y %H:%M:%S GMT" hdr

    (_ , actual) <- sign' rq auth NorthVirgnia time

    -- when (actual /= expect) . putStrLn $ unlines
    --     [ show rq
    --     , ""
    --     , show actual
    --     , ""
    --     , show expect
    --     , ""
    --     ]

    assertEqual "Signing result does not match:" expect actual
  where
    auth = Auth "AKIDEXAMPLE" "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

    -- AKIDEXAMPLE/20110909/us-east-1/host/aws4_request
    -- wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY

    -- read DATE or Date headers format
    -- Mon, 09 Sep 2011 23:36:00 GMT

loadMetadata :: SigningVersion -> String -> IO (RawRequest, SigningMetadata)
loadMetadata ver name = do
    (,) <$> (parseRequest <$> load ".req")
        <*> (SigningMetadata
                <$> load ".creq"
                <*> load ".sreq"
                <*> load ".sts"
                <*> load ".authz")
  where
    load ext = BS.readFile =<< path ext `fmap` getCurrentDirectory
    path ext = (++ ("/test/resources/" ++ show ver ++ "/" ++ name ++ ext))

parseRequest :: ByteString -> RawRequest
parseRequest = either error id . parseOnly parser
  where
    parser = do
        meth <- P.takeWhile1 token <* char8 ' '
        uri  <- P.takeWhile1 (/= 32) <* char8 ' '
        _    <- "http/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46) <* endOfLine
        hdrs <- many1 header

        let (path, q) = BS.span (/= '?') uri
            qry       = decodeQuery (urlDecode True) . maybe "" snd $ BS.uncons q

        return $! RawRequest
            service (read $ BS.unpack meth) FormEncoded path hdrs qry Nothing

    header = (,)
        <$> P.takeWhile token <* char8 ':' <* skipWhile P8.isHorizontalSpace
        <*> takeTill P8.isEndOfLine <* endOfLine

    token w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

    service = Service "host" "2011-09-09" SigningVersion4 (const "host.foo.com")

-- "get-header-key-duplicate"
-- "get-header-value-order"
-- "get-header-value-trim"
-- "get-relative-relative"
-- "get-relative"
-- "get-slash-dot-slash"
-- "get-slash-pointless-dot"
-- "get-slash"
-- "get-slashes"
-- "get-space"
-- "get-unreserved"
-- "get-utf8"
-- "get-vanilla-empty-query-key"
-- "get-vanilla-query-order-key-case"
-- "get-vanilla-query-order-key"
-- "get-vanilla-query-order-value"
-- "get-vanilla-query-unreserved"
-- "get-vanilla-query"
-- "get-vanilla-ut8-query"
-- "get-vanilla"
-- "post-header-key-case"
-- "post-header-key-sort"
-- "post-header-value-case"
-- "post-vanilla-empty-query-value"
-- "post-vanilla-query-nonunreserved"
-- "post-vanilla-query-space"
-- "post-vanilla-query"
-- "post-vanilla"
-- "post-x-www-form-urlencoded-parameters"
-- "post-x-www-form-urlencoded"
