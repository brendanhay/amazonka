{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

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
import           Control.Monad
import           Data.Attoparsec                as P
import           Data.Attoparsec.Char8          (char8, endOfLine, isDigit_w8)
import qualified Data.Attoparsec.Char8          as P8
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS
import           Data.List                      ((\\))
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
        -- , testSigner "get-header-value-order"
        -- , testSigner "get-header-value-trim"
        -- , testSigner "get-relative-relative"
        -- , testSigner "get-relative"
        -- , testSigner "get-slash-dot-slash"
        -- , testSigner "get-slash-pointless-dot"
        -- , testSigner "get-slash"
        -- , testSigner "get-slashes"
        -- , testSigner "get-space"
        -- , testSigner "get-unreserved"
        -- , testSigner "get-utf8"
        , testSigner "get-vanilla-empty-query-key"
        , testSigner "get-vanilla-query-order-key-case"
        , testSigner "get-vanilla-query-order-key"
        , testSigner "get-vanilla-query-order-value"
        , testSigner "get-vanilla-query-unreserved"
        , testSigner "get-vanilla-query"
        -- , testSigner "get-vanilla-ut8-query"
        , testSigner "get-vanilla"
        -- , testSigner "post-header-key-case"
        -- , testSigner "post-header-key-sort"
        -- , testSigner "post-header-value-case"
        -- , testSigner "post-vanilla-empty-query-value"
        -- , testSigner "post-vanilla-query-nonunreserved"
        -- , testSigner "post-vanilla-query-space"
        -- , testSigner "post-vanilla-query"
        -- , testSigner "post-vanilla"
        -- , testSigner "post-x-www-form-urlencoded-parameters"
        -- , testSigner "post-x-www-form-urlencoded"
        ]
    ]

testSigner :: String -> Test
testSigner name = testCase name $ do
    (rq, expect) <- loadMetadata SigningVersion4 name

    let hdr  = BS.unpack . fromMaybe "" $ "Date" `lookup` rqHeaders rq
        time = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" hdr

    (_ , actual) <- sign' rq auth NorthVirgnia time

    when (actual /= expect) $ do
        let f = \(BS.unpack -> x) (BS.unpack -> y) -> (x \\ y) ++ (y \\ x)

        print $ f (smdCReq expect) (smdCReq actual)
        print $ f (smdSTS expect) (smdSTS actual)
        print $ f (smdAuthz expect) (smdAuthz actual)
        print $ f (smdSReq expect) (smdSReq actual)

    assertEqual "Signing results do not match." expect actual
  where
    auth = Auth "AKIDEXAMPLE" "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

loadMetadata :: SigningVersion -> String -> IO (RawRequest, SigningMetadata)
loadMetadata ver name = do
    (,) <$> (parseRequest <$> load ".req")
        <*> (SigningMetadata
                <$> load ".creq"
                <*> load ".sts"
                <*> load ".authz"
                <*> load ".sreq")
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
