{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

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
import           Control.Lens
import qualified Data.Attoparsec            as P
import           Data.Attoparsec.Char8      hiding (signed)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.CaseInsensitive       as CI
import qualified Data.Char                  as Char
import           Data.Default
import           Data.List                  (isSuffixOf)
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.V4
import           Network.AWS.Types
import           Network.HTTP.Client.Lens
import           Network.HTTP.Types
import           Prelude                    hiding (takeWhile)
import           System.Directory
import           System.FilePath
import           System.Locale
import           Test.Tasty
import           Test.Tasty.HUnit

data Test

data Ctx = Ctx
    { name  :: TestName
    , rq    :: Request Test
    , srq   :: ClientRequest
    , creq  :: ByteString
    , sts   :: ByteString
    , authz :: ByteString
    }

newtype WeakEq a = Eq ClientRequest
    deriving (Show)

instance Eq (WeakEq a) where
    (==) (Eq a) (Eq b) =
           view host           a == view host           b
        && view method         a == view method         b
        && view path           a == view path           b
        && view queryString    a == view queryString    b
        && view requestHeaders a == view requestHeaders b

instance AWSService Test where
    type Signer' Test = V4
    type Error'  Test = ()

    service = Service "host.foo.com" "host" "2011-08" Nothing

instance AWSRequest Test where
    type Service'  Test = Test
    type Response' Test = ()

    request  = undefined
    response = undefined

auth :: Auth
auth = Auth access secret Nothing Nothing

access, secret :: ByteString
access = "AKIDEXAMPLE"
secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

region :: Region
region = NorthVirginia

-- | Required due to errorneous date in the aws4 test suite.
locale :: TimeLocale
locale = defaultTimeLocale { wDays = cycle [("Monday", "Mon")] }

tests :: FilePath -> IO TestTree
tests dir = do
    v4rs <- getContexts (dir </> "test/resources/v4")
    return $ testGroup "Signing" [testGroup "Version 4" (map v4 v4rs)]
  where
    v4 Ctx{..} =
        let date = BS.unpack . fromMaybe "" $ "Date" `lookup` _rqHeaders rq
            time = readTime defaultTimeLocale rfc822DateFormat date
            sg   = signed service auth region rq locale time
            meta = _sgMeta sg

         in testGroup (name ++ ".req")
              [ testCase "Canonical Request" $ _mCReq meta @?= creq
              , testCase "String To Sign"    $ _mSTS  meta @?= sts
              , testCase "Authorisation"     $ authorisation meta @?= authz
              , testCase "Signed Request"    $ Eq (_sgRequest sg) @?= Eq srq
              ]

getContexts :: FilePath -> IO [Ctx]
getContexts dir = files <$> getDirectoryContents dir >>= mapM loadContext
  where
    files = map (combine dir . dropExtension) . filter (isSuffixOf ".req")

loadContext :: FilePath -> IO Ctx
loadContext f = Ctx (takeBaseName f)
    <$> (fst . parseRequest <$> load ".req")
    <*> (snd . parseRequest <$> load ".sreq")
    <*> load ".creq"
    <*> load ".sts"
    <*> load ".authz"
  where
    load = BS.readFile . addExtension f

parseRequest :: ByteString -> (Request Test, ClientRequest)
parseRequest = either error id . parseOnly req
  where
    req = do
        m  <- P.takeWhile1 token <* char8 ' '
        m' <- either (fail . BS.unpack) return (parseMethod m)
        p  <- BS.pack <$> manyTill' anyChar http
        hs <- many1 header
        b  <- endOfLine *> takeByteString <* endOfInput

        let (path', q) = BS.span (/= '?') p
            p'         = urlDecode False path'
            q'         = maybe "" snd $ BS.uncons q

        return
            ( def
                & rqMethod  .~ m'
                & rqPath    .~ p'
                & rqQuery   .~ decodeQuery q'
                & rqHeaders .~ hs
                & rqBody    .~ toBody b

            , clientRequest
                & method         .~ m
                & host           .~ fromMaybe "" (hHost `lookup` hs)
                & path           .~ p'
                & queryString    .~ q'
                & requestHeaders .~ hs
                & requestBody    .~ mempty
            )

    http = " http/" *> takeWhile (\c -> Char.isDigit c || c == '.') <* endOfLine

    header = (,) <$> (CI.mk <$> key) <*> val
      where
        key = P.takeWhile token <* char8 ':' <* P.skipWhile isHorizontalSpace
        val = P.takeTill isEndOfLine <* endOfLine

    token w = w <= 127 && P.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w
