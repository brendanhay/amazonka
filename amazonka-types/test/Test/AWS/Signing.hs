{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import qualified Data.Attoparsec           as P
import           Data.Attoparsec.Char8
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.Char                 as Char
import           Data.List                 (isSuffixOf)
import           Data.Maybe
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Signing.V4
import           Network.AWS.Types
import           Network.HTTP.Types
import           Prelude                   hiding (takeWhile)
import           System.Directory
import           System.FilePath
import           System.Locale
import           Test.Tasty
import           Test.Tasty.HUnit


data Test

data Ctx = Ctx
    { name :: TestName
    , rq   :: Request Test
    , srq  :: Request ()
    , meta :: Meta V4
    }

newtype WeakEq a = Eq (Request a)
    deriving (Show)

instance Eq (WeakEq a) where
    (==) (Eq a) (Eq b) = f rqMethod && f rqPath && f rqQuery && f rqHeaders
      where
        f g = g a == g b

dummy :: Service Test V4
dummy = Service "host.foo.com" "host" "2011-08" Nothing

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
        let date = BS.unpack . fromMaybe "" $ "Date" `lookup` rqHeaders rq
            time = readTime defaultTimeLocale rfc822DateFormat date
            sg   = finalise dummy rq auth region locale time
            eq f = f (sgMeta sg) @?= f meta

         in testGroup (name ++ ".req")
              [ testCase "Canonical Request" $ eq mCanon
              , testCase "String To Sign"    $ eq mSTS
              , testCase "Authorisation"     $ eq mAuth
              , testCase "Signed Request"    $ Eq (sgRequest sg) @?= Eq srq
              ]

getContexts :: FilePath -> IO [Ctx]
getContexts dir = files <$> getDirectoryContents dir >>= mapM loadContext
  where
    files = map (combine dir . dropExtension) . filter (isSuffixOf ".req")

loadContext :: FilePath -> IO Ctx
loadContext path = Ctx (takeBaseName path)
    <$> (parseRequest <$> load ".req")
    <*> (parseRequest <$> load ".sreq")
    <*> (Meta <$> load ".creq" <*> load ".authz" <*> load ".sts")
  where
    load = BS.readFile . addExtension path

parseRequest :: ByteString -> Request a
parseRequest = either error id . parseOnly req
  where
    req = do
        m  <- P.takeWhile1 token <* char8 ' '
        m' <- either (fail . BS.unpack) return (parseMethod m)
        p  <- takeWhile1 (not . Char.isSpace) <* http
        hs <- many1 header
        b  <- endOfLine *> takeByteString <* endOfInput

        let (path, q) = BS.span (/= '?') p
            query     = decodeQuery . maybe "" snd $ BS.uncons q
            (bdy,  h) = byteStringBody b

        return $! Request
            { rqMethod  = m'
            , rqPath    = urlDecode False path
            , rqQuery   = query
            , rqHeaders = hs
            , rqBody    = bdy
            , rqSHA256  = h
            }

    http = " http/" *> takeWhile (\c -> Char.isDigit c || c == '.') <* endOfLine

    header = (,) <$> (CI.mk <$> key) <*> val
      where
        key = P.takeWhile token <* char8 ':' <* P.skipWhile isHorizontalSpace
        val = P.takeTill isEndOfLine <* endOfLine

    token w = w <= 127 && P.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w
