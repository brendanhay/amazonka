{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Test.AWS.Fixture
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Fixture where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Attoparsec.ByteString     as B
import qualified Data.Attoparsec.Text           as A
import           Data.Bifunctor
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary            as Conduit
import qualified Data.HashMap.Strict            as Map
import           Data.Proxy
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Typeable
import           Data.Yaml
import           GHC.Generics                   (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.XML
import           Network.AWS.Prelude            hiding ((<.>))
import           Network.AWS.Types
import           Network.HTTP.Client.Internal   hiding (Proxy, Request,
                                                 Response)
import qualified Network.HTTP.Client.Internal   as Client
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath
import           Test.AWS.Assert
import           Test.AWS.Orphans
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Text.PrettyPrint.GenericPretty

res :: (AWSRequest a, Eq (Rs a), Out (Rs a))
    => TestName
    -> FilePath
    -> Proxy a
    -> Rs a
    -> TestTree
res n f p e = testCase n $
        LBS.readFile f
    >>= testResponse p
    >>= assertEqualPP f e

req :: (AWSRequest a, Eq a, Out a)
    => TestName
    -> FilePath
    -> a
    -> TestTree
req n f e = testCase n $ do
    e' <- expected
    a  <- decodeFileEither f
    assertEqualPP f e' (first show a)
  where
    expected = do
        let x = request e
        b <- sink (_bdyBody (_rqBody x))
        return $!
            Req (_rqMethod x)
                (_rqPath   x)
                (parseSimpleQuery (toBS (_rqQuery x)))
                (_rqHeaders x)
                b

    sink = \case
        RequestBodyLBS     lbs -> pure (toBS lbs)
        RequestBodyBS      bs  -> pure bs
        RequestBodyBuilder _ b -> pure (toBS b)
        _                      -> fail "Streaming body not supported."

data Req = Req
    { testMethod  :: !StdMethod
    , testPath    :: ByteString
    , testQuery   :: SimpleQuery
    , testHeaders :: [Header]
    , testBody    :: ByteString
    } deriving (Eq, Show, Generic)

instance Out Req

instance FromJSON Req where
    parseJSON = withObject "req" $ \o -> Req
        <$> o .:  "method"
        <*> o .:  "path"
        <*> (o .: "query"   <&> parseSimpleQuery)
        <*> (o .: "headers" <&> Map.toList)
        <*> o .:? "body"    .!= mempty

testResponse :: forall a. (AWSService (Sv a), AWSRequest a)
             => Proxy a
             -> LazyByteString
             -> IO (Either String (Rs a))
testResponse x lbs = do
    y <- runResourceT $ response l (service x) rq (Right rs)
    return $! first show (snd <$> y)
  where
    l _ _ = return ()

    rq = undefined :: Request a

    rs = Client.Response
        { responseStatus    = status200
        , responseVersion   = http11
        , responseHeaders   = mempty
        , responseBody      = newResumableSource (Conduit.sourceLbs lbs)
        , responseCookieJar = mempty
        , responseClose'    = ResponseClose (pure ())
        }
