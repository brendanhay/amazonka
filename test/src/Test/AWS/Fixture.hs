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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Fixture where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Conduit.Binary          as Conduit
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (sortBy)
import           Data.Ord
import           Data.Proxy
import           Data.Time
import qualified Data.Yaml                    as YAML
import           Network.AWS.Data.ByteString
import qualified Network.AWS.Data.Map         as Map
import           Network.AWS.Lens             (trying, (<&>))
import           Network.AWS.Prelude
import           Network.HTTP.Client.Internal hiding (Proxy, Request, Response)
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Types
import           Test.AWS.Assert
import           Test.AWS.Orphans             ()
import           Test.AWS.TH
import           Test.Tasty
import           Test.Tasty.HUnit

res :: (AWSRequest a, Eq (Rs a), Show (Rs a))
    => TestName
    -> FilePath
    -> Service
    -> Proxy a
    -> Rs a
    -> TestTree
res n f s p e = testCase n $
        LBS.readFile f
    >>= testResponse s p
    >>= assertDiff f e

req :: (AWSRequest a, Eq a, Show a)
    => TestName
    -> FilePath
    -> a
    -> TestTree
req n f e = testCase n $ do
    a  <- YAML.decodeFileEither f
    e' <- expected
    assertDiff f e' (first show a)
  where
    expected = do
        let x = sgRequest (rqSign (request e) auth NorthVirginia time)
        b <- sink (requestBody x)
        return $! mkReq
            (method x)
            (path   x)
            (queryString x)
            (requestHeaders x)
            b

    sink = \case
        RequestBodyLBS     lbs -> pure (toBS lbs)
        RequestBodyBS      bs  -> pure bs
        RequestBodyBuilder _ b -> pure (toBS b)
        _                      -> fail "Streaming body not supported."

testResponse :: forall a. AWSRequest a
             => Service
             -> Proxy a
             -> LazyByteString
             -> IO (Either String (Rs a))
testResponse s p lbs = do
    y <- trying _Error $ runResourceT (response l s p rs)
    return $! first show (snd <$> y)
  where
    l _ _ = return ()

    rs = Client.Response
        { responseStatus    = status200
        , responseVersion   = http11
        , responseHeaders   = mempty
        , responseBody      = Conduit.sourceLbs lbs
        , responseCookieJar = mempty
        , responseClose'    = ResponseClose (pure ())
        }

auth :: AuthEnv
auth = AuthEnv "access" "secret" Nothing Nothing

time :: UTCTime
time = $(mkTime "2009-10-28T22:32:00Z")

data Req = Req
    { _method  :: Method
    , _path    :: ByteString
    , _query   :: ByteString
    , _headers :: [Header]
    , _body    :: ByteString
    } deriving (Eq, Show, Generic)

mkReq :: Method -> ByteString -> ByteString -> [Header] -> ByteString -> Req
mkReq m p q h = Req m p q (sortKeys h)

instance FromJSON Req where
    parseJSON = withObject "req" $ \o -> mkReq
        <$> o .: "method"
        <*> (o .:? "path"    .!= "/")
        <*> (o .:? "query"   .!= mempty)
        <*> (o .:? "headers" .!= mempty <&> Map.toList . Map.toMap)
        <*> (o .:? "body"    .!= mempty)

sortKeys :: Ord a => [(a, b)] -> [(a, b)]
sortKeys = sortBy (comparing fst)
