{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.Amazonka.Fixture
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Fixture where

import Amazonka.Core
import Amazonka.Data
import Amazonka.Prelude
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Binary as Conduit
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HTTP
import Test.Amazonka.Assert
import Test.Amazonka.Orphans ()
import Test.Amazonka.TH
import Test.Tasty
import Test.Tasty.HUnit

res ::
  (AWSRequest a, Eq (AWSResponse a), Show (AWSResponse a)) =>
  TestName ->
  FilePath ->
  Service ->
  Proxy a ->
  AWSResponse a ->
  TestTree
res n f s p e =
  testCase n $
    LBS.readFile f
      >>= testResponse s p
      >>= assertDiff f e

req ::
  forall a.
  (AWSRequest a, Eq a, Show a) =>
  TestName ->
  FilePath ->
  a ->
  TestTree
req n f e = testCase n $ do
  a <- YAML.decodeFileEither f
  e' <- expected
  assertDiff f e' (first show a)
  where
    expected = do
      let x = signedRequest (requestSign (request id e) auth NorthVirginia time)
      b <- sink (Client.requestBody x)
      return $!
        mkReq
          (Client.method x)
          (Client.path x)
          (Client.queryString x)
          (Client.requestHeaders x)
          b

    sink = \case
      Client.RequestBodyLBS lbs -> pure (toBS lbs)
      Client.RequestBodyBS bs -> pure bs
      Client.RequestBodyBuilder _ b -> pure (toBS b)
      _ -> fail "Streaming body not supported."

testResponse ::
  forall a.
  AWSRequest a =>
  Service ->
  Proxy a ->
  ByteStringLazy ->
  IO (Either String (AWSResponse a))
testResponse s p lbs = do
  y <- runResourceT (response pure s p rs)

  return $! first show (Client.responseBody <$> y)
  where
    rs =
      Client.Response
        { responseStatus = HTTP.status200,
          responseVersion = HTTP.http11,
          responseHeaders = mempty,
          responseBody = Conduit.sourceLbs lbs,
          responseCookieJar = mempty,
          responseClose' = Client.ResponseClose (pure ())
        }

auth :: AuthEnv
auth = AuthEnv "access" "secret" Nothing Nothing

time :: UTCTime
time = $(mkTime "2009-10-28T22:32:00Z")

data Req = Req
  { _method :: Method,
    _path :: ByteString,
    _query :: ByteString,
    _headers :: [Header],
    _body :: ByteString
  }
  deriving (Eq, Show, Generic)

mkReq :: Method -> ByteString -> ByteString -> [Header] -> ByteString -> Req
mkReq m p q h = Req m p q (sortKeys h)

instance FromJSON Req where
  parseJSON = withObject "req" $ \o -> do
    headers <- o .:? "headers" .!= mempty

    mkReq
      <$> o .: "method"
      <*> (o .:? "path" .!= "/")
      <*> (o .:? "query" .!= mempty)
      <*> pure (map (bimap (CI.mk . Text.encodeUtf8) Text.encodeUtf8) headers)
      <*> (o .:? "body" .!= mempty)

sortKeys :: Ord a => [(a, b)] -> [(a, b)]
sortKeys = List.sortBy (Ord.comparing fst)
