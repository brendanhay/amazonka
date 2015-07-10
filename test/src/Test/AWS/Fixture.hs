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
import           Data.List                      (sortBy)
import           Data.Ord
import           Data.Proxy
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Time
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
import           Test.AWS.TH
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
    a  <- decodeFileEither f
    e' <- expected
    assertEqualPP f e' (first show a)

  where
    expected = do
        let rq = request e
            sg = signed auth NorthVirginia time (serviceOf e) rq
            x  = sg ^. sgRequest
        b <- sink (requestBody x)
        return $! mkReq
            (method x)
            (path   x)
            (parseSimpleQuery (queryString x))
            (requestHeaders x)
            b

    sink = \case
        RequestBodyLBS     lbs -> pure (toBS lbs)
        RequestBodyBS      bs  -> pure bs
        RequestBodyBuilder _ b -> pure (toBS b)
        _                      -> fail "Streaming body not supported."

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

auth :: AuthEnv
auth = AuthEnv "access" "secret" Nothing Nothing

time :: UTCTime
time = $(mkTime "2009-10-28T22:32:00Z")

data Req = Req
    { _method  :: Method
    , _path    :: ByteString
    , _query   :: SimpleQuery
    , _headers :: [Header]
    , _body    :: ByteString
    } deriving (Eq, Generic)

mkReq :: Method -> ByteString -> SimpleQuery -> [Header] -> ByteString -> Req
mkReq m p q h b = Req m p (sortKeys q) (sortKeys h) b

instance Out Req

instance FromJSON Req where
    parseJSON = withObject "req" $ \o -> mkReq
        <$> o .:  "method"
        <*> o .:  "path"
        <*> (o .: "query"   <&> Map.toList)
        <*> (o .: "headers" <&> Map.toList)
        <*> o .:? "body"    .!= mempty

sortKeys :: Ord a => [(a, b)] -> [(a, b)]
sortKeys = sortBy (comparing fst)
