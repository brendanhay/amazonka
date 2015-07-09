{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- Module      : Test.AWS.Fixture
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Fixture
    ( module Test.AWS.Fixture
    , module Text.PrettyPrint.GenericPretty
    ) where

import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.Text           as A
import           Data.Bifunctor
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Data.ByteString.Lazy           as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary            as Conduit
import           Data.Proxy
import qualified Data.Text                      as Text
import           Data.Typeable
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Network.AWS.Data.XML
import           Network.AWS.Prelude            hiding ((<.>))
import           Network.AWS.Types
import           Network.HTTP.Client.Internal   hiding (Proxy, Request,
                                                 Response)
import qualified Network.HTTP.Client.Internal   as Client
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
-- import           Text.Show.Pretty
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

resp :: (AWSRequest a, Eq (Rs a), Out (Rs a))
     => TestName
     -> FilePath
     -> Proxy a
     -> Rs a
     -> TestTree
resp n f p e = testCase n (LBS.readFile f >>= mockResponse p >>= assertEq)
  where
    assertEq (Left  m) = assertFailure m
    assertEq (Right a) = unless (e == a) (assertFailure m)
      where
        m = "[Expected]:\n  " ++ prettyStyle s e ++ "\n["
             ++ f ++ "]:\n  " ++ prettyStyle s a

        s = Style
            { mode           = PageMode
            , lineLength     = 80
            , ribbonsPerLine = 1.5
            }

instance Out Text where
    docPrec _ = doc
    doc = doc . Text.unpack

instance Out (Time a)

instance Out UTCTime where
    docPrec _ = doc
    doc = doc . show

mockResponse :: forall a. (AWSService (Sv a), AWSRequest a)
             => Proxy a
             -> LazyByteString
             -> IO (Either String (Rs a))
mockResponse x lbs = do
    y <- runResourceT $ response l (service x) rq (Right rs)
    return $! first show (snd <$> y)
  where
    rq = undefined :: Request a

    l _ _ = return ()

    rs = Client.Response
        { responseStatus    = status200
        , responseVersion   = http11
        , responseHeaders   = mempty
        , responseBody      = newResumableSource (Conduit.sourceLbs lbs)
        , responseCookieJar = mempty
        , responseClose'    = ResponseClose (pure ())
        }

-- FIXME: .Types can be handled according to protocol isomorphisms.

-- query :: Name -> Q Exp
-- query n = do
--     let svc  = serviceName (show n)
--         name = svc <.> nameBase n <.> "query"
--         path = fixtures </> name

--     [| testCase name (check $(return sig) path decodeXML undefined) |]
--   where
--     sig    = SigE (ConE proxy') (AppT (ConT proxy') (ConT n))
--     proxy' = mkName "Proxy"

-- check :: (Eq a, Show a)
--       => Proxy a
--       -> FilePath
--       -> (LazyByteString -> Either String a)
--       -> (a -> LazyByteString)
--       -> Assertion
-- check _ p f g = do
--     x <- LBS.readFile p
--     (g <$> f x) @=? Right x

-- serviceName :: String -> String
-- serviceName s = either (const s) unSN . fromText $ Text.pack s

-- newtype ServiceName = SN { unSN :: String }

-- instance FromText ServiceName where
--     parser = SN . Text.unpack
--          <$> (A.string "Network.AWS." *> A.takeWhile1 (/= '.'))
