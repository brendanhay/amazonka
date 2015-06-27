{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- Module      : Test.AWS.Fixture
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Fixture where

import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.Text         as A
import           Data.Bifunctor
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Proxy
import qualified Data.Text                    as Text
import           Data.Typeable
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Network.AWS.Data.XML
import           Network.AWS.Prelude          hiding ((<.>))
import           Network.AWS.Types
import           Network.HTTP.Client.Internal hiding (Proxy, Request, Response)
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

-- FIXME: make test pathing more robust.
fixtures :: FilePath
fixtures = "fixture"

-- FIXME: .Types can be handled according to protocol isomorphisms.

-- FIXME: responses will have to be constructed manually.

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

resp :: forall a.
        (AWSRequest a, Eq (Rs a), Show (Rs a), Typeable (Rs a))
     => Proxy a
     -> Rs a
     -> TestTree
resp p e = testCase name $ do
    x <- LBS.readFile path
    a <- runResourceT (mock p x)
    Right e @=? first show (snd <$> a)
  where
    path = fixtures </> name
    name = m <.> n <.> "query"

    m = serviceName (tyConModule t)
    n = tyConName t
    t = typeRepTyCon (typeOf e)

mock :: forall m a.
        (MonadResource m, AWSService (Sv a), AWSRequest a)
     => Proxy a
     -> LazyByteString
     -> m (Response a)
mock x lbs = response l (service x) rq (Right rs)
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

serviceName :: String -> String
serviceName s = either (const s) unSN . fromText $ Text.pack s

newtype ServiceName = SN { unSN :: String }

instance FromText ServiceName where
    parser = SN . Text.unpack
         <$> (A.string "Network.AWS." *> A.takeWhile1 (/= '.'))

--    bs <- embed path
-- testCase "input"
-- embed :: FilePath -> Q Exp
-- embed p = do
--     bs <- runIO (BS.readFile p)
--     f  <- [| stringToBs |]
--     let s = BS8.unpack bs
--     return $! AppE f $! LitE $! StringL s

-- goldenVsString
-- :: TestName
-- -> FilePath path to the «golden» file (the file that contains correct output)
-- -> IO ByteString action that returns a string
-- -> TestTree test verifies that the returned string is the same as the golden file contents
-- dots :: String -> Bool
-- dots "."  = False
-- dots ".." = False
-- dots _    = True

-- run :: [Fixture] -> [TestTree]
-- run _ = []

-- data Protocol
--     = JSON
--     | RestJSON
--     | RestXML
--     | Query
--     | EC2
--       deriving (Eq, Show)

-- instance FromText Protocol where
--     parser = matchCI "json"     JSON
--          <|> matchCI "restjson" RestJSON
--          <|> matchCI "restxml"  RestXML
--          <|> matchCI "query"    Query
--          <|> matchCI "ec2"      EC2

-- instance FromText Fixture where
--     parser = Fixture
--         <$> A.takeWhile1 (/= '.')
--         <*> A.takeWhile1 (/= '.')
--         <*> parser

-- data Fixture = Fixture
--     { service  :: Text
--     , typeOf   :: Text
--     , protocol :: Protocol
--     }

-- fixture :: Fixture -> Assertion
-- fixture Fixture{..} = undefined
