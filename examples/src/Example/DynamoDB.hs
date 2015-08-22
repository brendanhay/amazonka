{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.DynamoDB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.DynamoDB where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString         (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           System.IO

printTables :: Region
               -- ^ Region to operate in.
            -> Maybe (ByteString, Int)
               -- ^ Custom endpoint port, such as (localhost, 80000).
            -> IO ()
printTables r h = do
    lgr <- newLogger Debug stdout
    env <- newEnv r Discover <&> envLogger .~ lgr

    runResourceT . runAWST env $ do
        say $ "Listing all tables in region " <> toText r
        paginateWith (redirect h) listTables
            =$= CL.concatMap (view ltrsTableNames)
             $$ CL.mapM_ (say . mappend "Table: ")

redirect :: Maybe (ByteString, Int) -> Service DynamoDB -> Service DynamoDB
redirect Nothing       = id
redirect (Just (h, p)) = svcEndpoint .~ const local
  where
    local = Endpoint
        { _endpointHost   = h
        , _endpointPort   = p
        , _endpointSecure = False
        , _endpointScope  = "us-east-1"
        }

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
