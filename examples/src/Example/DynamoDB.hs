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
        -- Scoping the endpoint change using 'endpoint':
        endpoint (redirect h) $ do
            say $ "Listing all tables in region " <> toText r
            paginate listTables
                =$= CL.concatMap (view ltrsTableNames)
                 $$ CL.mapM_ (say . mappend "Table: ")

        -- This will _not_ use the redirected endpoint, and will hit AWS directly.
        say $ "Listing all tables in region " <> toText r
        paginate listTables
            =$= CL.concatMap (view ltrsTableNames)
             $$ CL.mapM_ (say . mappend "Table: ")

    -- You can also hardcode the endpoint in the initial environment
    -- by manually constructing it:
    -- let env' = Override (svcEndpoint .~ const (Endpoint ...))
    -- runResourceT . runAWST env' $

redirect :: Maybe (ByteString, Int) -> Endpoint -> Endpoint
redirect Nothing       = id
redirect (Just (h, p)) =
      (endpointHost   .~ h)
    . (endpointPort   .~ p)
    . (endpointSecure .~ (p == 443))

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
