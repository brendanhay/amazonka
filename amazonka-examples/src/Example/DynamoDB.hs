{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.DynamoDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- The examples in this module illustrate the use of 'setEndpoint' to allow
-- for <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html local development>
-- using DynamoDB. If you plan on developing against remote AWS DynamoDB, then
-- you can omit the 'setEndpoint' and 'reconfigure' steps below.
module Example.DynamoDB where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString         (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           System.IO

printTables :: Region
               -- ^ Region to operate in.
            -> Bool
               -- ^ Whether to use HTTPS (ie. SSL).
            -> ByteString
               -- ^ The hostname to connect to.
            -> Int
               -- ^ The port number to connect to.
            -> IO ()
printTables region secure host port = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    -- Specify a custom DynamoDB endpoint to communicate with:
    let dynamo = setEndpoint secure host port dynamoDB

    runResourceT . runAWST env . within region $ do
        -- Scoping the endpoint change using 'reconfigure':
        reconfigure dynamo $ do
            say $ "Listing all tables in region " <> toText region
            paginate listTables
                =$= CL.concatMap (view ltrsTableNames)
                 $$ CL.mapM_ (say . mappend "Table: ")

        -- This will _not_ use the redirected endpoint, and will hit AWS directly.
        say $ "Listing all tables in region " <> toText region
        paginate listTables
            =$= CL.concatMap (view ltrsTableNames)
             $$ CL.mapM_ (say . mappend "Table: ")

    -- You can also hardcode the endpoint in the initial environment
    -- by manually constructing it:
    -- let env' = setEndpoint s h p env
    -- runResourceT . runAWST env' $ do
    --     ...

insertItem :: Region
              -- ^ Region to operate in.
           -> Bool
              -- ^ Whether to use HTTPS (ie. SSL).
           -> ByteString
              -- ^ The hostname to connect to.
           -> Int
              -- ^ The port number to connect to.
           -> Text
              -- ^ The table to insert the item into.
           -> HashMap Text AttributeValue
              -- ^ The attribute name-value pairs that constitute an item.
           -> IO PutItemResponse
insertItem region secure host port table item = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    -- Specify a custom DynamoDB endpoint to communicate with:
    let dynamo = setEndpoint secure host port dynamoDB

    runResourceT . runAWST env . within region $ do
        -- Scoping the endpoint change using 'reconfigure':
        reconfigure dynamo $ do
            say $ "Inserting item into table '"
               <> table
               <> "' with attribute names: "
               <> Text.intercalate ", " (Map.keys item)
            -- Insert the new item into the specified table:
            send $ putItem table & piItem .~ item

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
