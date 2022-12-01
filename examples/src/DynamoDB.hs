{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The examples in this module illustrate the use of 'setEndpoint' to allow
-- for <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html local development>
-- using DynamoDB. If you plan on developing against remote AWS DynamoDB, then
-- you can omit the 'setEndpoint' and 'configure' steps below.
module DynamoDB where

import Amazonka
import Amazonka.DynamoDB as DynamoDB
import Control.Lens
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Generics.Product
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

printTables ::
  -- | Region to operate in.
  Region ->
  -- | Whether to use HTTPS (ie. SSL).
  Bool ->
  -- | The hostname to connect to.
  ByteString ->
  -- | The port number to connect to.
  Int ->
  IO ()
printTables reg sec hst prt = do
  -- Specify a custom DynamoDB endpoint to communicate with:
  let dynamo = setEndpoint sec hst prt DynamoDB.defaultService

  lgr <- newLogger Debug stdout
  env <-
    newEnv discover
      <&> set (field @"logger") lgr
        . set (field @"region") reg
        . configureService dynamo

  runResourceT $ do
    say $ "Listing all tables in region " <> toText reg
    runConduit $
      paginate env newListTables
        .| CL.concatMap (view (field @"tableNames" . _Just))
        .| CL.mapM_ (say . mappend "Table: ")

insertItem ::
  -- | Region to operate in.
  Region ->
  -- | Whether to use HTTPS (ie. SSL).
  Bool ->
  -- | The hostname to connect to.
  ByteString ->
  -- | The port number to connect to.
  Int ->
  -- | The table to insert the item into.
  Text ->
  -- | The attribute name-value pairs that constitute an item.
  HashMap Text AttributeValue ->
  IO PutItemResponse
insertItem reg sec hst prt table item = do
  -- Specify a custom DynamoDB endpoint to communicate with:
  let dynamo = setEndpoint sec hst prt DynamoDB.defaultService

  lgr <- newLogger Debug stdout
  env <-
    newEnv discover
      <&> set (field @"logger") lgr
        . set (field @"region") reg
        . configureService dynamo

  runResourceT $ do
    say $
      "Inserting item into table '"
        <> table
        <> "' with attribute names: "
        <> Text.intercalate ", " (Map.keys item)
    -- Insert the new item into the specified table:
    send env $ newPutItem table & field @"item" .~ item

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
