{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the provisioned throughput for the given table. Setting the
-- throughput for a table helps you manage performance and is part of the
-- provisioned throughput feature of DynamoDB.
--
-- The provisioned throughput values can be upgraded or downgraded based on the
-- maximums and minimums listed in the <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> section in the /Amazon DynamoDBDeveloper Guide/.
--
-- The table must be in the 'ACTIVE' state for this operation to succeed. /UpdateTable/ is an asynchronous operation; while executing the operation, the table is in
-- the 'UPDATING' state. While the table is in the 'UPDATING' state, the table still
-- has the provisioned throughput from before the call. The new provisioned
-- throughput setting is in effect only when the table returns to the 'ACTIVE'
-- state after the /UpdateTable/ operation.
--
-- You cannot add, modify or delete indexes using /UpdateTable/. Indexes can only
-- be defined at table creation time.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateTable.html>
module Network.AWS.DynamoDB.UpdateTable
    (
    -- * Request
      UpdateTable
    -- ** Request constructor
    , updateTable
    -- ** Request lenses
    , utGlobalSecondaryIndexUpdates
    , utProvisionedThroughput
    , utTableName

    -- * Response
    , UpdateTableResponse
    -- ** Response constructor
    , updateTableResponse
    -- ** Response lenses
    , utrTableDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data UpdateTable = UpdateTable
    { _utGlobalSecondaryIndexUpdates :: List "GlobalSecondaryIndexUpdates" GlobalSecondaryIndexUpdate
    , _utProvisionedThroughput       :: Maybe ProvisionedThroughput
    , _utTableName                   :: Text
    } deriving (Eq, Show)

-- | 'UpdateTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utGlobalSecondaryIndexUpdates' @::@ ['GlobalSecondaryIndexUpdate']
--
-- * 'utProvisionedThroughput' @::@ 'Maybe' 'ProvisionedThroughput'
--
-- * 'utTableName' @::@ 'Text'
--
updateTable :: Text -- ^ 'utTableName'
            -> UpdateTable
updateTable p1 = UpdateTable
    { _utTableName                   = p1
    , _utProvisionedThroughput       = Nothing
    , _utGlobalSecondaryIndexUpdates = mempty
    }

-- | An array of one or more global secondary indexes on the table, together with
-- provisioned throughput settings for each index.
--
utGlobalSecondaryIndexUpdates :: Lens' UpdateTable [GlobalSecondaryIndexUpdate]
utGlobalSecondaryIndexUpdates =
    lens _utGlobalSecondaryIndexUpdates
        (\s a -> s { _utGlobalSecondaryIndexUpdates = a })
            . _List

utProvisionedThroughput :: Lens' UpdateTable (Maybe ProvisionedThroughput)
utProvisionedThroughput =
    lens _utProvisionedThroughput (\s a -> s { _utProvisionedThroughput = a })

-- | The name of the table to be updated.
--
utTableName :: Lens' UpdateTable Text
utTableName = lens _utTableName (\s a -> s { _utTableName = a })

newtype UpdateTableResponse = UpdateTableResponse
    { _utrTableDescription :: Maybe TableDescription
    } deriving (Eq, Show)

-- | 'UpdateTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utrTableDescription' @::@ 'Maybe' 'TableDescription'
--
updateTableResponse :: UpdateTableResponse
updateTableResponse = UpdateTableResponse
    { _utrTableDescription = Nothing
    }

utrTableDescription :: Lens' UpdateTableResponse (Maybe TableDescription)
utrTableDescription =
    lens _utrTableDescription (\s a -> s { _utrTableDescription = a })

instance ToPath UpdateTable where
    toPath = const "/"

instance ToQuery UpdateTable where
    toQuery = const mempty

instance ToHeaders UpdateTable

instance ToJSON UpdateTable where
    toJSON UpdateTable{..} = object
        [ "TableName"                   .= _utTableName
        , "ProvisionedThroughput"       .= _utProvisionedThroughput
        , "GlobalSecondaryIndexUpdates" .= _utGlobalSecondaryIndexUpdates
        ]

instance AWSRequest UpdateTable where
    type Sv UpdateTable = DynamoDB
    type Rs UpdateTable = UpdateTableResponse

    request  = post "UpdateTable"
    response = jsonResponse

instance FromJSON UpdateTableResponse where
    parseJSON = withObject "UpdateTableResponse" $ \o -> UpdateTableResponse
        <$> o .:? "TableDescription"
