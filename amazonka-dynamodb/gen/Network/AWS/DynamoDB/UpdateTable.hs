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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the provisioned throughput for the given table, or manages the global
-- secondary indexes on the table.
--
-- You can increase or decrease the table's provisioned throughput values
-- within the maximums and minimums listed in the <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> section in the /AmazonDynamoDB Developer Guide/.
--
-- In addition, you can use /UpdateTable/ to add, modify or delete global
-- secondary indexes on the table. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing GlobalSecondary Indexes> in the /Amazon DynamoDB Developer Guide/.
--
-- The table must be in the 'ACTIVE' state for /UpdateTable/ to succeed. /UpdateTable/
-- is an asynchronous operation; while executing the operation, the table is in
-- the 'UPDATING' state. While the table is in the 'UPDATING' state, the table still
-- has the provisioned throughput from before the call. The table's new
-- provisioned throughput settings go into effect when the table returns to the 'ACTIVE' state; at that point, the /UpdateTable/ operation is complete.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateTable.html>
module Network.AWS.DynamoDB.UpdateTable
    (
    -- * Request
      UpdateTable
    -- ** Request constructor
    , updateTable
    -- ** Request lenses
    , utAttributeDefinitions
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
    { _utAttributeDefinitions        :: List "AttributeDefinitions" AttributeDefinition
    , _utGlobalSecondaryIndexUpdates :: List "GlobalSecondaryIndexUpdates" GlobalSecondaryIndexUpdate
    , _utProvisionedThroughput       :: Maybe ProvisionedThroughput
    , _utTableName                   :: Text
    } deriving (Eq, Read, Show)

-- | 'UpdateTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utAttributeDefinitions' @::@ ['AttributeDefinition']
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
    , _utAttributeDefinitions        = mempty
    , _utProvisionedThroughput       = Nothing
    , _utGlobalSecondaryIndexUpdates = mempty
    }

-- | An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table, /AttributeDefinitions/ must include the key element(s) of the new index.
utAttributeDefinitions :: Lens' UpdateTable [AttributeDefinition]
utAttributeDefinitions =
    lens _utAttributeDefinitions (\s a -> s { _utAttributeDefinitions = a })
        . _List

-- | An array of one or more global secondary indexes for the table. For each
-- index in the array, you can specify one action:
--
-- /Create/ - add a new global secondary index to the table.
--
-- /Update/ - modify the provisioned throughput settings of an existing global
-- secondary index.
--
-- /Delete/ - remove a global secondary index from the table.
--
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
utTableName :: Lens' UpdateTable Text
utTableName = lens _utTableName (\s a -> s { _utTableName = a })

newtype UpdateTableResponse = UpdateTableResponse
    { _utrTableDescription :: Maybe TableDescription
    } deriving (Eq, Read, Show)

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
        [ "AttributeDefinitions"        .= _utAttributeDefinitions
        , "TableName"                   .= _utTableName
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
