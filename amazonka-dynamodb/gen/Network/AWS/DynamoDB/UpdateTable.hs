{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the provisioned throughput for the given table, or manages the
-- global secondary indexes on the table.
--
-- You can increase or decrease the table\'s provisioned throughput values
-- within the maximums and minimums listed in the
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits>
-- section in the /Amazon DynamoDB Developer Guide/.
--
-- In addition, you can use /UpdateTable/ to add, modify or delete global
-- secondary indexes on the table. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- The table must be in the @ACTIVE@ state for /UpdateTable/ to succeed.
-- /UpdateTable/ is an asynchronous operation; while executing the
-- operation, the table is in the @UPDATING@ state. While the table is in
-- the @UPDATING@ state, the table still has the provisioned throughput
-- from before the call. The table\'s new provisioned throughput settings
-- go into effect when the table returns to the @ACTIVE@ state; at that
-- point, the /UpdateTable/ operation is complete.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateTable.html>
module Network.AWS.DynamoDB.UpdateTable
    (
    -- * Request
      UpdateTable
    -- ** Request constructor
    , updateTable
    -- ** Request lenses
    , utProvisionedThroughput
    , utAttributeDefinitions
    , utGlobalSecondaryIndexUpdates
    , utTableName

    -- * Response
    , UpdateTableResponse
    -- ** Response constructor
    , updateTableResponse
    -- ** Response lenses
    , utrTableDescription
    , utrStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an /UpdateTable/ operation.
--
-- /See:/ 'updateTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utProvisionedThroughput'
--
-- * 'utAttributeDefinitions'
--
-- * 'utGlobalSecondaryIndexUpdates'
--
-- * 'utTableName'
data UpdateTable = UpdateTable'
    { _utProvisionedThroughput       :: !(Maybe ProvisionedThroughput)
    , _utAttributeDefinitions        :: !(Maybe [AttributeDefinition])
    , _utGlobalSecondaryIndexUpdates :: !(Maybe [GlobalSecondaryIndexUpdate])
    , _utTableName                   :: !Text
    } deriving (Eq,Read,Show)

-- | 'UpdateTable' smart constructor.
updateTable :: Text -> UpdateTable
updateTable pTableName =
    UpdateTable'
    { _utProvisionedThroughput = Nothing
    , _utAttributeDefinitions = Nothing
    , _utGlobalSecondaryIndexUpdates = Nothing
    , _utTableName = pTableName
    }

-- | FIXME: Undocumented member.
utProvisionedThroughput :: Lens' UpdateTable (Maybe ProvisionedThroughput)
utProvisionedThroughput = lens _utProvisionedThroughput (\ s a -> s{_utProvisionedThroughput = a});

-- | An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- /AttributeDefinitions/ must include the key element(s) of the new index.
utAttributeDefinitions :: Lens' UpdateTable [AttributeDefinition]
utAttributeDefinitions = lens _utAttributeDefinitions (\ s a -> s{_utAttributeDefinitions = a}) . _Default;

-- | An array of one or more global secondary indexes for the table. For each
-- index in the array, you can request one action:
--
-- -   /Create/ - add a new global secondary index to the table.
--
-- -   /Update/ - modify the provisioned throughput settings of an existing
--     global secondary index.
--
-- -   /Delete/ - remove a global secondary index from the table.
--
utGlobalSecondaryIndexUpdates :: Lens' UpdateTable [GlobalSecondaryIndexUpdate]
utGlobalSecondaryIndexUpdates = lens _utGlobalSecondaryIndexUpdates (\ s a -> s{_utGlobalSecondaryIndexUpdates = a}) . _Default;

-- | The name of the table to be updated.
utTableName :: Lens' UpdateTable Text
utTableName = lens _utTableName (\ s a -> s{_utTableName = a});

instance AWSRequest UpdateTable where
        type Sv UpdateTable = DynamoDB
        type Rs UpdateTable = UpdateTableResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTableResponse' <$>
                   (x .?> "TableDescription") <*> (pure s))

instance ToHeaders UpdateTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateTable where
        toJSON UpdateTable'{..}
          = object
              ["ProvisionedThroughput" .= _utProvisionedThroughput,
               "AttributeDefinitions" .= _utAttributeDefinitions,
               "GlobalSecondaryIndexUpdates" .=
                 _utGlobalSecondaryIndexUpdates,
               "TableName" .= _utTableName]

instance ToPath UpdateTable where
        toPath = const "/"

instance ToQuery UpdateTable where
        toQuery = const mempty

-- | Represents the output of an /UpdateTable/ operation.
--
-- /See:/ 'updateTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utrTableDescription'
--
-- * 'utrStatus'
data UpdateTableResponse = UpdateTableResponse'
    { _utrTableDescription :: !(Maybe TableDescription)
    , _utrStatus           :: !Status
    } deriving (Eq,Read,Show)

-- | 'UpdateTableResponse' smart constructor.
updateTableResponse :: Status -> UpdateTableResponse
updateTableResponse pStatus =
    UpdateTableResponse'
    { _utrTableDescription = Nothing
    , _utrStatus = pStatus
    }

-- | FIXME: Undocumented member.
utrTableDescription :: Lens' UpdateTableResponse (Maybe TableDescription)
utrTableDescription = lens _utrTableDescription (\ s a -> s{_utrTableDescription = a});

-- | FIXME: Undocumented member.
utrStatus :: Lens' UpdateTableResponse Status
utrStatus = lens _utrStatus (\ s a -> s{_utrStatus = a});
