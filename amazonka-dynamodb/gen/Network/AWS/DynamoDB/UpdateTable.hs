{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the provisioned throughput settings, global secondary indexes,
-- or DynamoDB Streams settings for a given table.
--
-- You can only perform one of the following operations at once:
--
-- -   Modify the provisioned throughput settings of the table.
--
-- -   Enable or disable Streams on the table.
--
-- -   Remove a global secondary index from the table.
--
-- -   Create a new global secondary index on the table. Once the index
--     begins backfilling, you can use /UpdateTable/ to perform other
--     operations.
--
-- /UpdateTable/ is an asynchronous operation; while it is executing, the
-- table status changes from @ACTIVE@ to @UPDATING@. While it is
-- @UPDATING@, you cannot issue another /UpdateTable/ request. When the
-- table returns to the @ACTIVE@ state, the /UpdateTable/ operation is
-- complete.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateTable.html>
module Network.AWS.DynamoDB.UpdateTable
    (
    -- * Request
      UpdateTable
    -- ** Request constructor
    , updateTable
    -- ** Request lenses
    , utrqProvisionedThroughput
    , utrqAttributeDefinitions
    , utrqGlobalSecondaryIndexUpdates
    , utrqStreamSpecification
    , utrqTableName

    -- * Response
    , UpdateTableResponse
    -- ** Response constructor
    , updateTableResponse
    -- ** Response lenses
    , utrsTableDescription
    , utrsStatus
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
-- * 'utrqProvisionedThroughput'
--
-- * 'utrqAttributeDefinitions'
--
-- * 'utrqGlobalSecondaryIndexUpdates'
--
-- * 'utrqStreamSpecification'
--
-- * 'utrqTableName'
data UpdateTable = UpdateTable'
    { _utrqProvisionedThroughput       :: !(Maybe ProvisionedThroughput)
    , _utrqAttributeDefinitions        :: !(Maybe [AttributeDefinition])
    , _utrqGlobalSecondaryIndexUpdates :: !(Maybe [GlobalSecondaryIndexUpdate])
    , _utrqStreamSpecification         :: !(Maybe StreamSpecification)
    , _utrqTableName                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateTable' smart constructor.
updateTable :: Text -> UpdateTable
updateTable pTableName =
    UpdateTable'
    { _utrqProvisionedThroughput = Nothing
    , _utrqAttributeDefinitions = Nothing
    , _utrqGlobalSecondaryIndexUpdates = Nothing
    , _utrqStreamSpecification = Nothing
    , _utrqTableName = pTableName
    }

-- | FIXME: Undocumented member.
utrqProvisionedThroughput :: Lens' UpdateTable (Maybe ProvisionedThroughput)
utrqProvisionedThroughput = lens _utrqProvisionedThroughput (\ s a -> s{_utrqProvisionedThroughput = a});

-- | An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- /AttributeDefinitions/ must include the key element(s) of the new index.
utrqAttributeDefinitions :: Lens' UpdateTable [AttributeDefinition]
utrqAttributeDefinitions = lens _utrqAttributeDefinitions (\ s a -> s{_utrqAttributeDefinitions = a}) . _Default;

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
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes>
-- in the /Amazon DynamoDB Developer Guide/.
utrqGlobalSecondaryIndexUpdates :: Lens' UpdateTable [GlobalSecondaryIndexUpdate]
utrqGlobalSecondaryIndexUpdates = lens _utrqGlobalSecondaryIndexUpdates (\ s a -> s{_utrqGlobalSecondaryIndexUpdates = a}) . _Default;

-- | Represents the DynamoDB Streams configuration for the table.
--
-- You will receive a /ResourceInUseException/ if you attempt to enable a
-- stream on a table that already has a stream, or if you attempt to
-- disable a stream on a table which does not have a stream.
utrqStreamSpecification :: Lens' UpdateTable (Maybe StreamSpecification)
utrqStreamSpecification = lens _utrqStreamSpecification (\ s a -> s{_utrqStreamSpecification = a});

-- | The name of the table to be updated.
utrqTableName :: Lens' UpdateTable Text
utrqTableName = lens _utrqTableName (\ s a -> s{_utrqTableName = a});

instance AWSRequest UpdateTable where
        type Sv UpdateTable = DynamoDB
        type Rs UpdateTable = UpdateTableResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTableResponse' <$>
                   (x .?> "TableDescription") <*> (pure (fromEnum s)))

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
              ["ProvisionedThroughput" .=
                 _utrqProvisionedThroughput,
               "AttributeDefinitions" .= _utrqAttributeDefinitions,
               "GlobalSecondaryIndexUpdates" .=
                 _utrqGlobalSecondaryIndexUpdates,
               "StreamSpecification" .= _utrqStreamSpecification,
               "TableName" .= _utrqTableName]

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
-- * 'utrsTableDescription'
--
-- * 'utrsStatus'
data UpdateTableResponse = UpdateTableResponse'
    { _utrsTableDescription :: !(Maybe TableDescription)
    , _utrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateTableResponse' smart constructor.
updateTableResponse :: Int -> UpdateTableResponse
updateTableResponse pStatus =
    UpdateTableResponse'
    { _utrsTableDescription = Nothing
    , _utrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
utrsTableDescription :: Lens' UpdateTableResponse (Maybe TableDescription)
utrsTableDescription = lens _utrsTableDescription (\ s a -> s{_utrsTableDescription = a});

-- | FIXME: Undocumented member.
utrsStatus :: Lens' UpdateTableResponse Int
utrsStatus = lens _utrsStatus (\ s a -> s{_utrsStatus = a});
