{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetPartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partitions in a batch request.
--
--
module Network.AWS.Glue.BatchGetPartition
    (
    -- * Creating a Request
      batchGetPartition
    , BatchGetPartition
    -- * Request Lenses
    , bgpCatalogId
    , bgpDatabaseName
    , bgpTableName
    , bgpPartitionsToGet

    -- * Destructuring the Response
    , batchGetPartitionResponse
    , BatchGetPartitionResponse
    -- * Response Lenses
    , bgprsUnprocessedKeys
    , bgprsPartitions
    , bgprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetPartition' smart constructor.
data BatchGetPartition = BatchGetPartition'
  { _bgpCatalogId       :: !(Maybe Text)
  , _bgpDatabaseName    :: !Text
  , _bgpTableName       :: !Text
  , _bgpPartitionsToGet :: ![PartitionValueList]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetPartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgpCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'bgpDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'bgpTableName' - The name of the partitions' table.
--
-- * 'bgpPartitionsToGet' - A list of partition values identifying the partitions to retrieve.
batchGetPartition
    :: Text -- ^ 'bgpDatabaseName'
    -> Text -- ^ 'bgpTableName'
    -> BatchGetPartition
batchGetPartition pDatabaseName_ pTableName_ =
  BatchGetPartition'
    { _bgpCatalogId = Nothing
    , _bgpDatabaseName = pDatabaseName_
    , _bgpTableName = pTableName_
    , _bgpPartitionsToGet = mempty
    }


-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
bgpCatalogId :: Lens' BatchGetPartition (Maybe Text)
bgpCatalogId = lens _bgpCatalogId (\ s a -> s{_bgpCatalogId = a})

-- | The name of the catalog database where the partitions reside.
bgpDatabaseName :: Lens' BatchGetPartition Text
bgpDatabaseName = lens _bgpDatabaseName (\ s a -> s{_bgpDatabaseName = a})

-- | The name of the partitions' table.
bgpTableName :: Lens' BatchGetPartition Text
bgpTableName = lens _bgpTableName (\ s a -> s{_bgpTableName = a})

-- | A list of partition values identifying the partitions to retrieve.
bgpPartitionsToGet :: Lens' BatchGetPartition [PartitionValueList]
bgpPartitionsToGet = lens _bgpPartitionsToGet (\ s a -> s{_bgpPartitionsToGet = a}) . _Coerce

instance AWSRequest BatchGetPartition where
        type Rs BatchGetPartition = BatchGetPartitionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetPartitionResponse' <$>
                   (x .?> "UnprocessedKeys" .!@ mempty) <*>
                     (x .?> "Partitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetPartition where

instance NFData BatchGetPartition where

instance ToHeaders BatchGetPartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchGetPartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetPartition where
        toJSON BatchGetPartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bgpCatalogId,
                  Just ("DatabaseName" .= _bgpDatabaseName),
                  Just ("TableName" .= _bgpTableName),
                  Just ("PartitionsToGet" .= _bgpPartitionsToGet)])

instance ToPath BatchGetPartition where
        toPath = const "/"

instance ToQuery BatchGetPartition where
        toQuery = const mempty

-- | /See:/ 'batchGetPartitionResponse' smart constructor.
data BatchGetPartitionResponse = BatchGetPartitionResponse'
  { _bgprsUnprocessedKeys :: !(Maybe [PartitionValueList])
  , _bgprsPartitions      :: !(Maybe [Partition])
  , _bgprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetPartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgprsUnprocessedKeys' - A list of the partition values in the request for which partions were not returned.
--
-- * 'bgprsPartitions' - A list of the requested partitions.
--
-- * 'bgprsResponseStatus' - -- | The response status code.
batchGetPartitionResponse
    :: Int -- ^ 'bgprsResponseStatus'
    -> BatchGetPartitionResponse
batchGetPartitionResponse pResponseStatus_ =
  BatchGetPartitionResponse'
    { _bgprsUnprocessedKeys = Nothing
    , _bgprsPartitions = Nothing
    , _bgprsResponseStatus = pResponseStatus_
    }


-- | A list of the partition values in the request for which partions were not returned.
bgprsUnprocessedKeys :: Lens' BatchGetPartitionResponse [PartitionValueList]
bgprsUnprocessedKeys = lens _bgprsUnprocessedKeys (\ s a -> s{_bgprsUnprocessedKeys = a}) . _Default . _Coerce

-- | A list of the requested partitions.
bgprsPartitions :: Lens' BatchGetPartitionResponse [Partition]
bgprsPartitions = lens _bgprsPartitions (\ s a -> s{_bgprsPartitions = a}) . _Default . _Coerce

-- | -- | The response status code.
bgprsResponseStatus :: Lens' BatchGetPartitionResponse Int
bgprsResponseStatus = lens _bgprsResponseStatus (\ s a -> s{_bgprsResponseStatus = a})

instance NFData BatchGetPartitionResponse where
