{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchUpdatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchUpdatePartition
  ( -- * Creating a Request
    batchUpdatePartition,
    BatchUpdatePartition,

    -- * Request Lenses
    bupCatalogId,
    bupDatabaseName,
    bupTableName,
    bupEntries,

    -- * Destructuring the Response
    batchUpdatePartitionResponse,
    BatchUpdatePartitionResponse,

    -- * Response Lenses
    buprsErrors,
    buprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchUpdatePartition' smart constructor.
data BatchUpdatePartition = BatchUpdatePartition'
  { _bupCatalogId ::
      !(Maybe Text),
    _bupDatabaseName :: !Text,
    _bupTableName :: !Text,
    _bupEntries ::
      !(List1 BatchUpdatePartitionRequestEntry)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdatePartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bupCatalogId' - The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
--
-- * 'bupDatabaseName' - The name of the metadata database in which the partition is to be updated.
--
-- * 'bupTableName' - The name of the metadata table in which the partition is to be updated.
--
-- * 'bupEntries' - A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
batchUpdatePartition ::
  -- | 'bupDatabaseName'
  Text ->
  -- | 'bupTableName'
  Text ->
  -- | 'bupEntries'
  NonEmpty BatchUpdatePartitionRequestEntry ->
  BatchUpdatePartition
batchUpdatePartition pDatabaseName_ pTableName_ pEntries_ =
  BatchUpdatePartition'
    { _bupCatalogId = Nothing,
      _bupDatabaseName = pDatabaseName_,
      _bupTableName = pTableName_,
      _bupEntries = _List1 # pEntries_
    }

-- | The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
bupCatalogId :: Lens' BatchUpdatePartition (Maybe Text)
bupCatalogId = lens _bupCatalogId (\s a -> s {_bupCatalogId = a})

-- | The name of the metadata database in which the partition is to be updated.
bupDatabaseName :: Lens' BatchUpdatePartition Text
bupDatabaseName = lens _bupDatabaseName (\s a -> s {_bupDatabaseName = a})

-- | The name of the metadata table in which the partition is to be updated.
bupTableName :: Lens' BatchUpdatePartition Text
bupTableName = lens _bupTableName (\s a -> s {_bupTableName = a})

-- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
bupEntries :: Lens' BatchUpdatePartition (NonEmpty BatchUpdatePartitionRequestEntry)
bupEntries = lens _bupEntries (\s a -> s {_bupEntries = a}) . _List1

instance AWSRequest BatchUpdatePartition where
  type Rs BatchUpdatePartition = BatchUpdatePartitionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          BatchUpdatePartitionResponse'
            <$> (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable BatchUpdatePartition

instance NFData BatchUpdatePartition

instance ToHeaders BatchUpdatePartition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.BatchUpdatePartition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchUpdatePartition where
  toJSON BatchUpdatePartition' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _bupCatalogId,
            Just ("DatabaseName" .= _bupDatabaseName),
            Just ("TableName" .= _bupTableName),
            Just ("Entries" .= _bupEntries)
          ]
      )

instance ToPath BatchUpdatePartition where
  toPath = const "/"

instance ToQuery BatchUpdatePartition where
  toQuery = const mempty

-- | /See:/ 'batchUpdatePartitionResponse' smart constructor.
data BatchUpdatePartitionResponse = BatchUpdatePartitionResponse'
  { _buprsErrors ::
      !( Maybe
           [BatchUpdatePartitionFailureEntry]
       ),
    _buprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdatePartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buprsErrors' - The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
--
-- * 'buprsResponseStatus' - -- | The response status code.
batchUpdatePartitionResponse ::
  -- | 'buprsResponseStatus'
  Int ->
  BatchUpdatePartitionResponse
batchUpdatePartitionResponse pResponseStatus_ =
  BatchUpdatePartitionResponse'
    { _buprsErrors = Nothing,
      _buprsResponseStatus = pResponseStatus_
    }

-- | The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
buprsErrors :: Lens' BatchUpdatePartitionResponse [BatchUpdatePartitionFailureEntry]
buprsErrors = lens _buprsErrors (\s a -> s {_buprsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
buprsResponseStatus :: Lens' BatchUpdatePartitionResponse Int
buprsResponseStatus = lens _buprsResponseStatus (\s a -> s {_buprsResponseStatus = a})

instance NFData BatchUpdatePartitionResponse
