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
-- Module      : Network.AWS.Glue.GetColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partition statistics of columns.
--
--
-- The Identity and Access Management (IAM) permission required for this operation is @GetPartition@ .
module Network.AWS.Glue.GetColumnStatisticsForPartition
  ( -- * Creating a Request
    getColumnStatisticsForPartition,
    GetColumnStatisticsForPartition,

    -- * Request Lenses
    gcsfpCatalogId,
    gcsfpDatabaseName,
    gcsfpTableName,
    gcsfpPartitionValues,
    gcsfpColumnNames,

    -- * Destructuring the Response
    getColumnStatisticsForPartitionResponse,
    GetColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    gcsfprsErrors,
    gcsfprsColumnStatisticsList,
    gcsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getColumnStatisticsForPartition' smart constructor.
data GetColumnStatisticsForPartition = GetColumnStatisticsForPartition'
  { _gcsfpCatalogId ::
      !(Maybe Text),
    _gcsfpDatabaseName :: !Text,
    _gcsfpTableName :: !Text,
    _gcsfpPartitionValues ::
      ![Text],
    _gcsfpColumnNames ::
      ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsfpCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'gcsfpDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'gcsfpTableName' - The name of the partitions' table.
--
-- * 'gcsfpPartitionValues' - A list of partition values identifying the partition.
--
-- * 'gcsfpColumnNames' - A list of the column names.
getColumnStatisticsForPartition ::
  -- | 'gcsfpDatabaseName'
  Text ->
  -- | 'gcsfpTableName'
  Text ->
  GetColumnStatisticsForPartition
getColumnStatisticsForPartition pDatabaseName_ pTableName_ =
  GetColumnStatisticsForPartition'
    { _gcsfpCatalogId = Nothing,
      _gcsfpDatabaseName = pDatabaseName_,
      _gcsfpTableName = pTableName_,
      _gcsfpPartitionValues = mempty,
      _gcsfpColumnNames = mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
gcsfpCatalogId :: Lens' GetColumnStatisticsForPartition (Maybe Text)
gcsfpCatalogId = lens _gcsfpCatalogId (\s a -> s {_gcsfpCatalogId = a})

-- | The name of the catalog database where the partitions reside.
gcsfpDatabaseName :: Lens' GetColumnStatisticsForPartition Text
gcsfpDatabaseName = lens _gcsfpDatabaseName (\s a -> s {_gcsfpDatabaseName = a})

-- | The name of the partitions' table.
gcsfpTableName :: Lens' GetColumnStatisticsForPartition Text
gcsfpTableName = lens _gcsfpTableName (\s a -> s {_gcsfpTableName = a})

-- | A list of partition values identifying the partition.
gcsfpPartitionValues :: Lens' GetColumnStatisticsForPartition [Text]
gcsfpPartitionValues = lens _gcsfpPartitionValues (\s a -> s {_gcsfpPartitionValues = a}) . _Coerce

-- | A list of the column names.
gcsfpColumnNames :: Lens' GetColumnStatisticsForPartition [Text]
gcsfpColumnNames = lens _gcsfpColumnNames (\s a -> s {_gcsfpColumnNames = a}) . _Coerce

instance AWSRequest GetColumnStatisticsForPartition where
  type
    Rs GetColumnStatisticsForPartition =
      GetColumnStatisticsForPartitionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetColumnStatisticsForPartitionResponse'
            <$> (x .?> "Errors" .!@ mempty)
            <*> (x .?> "ColumnStatisticsList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetColumnStatisticsForPartition

instance NFData GetColumnStatisticsForPartition

instance ToHeaders GetColumnStatisticsForPartition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.GetColumnStatisticsForPartition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetColumnStatisticsForPartition where
  toJSON GetColumnStatisticsForPartition' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gcsfpCatalogId,
            Just ("DatabaseName" .= _gcsfpDatabaseName),
            Just ("TableName" .= _gcsfpTableName),
            Just ("PartitionValues" .= _gcsfpPartitionValues),
            Just ("ColumnNames" .= _gcsfpColumnNames)
          ]
      )

instance ToPath GetColumnStatisticsForPartition where
  toPath = const "/"

instance ToQuery GetColumnStatisticsForPartition where
  toQuery = const mempty

-- | /See:/ 'getColumnStatisticsForPartitionResponse' smart constructor.
data GetColumnStatisticsForPartitionResponse = GetColumnStatisticsForPartitionResponse'
  { _gcsfprsErrors ::
      !( Maybe
           [ColumnError]
       ),
    _gcsfprsColumnStatisticsList ::
      !( Maybe
           [ColumnStatistics]
       ),
    _gcsfprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsfprsErrors' - Error occurred during retrieving column statistics data.
--
-- * 'gcsfprsColumnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
--
-- * 'gcsfprsResponseStatus' - -- | The response status code.
getColumnStatisticsForPartitionResponse ::
  -- | 'gcsfprsResponseStatus'
  Int ->
  GetColumnStatisticsForPartitionResponse
getColumnStatisticsForPartitionResponse pResponseStatus_ =
  GetColumnStatisticsForPartitionResponse'
    { _gcsfprsErrors =
        Nothing,
      _gcsfprsColumnStatisticsList = Nothing,
      _gcsfprsResponseStatus = pResponseStatus_
    }

-- | Error occurred during retrieving column statistics data.
gcsfprsErrors :: Lens' GetColumnStatisticsForPartitionResponse [ColumnError]
gcsfprsErrors = lens _gcsfprsErrors (\s a -> s {_gcsfprsErrors = a}) . _Default . _Coerce

-- | List of ColumnStatistics that failed to be retrieved.
gcsfprsColumnStatisticsList :: Lens' GetColumnStatisticsForPartitionResponse [ColumnStatistics]
gcsfprsColumnStatisticsList = lens _gcsfprsColumnStatisticsList (\s a -> s {_gcsfprsColumnStatisticsList = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsfprsResponseStatus :: Lens' GetColumnStatisticsForPartitionResponse Int
gcsfprsResponseStatus = lens _gcsfprsResponseStatus (\s a -> s {_gcsfprsResponseStatus = a})

instance NFData GetColumnStatisticsForPartitionResponse
