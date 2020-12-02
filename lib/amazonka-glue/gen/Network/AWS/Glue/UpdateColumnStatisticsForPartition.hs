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
-- Module      : Network.AWS.Glue.UpdateColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates partition statistics of columns.
--
--
-- The Identity and Access Management (IAM) permission required for this operation is @UpdatePartition@ .
module Network.AWS.Glue.UpdateColumnStatisticsForPartition
  ( -- * Creating a Request
    updateColumnStatisticsForPartition,
    UpdateColumnStatisticsForPartition,

    -- * Request Lenses
    ucsfpCatalogId,
    ucsfpDatabaseName,
    ucsfpTableName,
    ucsfpPartitionValues,
    ucsfpColumnStatisticsList,

    -- * Destructuring the Response
    updateColumnStatisticsForPartitionResponse,
    UpdateColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    ucsfprsErrors,
    ucsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateColumnStatisticsForPartition' smart constructor.
data UpdateColumnStatisticsForPartition = UpdateColumnStatisticsForPartition'
  { _ucsfpCatalogId ::
      !(Maybe Text),
    _ucsfpDatabaseName ::
      !Text,
    _ucsfpTableName ::
      !Text,
    _ucsfpPartitionValues ::
      ![Text],
    _ucsfpColumnStatisticsList ::
      ![ColumnStatistics]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsfpCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'ucsfpDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'ucsfpTableName' - The name of the partitions' table.
--
-- * 'ucsfpPartitionValues' - A list of partition values identifying the partition.
--
-- * 'ucsfpColumnStatisticsList' - A list of the column statistics.
updateColumnStatisticsForPartition ::
  -- | 'ucsfpDatabaseName'
  Text ->
  -- | 'ucsfpTableName'
  Text ->
  UpdateColumnStatisticsForPartition
updateColumnStatisticsForPartition pDatabaseName_ pTableName_ =
  UpdateColumnStatisticsForPartition'
    { _ucsfpCatalogId = Nothing,
      _ucsfpDatabaseName = pDatabaseName_,
      _ucsfpTableName = pTableName_,
      _ucsfpPartitionValues = mempty,
      _ucsfpColumnStatisticsList = mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
ucsfpCatalogId :: Lens' UpdateColumnStatisticsForPartition (Maybe Text)
ucsfpCatalogId = lens _ucsfpCatalogId (\s a -> s {_ucsfpCatalogId = a})

-- | The name of the catalog database where the partitions reside.
ucsfpDatabaseName :: Lens' UpdateColumnStatisticsForPartition Text
ucsfpDatabaseName = lens _ucsfpDatabaseName (\s a -> s {_ucsfpDatabaseName = a})

-- | The name of the partitions' table.
ucsfpTableName :: Lens' UpdateColumnStatisticsForPartition Text
ucsfpTableName = lens _ucsfpTableName (\s a -> s {_ucsfpTableName = a})

-- | A list of partition values identifying the partition.
ucsfpPartitionValues :: Lens' UpdateColumnStatisticsForPartition [Text]
ucsfpPartitionValues = lens _ucsfpPartitionValues (\s a -> s {_ucsfpPartitionValues = a}) . _Coerce

-- | A list of the column statistics.
ucsfpColumnStatisticsList :: Lens' UpdateColumnStatisticsForPartition [ColumnStatistics]
ucsfpColumnStatisticsList = lens _ucsfpColumnStatisticsList (\s a -> s {_ucsfpColumnStatisticsList = a}) . _Coerce

instance AWSRequest UpdateColumnStatisticsForPartition where
  type
    Rs UpdateColumnStatisticsForPartition =
      UpdateColumnStatisticsForPartitionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForPartitionResponse'
            <$> (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable UpdateColumnStatisticsForPartition

instance NFData UpdateColumnStatisticsForPartition

instance ToHeaders UpdateColumnStatisticsForPartition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.UpdateColumnStatisticsForPartition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateColumnStatisticsForPartition where
  toJSON UpdateColumnStatisticsForPartition' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _ucsfpCatalogId,
            Just ("DatabaseName" .= _ucsfpDatabaseName),
            Just ("TableName" .= _ucsfpTableName),
            Just ("PartitionValues" .= _ucsfpPartitionValues),
            Just ("ColumnStatisticsList" .= _ucsfpColumnStatisticsList)
          ]
      )

instance ToPath UpdateColumnStatisticsForPartition where
  toPath = const "/"

instance ToQuery UpdateColumnStatisticsForPartition where
  toQuery = const mempty

-- | /See:/ 'updateColumnStatisticsForPartitionResponse' smart constructor.
data UpdateColumnStatisticsForPartitionResponse = UpdateColumnStatisticsForPartitionResponse'
  { _ucsfprsErrors ::
      !( Maybe
           [ColumnStatisticsError]
       ),
    _ucsfprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsfprsErrors' - Error occurred during updating column statistics data.
--
-- * 'ucsfprsResponseStatus' - -- | The response status code.
updateColumnStatisticsForPartitionResponse ::
  -- | 'ucsfprsResponseStatus'
  Int ->
  UpdateColumnStatisticsForPartitionResponse
updateColumnStatisticsForPartitionResponse pResponseStatus_ =
  UpdateColumnStatisticsForPartitionResponse'
    { _ucsfprsErrors =
        Nothing,
      _ucsfprsResponseStatus = pResponseStatus_
    }

-- | Error occurred during updating column statistics data.
ucsfprsErrors :: Lens' UpdateColumnStatisticsForPartitionResponse [ColumnStatisticsError]
ucsfprsErrors = lens _ucsfprsErrors (\s a -> s {_ucsfprsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
ucsfprsResponseStatus :: Lens' UpdateColumnStatisticsForPartitionResponse Int
ucsfprsResponseStatus = lens _ucsfprsResponseStatus (\s a -> s {_ucsfprsResponseStatus = a})

instance NFData UpdateColumnStatisticsForPartitionResponse
