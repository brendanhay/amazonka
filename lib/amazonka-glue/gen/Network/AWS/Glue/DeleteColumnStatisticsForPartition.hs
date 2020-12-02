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
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the partition column statistics of a column.
--
--
-- The Identity and Access Management (IAM) permission required for this operation is @DeletePartition@ .
module Network.AWS.Glue.DeleteColumnStatisticsForPartition
  ( -- * Creating a Request
    deleteColumnStatisticsForPartition,
    DeleteColumnStatisticsForPartition,

    -- * Request Lenses
    dcsfpCatalogId,
    dcsfpDatabaseName,
    dcsfpTableName,
    dcsfpPartitionValues,
    dcsfpColumnName,

    -- * Destructuring the Response
    deleteColumnStatisticsForPartitionResponse,
    DeleteColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    dcsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteColumnStatisticsForPartition' smart constructor.
data DeleteColumnStatisticsForPartition = DeleteColumnStatisticsForPartition'
  { _dcsfpCatalogId ::
      !(Maybe Text),
    _dcsfpDatabaseName ::
      !Text,
    _dcsfpTableName ::
      !Text,
    _dcsfpPartitionValues ::
      ![Text],
    _dcsfpColumnName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsfpCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'dcsfpDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'dcsfpTableName' - The name of the partitions' table.
--
-- * 'dcsfpPartitionValues' - A list of partition values identifying the partition.
--
-- * 'dcsfpColumnName' - Name of the column.
deleteColumnStatisticsForPartition ::
  -- | 'dcsfpDatabaseName'
  Text ->
  -- | 'dcsfpTableName'
  Text ->
  -- | 'dcsfpColumnName'
  Text ->
  DeleteColumnStatisticsForPartition
deleteColumnStatisticsForPartition
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForPartition'
      { _dcsfpCatalogId = Nothing,
        _dcsfpDatabaseName = pDatabaseName_,
        _dcsfpTableName = pTableName_,
        _dcsfpPartitionValues = mempty,
        _dcsfpColumnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
dcsfpCatalogId :: Lens' DeleteColumnStatisticsForPartition (Maybe Text)
dcsfpCatalogId = lens _dcsfpCatalogId (\s a -> s {_dcsfpCatalogId = a})

-- | The name of the catalog database where the partitions reside.
dcsfpDatabaseName :: Lens' DeleteColumnStatisticsForPartition Text
dcsfpDatabaseName = lens _dcsfpDatabaseName (\s a -> s {_dcsfpDatabaseName = a})

-- | The name of the partitions' table.
dcsfpTableName :: Lens' DeleteColumnStatisticsForPartition Text
dcsfpTableName = lens _dcsfpTableName (\s a -> s {_dcsfpTableName = a})

-- | A list of partition values identifying the partition.
dcsfpPartitionValues :: Lens' DeleteColumnStatisticsForPartition [Text]
dcsfpPartitionValues = lens _dcsfpPartitionValues (\s a -> s {_dcsfpPartitionValues = a}) . _Coerce

-- | Name of the column.
dcsfpColumnName :: Lens' DeleteColumnStatisticsForPartition Text
dcsfpColumnName = lens _dcsfpColumnName (\s a -> s {_dcsfpColumnName = a})

instance AWSRequest DeleteColumnStatisticsForPartition where
  type
    Rs DeleteColumnStatisticsForPartition =
      DeleteColumnStatisticsForPartitionResponse
  request = postJSON glue
  response =
    receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForPartitionResponse'
            <$> (pure (fromEnum s))
      )

instance Hashable DeleteColumnStatisticsForPartition

instance NFData DeleteColumnStatisticsForPartition

instance ToHeaders DeleteColumnStatisticsForPartition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.DeleteColumnStatisticsForPartition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteColumnStatisticsForPartition where
  toJSON DeleteColumnStatisticsForPartition' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _dcsfpCatalogId,
            Just ("DatabaseName" .= _dcsfpDatabaseName),
            Just ("TableName" .= _dcsfpTableName),
            Just ("PartitionValues" .= _dcsfpPartitionValues),
            Just ("ColumnName" .= _dcsfpColumnName)
          ]
      )

instance ToPath DeleteColumnStatisticsForPartition where
  toPath = const "/"

instance ToQuery DeleteColumnStatisticsForPartition where
  toQuery = const mempty

-- | /See:/ 'deleteColumnStatisticsForPartitionResponse' smart constructor.
newtype DeleteColumnStatisticsForPartitionResponse = DeleteColumnStatisticsForPartitionResponse'
  { _dcsfprsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsfprsResponseStatus' - -- | The response status code.
deleteColumnStatisticsForPartitionResponse ::
  -- | 'dcsfprsResponseStatus'
  Int ->
  DeleteColumnStatisticsForPartitionResponse
deleteColumnStatisticsForPartitionResponse pResponseStatus_ =
  DeleteColumnStatisticsForPartitionResponse'
    { _dcsfprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcsfprsResponseStatus :: Lens' DeleteColumnStatisticsForPartitionResponse Int
dcsfprsResponseStatus = lens _dcsfprsResponseStatus (\s a -> s {_dcsfprsResponseStatus = a})

instance NFData DeleteColumnStatisticsForPartitionResponse
