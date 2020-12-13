{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- The Identity and Access Management (IAM) permission required for this operation is @GetPartition@ .
module Network.AWS.Glue.GetColumnStatisticsForPartition
  ( -- * Creating a request
    GetColumnStatisticsForPartition (..),
    mkGetColumnStatisticsForPartition,

    -- ** Request lenses
    gcsfpCatalogId,
    gcsfpDatabaseName,
    gcsfpColumnNames,
    gcsfpPartitionValues,
    gcsfpTableName,

    -- * Destructuring the response
    GetColumnStatisticsForPartitionResponse (..),
    mkGetColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    gcsfprsErrors,
    gcsfprsColumnStatisticsList,
    gcsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetColumnStatisticsForPartition' smart constructor.
data GetColumnStatisticsForPartition = GetColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Lude.Text,
    -- | A list of the column names.
    columnNames :: [Lude.Text],
    -- | A list of partition values identifying the partition.
    partitionValues :: [Lude.Text],
    -- | The name of the partitions' table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'columnNames' - A list of the column names.
-- * 'partitionValues' - A list of partition values identifying the partition.
-- * 'tableName' - The name of the partitions' table.
mkGetColumnStatisticsForPartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetColumnStatisticsForPartition
mkGetColumnStatisticsForPartition pDatabaseName_ pTableName_ =
  GetColumnStatisticsForPartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      columnNames = Lude.mempty,
      partitionValues = Lude.mempty,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpCatalogId :: Lens.Lens' GetColumnStatisticsForPartition (Lude.Maybe Lude.Text)
gcsfpCatalogId = Lens.lens (catalogId :: GetColumnStatisticsForPartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetColumnStatisticsForPartition)
{-# DEPRECATED gcsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpDatabaseName :: Lens.Lens' GetColumnStatisticsForPartition Lude.Text
gcsfpDatabaseName = Lens.lens (databaseName :: GetColumnStatisticsForPartition -> Lude.Text) (\s a -> s {databaseName = a} :: GetColumnStatisticsForPartition)
{-# DEPRECATED gcsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of the column names.
--
-- /Note:/ Consider using 'columnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpColumnNames :: Lens.Lens' GetColumnStatisticsForPartition [Lude.Text]
gcsfpColumnNames = Lens.lens (columnNames :: GetColumnStatisticsForPartition -> [Lude.Text]) (\s a -> s {columnNames = a} :: GetColumnStatisticsForPartition)
{-# DEPRECATED gcsfpColumnNames "Use generic-lens or generic-optics with 'columnNames' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpPartitionValues :: Lens.Lens' GetColumnStatisticsForPartition [Lude.Text]
gcsfpPartitionValues = Lens.lens (partitionValues :: GetColumnStatisticsForPartition -> [Lude.Text]) (\s a -> s {partitionValues = a} :: GetColumnStatisticsForPartition)
{-# DEPRECATED gcsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpTableName :: Lens.Lens' GetColumnStatisticsForPartition Lude.Text
gcsfpTableName = Lens.lens (tableName :: GetColumnStatisticsForPartition -> Lude.Text) (\s a -> s {tableName = a} :: GetColumnStatisticsForPartition)
{-# DEPRECATED gcsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest GetColumnStatisticsForPartition where
  type
    Rs GetColumnStatisticsForPartition =
      GetColumnStatisticsForPartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForPartitionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ColumnStatisticsList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetColumnStatisticsForPartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetColumnStatisticsForPartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetColumnStatisticsForPartition where
  toJSON GetColumnStatisticsForPartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("ColumnNames" Lude..= columnNames),
            Lude.Just ("PartitionValues" Lude..= partitionValues),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetColumnStatisticsForPartition where
  toPath = Lude.const "/"

instance Lude.ToQuery GetColumnStatisticsForPartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetColumnStatisticsForPartitionResponse' smart constructor.
data GetColumnStatisticsForPartitionResponse = GetColumnStatisticsForPartitionResponse'
  { -- | Error occurred during retrieving column statistics data.
    errors :: Lude.Maybe [ColumnError],
    -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Lude.Maybe [ColumnStatistics],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - Error occurred during retrieving column statistics data.
-- * 'columnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
-- * 'responseStatus' - The response status code.
mkGetColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetColumnStatisticsForPartitionResponse
mkGetColumnStatisticsForPartitionResponse pResponseStatus_ =
  GetColumnStatisticsForPartitionResponse'
    { errors = Lude.Nothing,
      columnStatisticsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Error occurred during retrieving column statistics data.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprsErrors :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Lude.Maybe [ColumnError])
gcsfprsErrors = Lens.lens (errors :: GetColumnStatisticsForPartitionResponse -> Lude.Maybe [ColumnError]) (\s a -> s {errors = a} :: GetColumnStatisticsForPartitionResponse)
{-# DEPRECATED gcsfprsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprsColumnStatisticsList :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Lude.Maybe [ColumnStatistics])
gcsfprsColumnStatisticsList = Lens.lens (columnStatisticsList :: GetColumnStatisticsForPartitionResponse -> Lude.Maybe [ColumnStatistics]) (\s a -> s {columnStatisticsList = a} :: GetColumnStatisticsForPartitionResponse)
{-# DEPRECATED gcsfprsColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprsResponseStatus :: Lens.Lens' GetColumnStatisticsForPartitionResponse Lude.Int
gcsfprsResponseStatus = Lens.lens (responseStatus :: GetColumnStatisticsForPartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetColumnStatisticsForPartitionResponse)
{-# DEPRECATED gcsfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
