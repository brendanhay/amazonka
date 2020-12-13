{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- The Identity and Access Management (IAM) permission required for this operation is @UpdatePartition@ .
module Network.AWS.Glue.UpdateColumnStatisticsForPartition
  ( -- * Creating a request
    UpdateColumnStatisticsForPartition (..),
    mkUpdateColumnStatisticsForPartition,

    -- ** Request lenses
    ucsfpCatalogId,
    ucsfpDatabaseName,
    ucsfpPartitionValues,
    ucsfpTableName,
    ucsfpColumnStatisticsList,

    -- * Destructuring the response
    UpdateColumnStatisticsForPartitionResponse (..),
    mkUpdateColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    ucsfprsErrors,
    ucsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateColumnStatisticsForPartition' smart constructor.
data UpdateColumnStatisticsForPartition = UpdateColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Lude.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Lude.Text],
    -- | The name of the partitions' table.
    tableName :: Lude.Text,
    -- | A list of the column statistics.
    columnStatisticsList :: [ColumnStatistics]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'partitionValues' - A list of partition values identifying the partition.
-- * 'tableName' - The name of the partitions' table.
-- * 'columnStatisticsList' - A list of the column statistics.
mkUpdateColumnStatisticsForPartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  UpdateColumnStatisticsForPartition
mkUpdateColumnStatisticsForPartition pDatabaseName_ pTableName_ =
  UpdateColumnStatisticsForPartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      partitionValues = Lude.mempty,
      tableName = pTableName_,
      columnStatisticsList = Lude.mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpCatalogId :: Lens.Lens' UpdateColumnStatisticsForPartition (Lude.Maybe Lude.Text)
ucsfpCatalogId = Lens.lens (catalogId :: UpdateColumnStatisticsForPartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateColumnStatisticsForPartition)
{-# DEPRECATED ucsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpDatabaseName :: Lens.Lens' UpdateColumnStatisticsForPartition Lude.Text
ucsfpDatabaseName = Lens.lens (databaseName :: UpdateColumnStatisticsForPartition -> Lude.Text) (\s a -> s {databaseName = a} :: UpdateColumnStatisticsForPartition)
{-# DEPRECATED ucsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpPartitionValues :: Lens.Lens' UpdateColumnStatisticsForPartition [Lude.Text]
ucsfpPartitionValues = Lens.lens (partitionValues :: UpdateColumnStatisticsForPartition -> [Lude.Text]) (\s a -> s {partitionValues = a} :: UpdateColumnStatisticsForPartition)
{-# DEPRECATED ucsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpTableName :: Lens.Lens' UpdateColumnStatisticsForPartition Lude.Text
ucsfpTableName = Lens.lens (tableName :: UpdateColumnStatisticsForPartition -> Lude.Text) (\s a -> s {tableName = a} :: UpdateColumnStatisticsForPartition)
{-# DEPRECATED ucsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of the column statistics.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpColumnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForPartition [ColumnStatistics]
ucsfpColumnStatisticsList = Lens.lens (columnStatisticsList :: UpdateColumnStatisticsForPartition -> [ColumnStatistics]) (\s a -> s {columnStatisticsList = a} :: UpdateColumnStatisticsForPartition)
{-# DEPRECATED ucsfpColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

instance Lude.AWSRequest UpdateColumnStatisticsForPartition where
  type
    Rs UpdateColumnStatisticsForPartition =
      UpdateColumnStatisticsForPartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForPartitionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateColumnStatisticsForPartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateColumnStatisticsForPartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateColumnStatisticsForPartition where
  toJSON UpdateColumnStatisticsForPartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("PartitionValues" Lude..= partitionValues),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("ColumnStatisticsList" Lude..= columnStatisticsList)
          ]
      )

instance Lude.ToPath UpdateColumnStatisticsForPartition where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateColumnStatisticsForPartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateColumnStatisticsForPartitionResponse' smart constructor.
data UpdateColumnStatisticsForPartitionResponse = UpdateColumnStatisticsForPartitionResponse'
  { -- | Error occurred during updating column statistics data.
    errors :: Lude.Maybe [ColumnStatisticsError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - Error occurred during updating column statistics data.
-- * 'responseStatus' - The response status code.
mkUpdateColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateColumnStatisticsForPartitionResponse
mkUpdateColumnStatisticsForPartitionResponse pResponseStatus_ =
  UpdateColumnStatisticsForPartitionResponse'
    { errors =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Error occurred during updating column statistics data.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfprsErrors :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse (Lude.Maybe [ColumnStatisticsError])
ucsfprsErrors = Lens.lens (errors :: UpdateColumnStatisticsForPartitionResponse -> Lude.Maybe [ColumnStatisticsError]) (\s a -> s {errors = a} :: UpdateColumnStatisticsForPartitionResponse)
{-# DEPRECATED ucsfprsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfprsResponseStatus :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse Lude.Int
ucsfprsResponseStatus = Lens.lens (responseStatus :: UpdateColumnStatisticsForPartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateColumnStatisticsForPartitionResponse)
{-# DEPRECATED ucsfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
