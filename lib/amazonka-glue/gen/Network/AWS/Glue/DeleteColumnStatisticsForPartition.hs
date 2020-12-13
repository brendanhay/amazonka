{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- The Identity and Access Management (IAM) permission required for this operation is @DeletePartition@ .
module Network.AWS.Glue.DeleteColumnStatisticsForPartition
  ( -- * Creating a request
    DeleteColumnStatisticsForPartition (..),
    mkDeleteColumnStatisticsForPartition,

    -- ** Request lenses
    dcsfpCatalogId,
    dcsfpDatabaseName,
    dcsfpPartitionValues,
    dcsfpTableName,
    dcsfpColumnName,

    -- * Destructuring the response
    DeleteColumnStatisticsForPartitionResponse (..),
    mkDeleteColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    dcsfprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteColumnStatisticsForPartition' smart constructor.
data DeleteColumnStatisticsForPartition = DeleteColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Lude.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Lude.Text],
    -- | The name of the partitions' table.
    tableName :: Lude.Text,
    -- | Name of the column.
    columnName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteColumnStatisticsForPartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'partitionValues' - A list of partition values identifying the partition.
-- * 'tableName' - The name of the partitions' table.
-- * 'columnName' - Name of the column.
mkDeleteColumnStatisticsForPartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'columnName'
  Lude.Text ->
  DeleteColumnStatisticsForPartition
mkDeleteColumnStatisticsForPartition
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForPartition'
      { catalogId = Lude.Nothing,
        databaseName = pDatabaseName_,
        partitionValues = Lude.mempty,
        tableName = pTableName_,
        columnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpCatalogId :: Lens.Lens' DeleteColumnStatisticsForPartition (Lude.Maybe Lude.Text)
dcsfpCatalogId = Lens.lens (catalogId :: DeleteColumnStatisticsForPartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeleteColumnStatisticsForPartition)
{-# DEPRECATED dcsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpDatabaseName :: Lens.Lens' DeleteColumnStatisticsForPartition Lude.Text
dcsfpDatabaseName = Lens.lens (databaseName :: DeleteColumnStatisticsForPartition -> Lude.Text) (\s a -> s {databaseName = a} :: DeleteColumnStatisticsForPartition)
{-# DEPRECATED dcsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpPartitionValues :: Lens.Lens' DeleteColumnStatisticsForPartition [Lude.Text]
dcsfpPartitionValues = Lens.lens (partitionValues :: DeleteColumnStatisticsForPartition -> [Lude.Text]) (\s a -> s {partitionValues = a} :: DeleteColumnStatisticsForPartition)
{-# DEPRECATED dcsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpTableName :: Lens.Lens' DeleteColumnStatisticsForPartition Lude.Text
dcsfpTableName = Lens.lens (tableName :: DeleteColumnStatisticsForPartition -> Lude.Text) (\s a -> s {tableName = a} :: DeleteColumnStatisticsForPartition)
{-# DEPRECATED dcsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Name of the column.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpColumnName :: Lens.Lens' DeleteColumnStatisticsForPartition Lude.Text
dcsfpColumnName = Lens.lens (columnName :: DeleteColumnStatisticsForPartition -> Lude.Text) (\s a -> s {columnName = a} :: DeleteColumnStatisticsForPartition)
{-# DEPRECATED dcsfpColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

instance Lude.AWSRequest DeleteColumnStatisticsForPartition where
  type
    Rs DeleteColumnStatisticsForPartition =
      DeleteColumnStatisticsForPartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForPartitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteColumnStatisticsForPartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteColumnStatisticsForPartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteColumnStatisticsForPartition where
  toJSON DeleteColumnStatisticsForPartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("PartitionValues" Lude..= partitionValues),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("ColumnName" Lude..= columnName)
          ]
      )

instance Lude.ToPath DeleteColumnStatisticsForPartition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteColumnStatisticsForPartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteColumnStatisticsForPartitionResponse' smart constructor.
newtype DeleteColumnStatisticsForPartitionResponse = DeleteColumnStatisticsForPartitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteColumnStatisticsForPartitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteColumnStatisticsForPartitionResponse
mkDeleteColumnStatisticsForPartitionResponse pResponseStatus_ =
  DeleteColumnStatisticsForPartitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfprsResponseStatus :: Lens.Lens' DeleteColumnStatisticsForPartitionResponse Lude.Int
dcsfprsResponseStatus = Lens.lens (responseStatus :: DeleteColumnStatisticsForPartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteColumnStatisticsForPartitionResponse)
{-# DEPRECATED dcsfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
