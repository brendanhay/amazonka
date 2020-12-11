{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeletePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more partitions in a batch operation.
module Network.AWS.Glue.BatchDeletePartition
  ( -- * Creating a request
    BatchDeletePartition (..),
    mkBatchDeletePartition,

    -- ** Request lenses
    bdpCatalogId,
    bdpDatabaseName,
    bdpTableName,
    bdpPartitionsToDelete,

    -- * Destructuring the response
    BatchDeletePartitionResponse (..),
    mkBatchDeletePartitionResponse,

    -- ** Response lenses
    bdprsErrors,
    bdprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeletePartition' smart constructor.
data BatchDeletePartition = BatchDeletePartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionsToDelete :: [PartitionValueList]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeletePartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the table in question resides.
-- * 'partitionsToDelete' - A list of @PartitionInput@ structures that define the partitions to be deleted.
-- * 'tableName' - The name of the table that contains the partitions to be deleted.
mkBatchDeletePartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  BatchDeletePartition
mkBatchDeletePartition pDatabaseName_ pTableName_ =
  BatchDeletePartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToDelete = Lude.mempty
    }

-- | The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpCatalogId :: Lens.Lens' BatchDeletePartition (Lude.Maybe Lude.Text)
bdpCatalogId = Lens.lens (catalogId :: BatchDeletePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchDeletePartition)
{-# DEPRECATED bdpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpDatabaseName :: Lens.Lens' BatchDeletePartition Lude.Text
bdpDatabaseName = Lens.lens (databaseName :: BatchDeletePartition -> Lude.Text) (\s a -> s {databaseName = a} :: BatchDeletePartition)
{-# DEPRECATED bdpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table that contains the partitions to be deleted.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpTableName :: Lens.Lens' BatchDeletePartition Lude.Text
bdpTableName = Lens.lens (tableName :: BatchDeletePartition -> Lude.Text) (\s a -> s {tableName = a} :: BatchDeletePartition)
{-# DEPRECATED bdpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of @PartitionInput@ structures that define the partitions to be deleted.
--
-- /Note:/ Consider using 'partitionsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpPartitionsToDelete :: Lens.Lens' BatchDeletePartition [PartitionValueList]
bdpPartitionsToDelete = Lens.lens (partitionsToDelete :: BatchDeletePartition -> [PartitionValueList]) (\s a -> s {partitionsToDelete = a} :: BatchDeletePartition)
{-# DEPRECATED bdpPartitionsToDelete "Use generic-lens or generic-optics with 'partitionsToDelete' instead." #-}

instance Lude.AWSRequest BatchDeletePartition where
  type Rs BatchDeletePartition = BatchDeletePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeletePartitionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeletePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchDeletePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeletePartition where
  toJSON BatchDeletePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionsToDelete" Lude..= partitionsToDelete)
          ]
      )

instance Lude.ToPath BatchDeletePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeletePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeletePartitionResponse' smart constructor.
data BatchDeletePartitionResponse = BatchDeletePartitionResponse'
  { errors ::
      Lude.Maybe [PartitionError],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeletePartitionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - The errors encountered when trying to delete the requested partitions.
-- * 'responseStatus' - The response status code.
mkBatchDeletePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeletePartitionResponse
mkBatchDeletePartitionResponse pResponseStatus_ =
  BatchDeletePartitionResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The errors encountered when trying to delete the requested partitions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdprsErrors :: Lens.Lens' BatchDeletePartitionResponse (Lude.Maybe [PartitionError])
bdprsErrors = Lens.lens (errors :: BatchDeletePartitionResponse -> Lude.Maybe [PartitionError]) (\s a -> s {errors = a} :: BatchDeletePartitionResponse)
{-# DEPRECATED bdprsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdprsResponseStatus :: Lens.Lens' BatchDeletePartitionResponse Lude.Int
bdprsResponseStatus = Lens.lens (responseStatus :: BatchDeletePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeletePartitionResponse)
{-# DEPRECATED bdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
