{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchCreatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchCreatePartition
  ( -- * Creating a request
    BatchCreatePartition (..),
    mkBatchCreatePartition,

    -- ** Request lenses
    bcpCatalogId,
    bcpDatabaseName,
    bcpTableName,
    bcpPartitionInputList,

    -- * Destructuring the response
    BatchCreatePartitionResponse (..),
    mkBatchCreatePartitionResponse,

    -- ** Response lenses
    bcprsErrors,
    bcprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchCreatePartition' smart constructor.
data BatchCreatePartition = BatchCreatePartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionInputList :: [PartitionInput]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCreatePartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the catalog in which the partition is to be created. Currently, this should be the AWS account ID.
-- * 'databaseName' - The name of the metadata database in which the partition is to be created.
-- * 'partitionInputList' - A list of @PartitionInput@ structures that define the partitions to be created.
-- * 'tableName' - The name of the metadata table in which the partition is to be created.
mkBatchCreatePartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  BatchCreatePartition
mkBatchCreatePartition pDatabaseName_ pTableName_ =
  BatchCreatePartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionInputList = Lude.mempty
    }

-- | The ID of the catalog in which the partition is to be created. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpCatalogId :: Lens.Lens' BatchCreatePartition (Lude.Maybe Lude.Text)
bcpCatalogId = Lens.lens (catalogId :: BatchCreatePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchCreatePartition)
{-# DEPRECATED bcpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the metadata database in which the partition is to be created.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpDatabaseName :: Lens.Lens' BatchCreatePartition Lude.Text
bcpDatabaseName = Lens.lens (databaseName :: BatchCreatePartition -> Lude.Text) (\s a -> s {databaseName = a} :: BatchCreatePartition)
{-# DEPRECATED bcpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpTableName :: Lens.Lens' BatchCreatePartition Lude.Text
bcpTableName = Lens.lens (tableName :: BatchCreatePartition -> Lude.Text) (\s a -> s {tableName = a} :: BatchCreatePartition)
{-# DEPRECATED bcpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of @PartitionInput@ structures that define the partitions to be created.
--
-- /Note:/ Consider using 'partitionInputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpPartitionInputList :: Lens.Lens' BatchCreatePartition [PartitionInput]
bcpPartitionInputList = Lens.lens (partitionInputList :: BatchCreatePartition -> [PartitionInput]) (\s a -> s {partitionInputList = a} :: BatchCreatePartition)
{-# DEPRECATED bcpPartitionInputList "Use generic-lens or generic-optics with 'partitionInputList' instead." #-}

instance Lude.AWSRequest BatchCreatePartition where
  type Rs BatchCreatePartition = BatchCreatePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchCreatePartitionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchCreatePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchCreatePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchCreatePartition where
  toJSON BatchCreatePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionInputList" Lude..= partitionInputList)
          ]
      )

instance Lude.ToPath BatchCreatePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchCreatePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchCreatePartitionResponse' smart constructor.
data BatchCreatePartitionResponse = BatchCreatePartitionResponse'
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

-- | Creates a value of 'BatchCreatePartitionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - The errors encountered when trying to create the requested partitions.
-- * 'responseStatus' - The response status code.
mkBatchCreatePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchCreatePartitionResponse
mkBatchCreatePartitionResponse pResponseStatus_ =
  BatchCreatePartitionResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The errors encountered when trying to create the requested partitions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcprsErrors :: Lens.Lens' BatchCreatePartitionResponse (Lude.Maybe [PartitionError])
bcprsErrors = Lens.lens (errors :: BatchCreatePartitionResponse -> Lude.Maybe [PartitionError]) (\s a -> s {errors = a} :: BatchCreatePartitionResponse)
{-# DEPRECATED bcprsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcprsResponseStatus :: Lens.Lens' BatchCreatePartitionResponse Lude.Int
bcprsResponseStatus = Lens.lens (responseStatus :: BatchCreatePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchCreatePartitionResponse)
{-# DEPRECATED bcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
