{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partitions in a batch request.
module Network.AWS.Glue.BatchGetPartition
  ( -- * Creating a request
    BatchGetPartition (..),
    mkBatchGetPartition,

    -- ** Request lenses
    bgpCatalogId,
    bgpDatabaseName,
    bgpTableName,
    bgpPartitionsToGet,

    -- * Destructuring the response
    BatchGetPartitionResponse (..),
    mkBatchGetPartitionResponse,

    -- ** Response lenses
    bgprsUnprocessedKeys,
    bgprsPartitions,
    bgprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetPartition' smart constructor.
data BatchGetPartition = BatchGetPartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionsToGet :: [PartitionValueList]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetPartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partitions reside.
-- * 'partitionsToGet' - A list of partition values identifying the partitions to retrieve.
-- * 'tableName' - The name of the partitions' table.
mkBatchGetPartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  BatchGetPartition
mkBatchGetPartition pDatabaseName_ pTableName_ =
  BatchGetPartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToGet = Lude.mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpCatalogId :: Lens.Lens' BatchGetPartition (Lude.Maybe Lude.Text)
bgpCatalogId = Lens.lens (catalogId :: BatchGetPartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchGetPartition)
{-# DEPRECATED bgpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpDatabaseName :: Lens.Lens' BatchGetPartition Lude.Text
bgpDatabaseName = Lens.lens (databaseName :: BatchGetPartition -> Lude.Text) (\s a -> s {databaseName = a} :: BatchGetPartition)
{-# DEPRECATED bgpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpTableName :: Lens.Lens' BatchGetPartition Lude.Text
bgpTableName = Lens.lens (tableName :: BatchGetPartition -> Lude.Text) (\s a -> s {tableName = a} :: BatchGetPartition)
{-# DEPRECATED bgpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of partition values identifying the partitions to retrieve.
--
-- /Note:/ Consider using 'partitionsToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpPartitionsToGet :: Lens.Lens' BatchGetPartition [PartitionValueList]
bgpPartitionsToGet = Lens.lens (partitionsToGet :: BatchGetPartition -> [PartitionValueList]) (\s a -> s {partitionsToGet = a} :: BatchGetPartition)
{-# DEPRECATED bgpPartitionsToGet "Use generic-lens or generic-optics with 'partitionsToGet' instead." #-}

instance Lude.AWSRequest BatchGetPartition where
  type Rs BatchGetPartition = BatchGetPartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetPartitionResponse'
            Lude.<$> (x Lude..?> "UnprocessedKeys" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Partitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetPartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetPartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetPartition where
  toJSON BatchGetPartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionsToGet" Lude..= partitionsToGet)
          ]
      )

instance Lude.ToPath BatchGetPartition where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetPartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetPartitionResponse' smart constructor.
data BatchGetPartitionResponse = BatchGetPartitionResponse'
  { unprocessedKeys ::
      Lude.Maybe [PartitionValueList],
    partitions :: Lude.Maybe [Partition],
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

-- | Creates a value of 'BatchGetPartitionResponse' with the minimum fields required to make a request.
--
-- * 'partitions' - A list of the requested partitions.
-- * 'responseStatus' - The response status code.
-- * 'unprocessedKeys' - A list of the partition values in the request for which partitions were not returned.
mkBatchGetPartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetPartitionResponse
mkBatchGetPartitionResponse pResponseStatus_ =
  BatchGetPartitionResponse'
    { unprocessedKeys = Lude.Nothing,
      partitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the partition values in the request for which partitions were not returned.
--
-- /Note:/ Consider using 'unprocessedKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsUnprocessedKeys :: Lens.Lens' BatchGetPartitionResponse (Lude.Maybe [PartitionValueList])
bgprsUnprocessedKeys = Lens.lens (unprocessedKeys :: BatchGetPartitionResponse -> Lude.Maybe [PartitionValueList]) (\s a -> s {unprocessedKeys = a} :: BatchGetPartitionResponse)
{-# DEPRECATED bgprsUnprocessedKeys "Use generic-lens or generic-optics with 'unprocessedKeys' instead." #-}

-- | A list of the requested partitions.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsPartitions :: Lens.Lens' BatchGetPartitionResponse (Lude.Maybe [Partition])
bgprsPartitions = Lens.lens (partitions :: BatchGetPartitionResponse -> Lude.Maybe [Partition]) (\s a -> s {partitions = a} :: BatchGetPartitionResponse)
{-# DEPRECATED bgprsPartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsResponseStatus :: Lens.Lens' BatchGetPartitionResponse Lude.Int
bgprsResponseStatus = Lens.lens (responseStatus :: BatchGetPartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetPartitionResponse)
{-# DEPRECATED bgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
