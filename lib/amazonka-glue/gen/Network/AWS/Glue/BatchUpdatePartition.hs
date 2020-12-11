{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    BatchUpdatePartition (..),
    mkBatchUpdatePartition,

    -- ** Request lenses
    bupCatalogId,
    bupDatabaseName,
    bupTableName,
    bupEntries,

    -- * Destructuring the response
    BatchUpdatePartitionResponse (..),
    mkBatchUpdatePartitionResponse,

    -- ** Response lenses
    buprsErrors,
    buprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchUpdatePartition' smart constructor.
data BatchUpdatePartition = BatchUpdatePartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    entries ::
      Lude.NonEmpty BatchUpdatePartitionRequestEntry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdatePartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
-- * 'databaseName' - The name of the metadata database in which the partition is to be updated.
-- * 'entries' - A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
-- * 'tableName' - The name of the metadata table in which the partition is to be updated.
mkBatchUpdatePartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'entries'
  Lude.NonEmpty BatchUpdatePartitionRequestEntry ->
  BatchUpdatePartition
mkBatchUpdatePartition pDatabaseName_ pTableName_ pEntries_ =
  BatchUpdatePartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      entries = pEntries_
    }

-- | The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupCatalogId :: Lens.Lens' BatchUpdatePartition (Lude.Maybe Lude.Text)
bupCatalogId = Lens.lens (catalogId :: BatchUpdatePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchUpdatePartition)
{-# DEPRECATED bupCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the metadata database in which the partition is to be updated.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupDatabaseName :: Lens.Lens' BatchUpdatePartition Lude.Text
bupDatabaseName = Lens.lens (databaseName :: BatchUpdatePartition -> Lude.Text) (\s a -> s {databaseName = a} :: BatchUpdatePartition)
{-# DEPRECATED bupDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupTableName :: Lens.Lens' BatchUpdatePartition Lude.Text
bupTableName = Lens.lens (tableName :: BatchUpdatePartition -> Lude.Text) (\s a -> s {tableName = a} :: BatchUpdatePartition)
{-# DEPRECATED bupTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupEntries :: Lens.Lens' BatchUpdatePartition (Lude.NonEmpty BatchUpdatePartitionRequestEntry)
bupEntries = Lens.lens (entries :: BatchUpdatePartition -> Lude.NonEmpty BatchUpdatePartitionRequestEntry) (\s a -> s {entries = a} :: BatchUpdatePartition)
{-# DEPRECATED bupEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Lude.AWSRequest BatchUpdatePartition where
  type Rs BatchUpdatePartition = BatchUpdatePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchUpdatePartitionResponse'
            Lude.<$> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchUpdatePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchUpdatePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchUpdatePartition where
  toJSON BatchUpdatePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("Entries" Lude..= entries)
          ]
      )

instance Lude.ToPath BatchUpdatePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchUpdatePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchUpdatePartitionResponse' smart constructor.
data BatchUpdatePartitionResponse = BatchUpdatePartitionResponse'
  { errors ::
      Lude.Maybe
        [BatchUpdatePartitionFailureEntry],
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

-- | Creates a value of 'BatchUpdatePartitionResponse' with the minimum fields required to make a request.
--
-- * 'errors' - The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
-- * 'responseStatus' - The response status code.
mkBatchUpdatePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchUpdatePartitionResponse
mkBatchUpdatePartitionResponse pResponseStatus_ =
  BatchUpdatePartitionResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprsErrors :: Lens.Lens' BatchUpdatePartitionResponse (Lude.Maybe [BatchUpdatePartitionFailureEntry])
buprsErrors = Lens.lens (errors :: BatchUpdatePartitionResponse -> Lude.Maybe [BatchUpdatePartitionFailureEntry]) (\s a -> s {errors = a} :: BatchUpdatePartitionResponse)
{-# DEPRECATED buprsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprsResponseStatus :: Lens.Lens' BatchUpdatePartitionResponse Lude.Int
buprsResponseStatus = Lens.lens (responseStatus :: BatchUpdatePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchUpdatePartitionResponse)
{-# DEPRECATED buprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
