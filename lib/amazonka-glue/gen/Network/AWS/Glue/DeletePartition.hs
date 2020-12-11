{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeletePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition.
module Network.AWS.Glue.DeletePartition
  ( -- * Creating a request
    DeletePartition (..),
    mkDeletePartition,

    -- ** Request lenses
    dpCatalogId,
    dpDatabaseName,
    dpTableName,
    dpPartitionValues,

    -- * Destructuring the response
    DeletePartitionResponse (..),
    mkDeletePartitionResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePartition' smart constructor.
data DeletePartition = DeletePartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionValues :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the table in question resides.
-- * 'partitionValues' - The values that define the partition.
-- * 'tableName' - The name of the table that contains the partition to be deleted.
mkDeletePartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  DeletePartition
mkDeletePartition pDatabaseName_ pTableName_ =
  DeletePartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionValues = Lude.mempty
    }

-- | The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCatalogId :: Lens.Lens' DeletePartition (Lude.Maybe Lude.Text)
dpCatalogId = Lens.lens (catalogId :: DeletePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeletePartition)
{-# DEPRECATED dpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDatabaseName :: Lens.Lens' DeletePartition Lude.Text
dpDatabaseName = Lens.lens (databaseName :: DeletePartition -> Lude.Text) (\s a -> s {databaseName = a} :: DeletePartition)
{-# DEPRECATED dpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table that contains the partition to be deleted.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpTableName :: Lens.Lens' DeletePartition Lude.Text
dpTableName = Lens.lens (tableName :: DeletePartition -> Lude.Text) (\s a -> s {tableName = a} :: DeletePartition)
{-# DEPRECATED dpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPartitionValues :: Lens.Lens' DeletePartition [Lude.Text]
dpPartitionValues = Lens.lens (partitionValues :: DeletePartition -> [Lude.Text]) (\s a -> s {partitionValues = a} :: DeletePartition)
{-# DEPRECATED dpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

instance Lude.AWSRequest DeletePartition where
  type Rs DeletePartition = DeletePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePartitionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeletePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePartition where
  toJSON DeletePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionValues" Lude..= partitionValues)
          ]
      )

instance Lude.ToPath DeletePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePartitionResponse' smart constructor.
newtype DeletePartitionResponse = DeletePartitionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePartitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePartitionResponse
mkDeletePartitionResponse pResponseStatus_ =
  DeletePartitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeletePartitionResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeletePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePartitionResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
