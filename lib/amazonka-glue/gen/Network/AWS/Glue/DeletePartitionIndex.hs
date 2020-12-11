{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeletePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition index from an existing table.
module Network.AWS.Glue.DeletePartitionIndex
  ( -- * Creating a request
    DeletePartitionIndex (..),
    mkDeletePartitionIndex,

    -- ** Request lenses
    dpiCatalogId,
    dpiDatabaseName,
    dpiTableName,
    dpiIndexName,

    -- * Destructuring the response
    DeletePartitionIndexResponse (..),
    mkDeletePartitionIndexResponse,

    -- ** Response lenses
    dpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePartitionIndex' smart constructor.
data DeletePartitionIndex = DeletePartitionIndex'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    indexName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePartitionIndex' with the minimum fields required to make a request.
--
-- * 'catalogId' - The catalog ID where the table resides.
-- * 'databaseName' - Specifies the name of a database from which you want to delete a partition index.
-- * 'indexName' - The name of the partition index to be deleted.
-- * 'tableName' - Specifies the name of a table from which you want to delete a partition index.
mkDeletePartitionIndex ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'indexName'
  Lude.Text ->
  DeletePartitionIndex
mkDeletePartitionIndex pDatabaseName_ pTableName_ pIndexName_ =
  DeletePartitionIndex'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      indexName = pIndexName_
    }

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiCatalogId :: Lens.Lens' DeletePartitionIndex (Lude.Maybe Lude.Text)
dpiCatalogId = Lens.lens (catalogId :: DeletePartitionIndex -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DeletePartitionIndex)
{-# DEPRECATED dpiCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | Specifies the name of a database from which you want to delete a partition index.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiDatabaseName :: Lens.Lens' DeletePartitionIndex Lude.Text
dpiDatabaseName = Lens.lens (databaseName :: DeletePartitionIndex -> Lude.Text) (\s a -> s {databaseName = a} :: DeletePartitionIndex)
{-# DEPRECATED dpiDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies the name of a table from which you want to delete a partition index.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiTableName :: Lens.Lens' DeletePartitionIndex Lude.Text
dpiTableName = Lens.lens (tableName :: DeletePartitionIndex -> Lude.Text) (\s a -> s {tableName = a} :: DeletePartitionIndex)
{-# DEPRECATED dpiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the partition index to be deleted.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiIndexName :: Lens.Lens' DeletePartitionIndex Lude.Text
dpiIndexName = Lens.lens (indexName :: DeletePartitionIndex -> Lude.Text) (\s a -> s {indexName = a} :: DeletePartitionIndex)
{-# DEPRECATED dpiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest DeletePartitionIndex where
  type Rs DeletePartitionIndex = DeletePartitionIndexResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePartitionIndexResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePartitionIndex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeletePartitionIndex" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePartitionIndex where
  toJSON DeletePartitionIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )

instance Lude.ToPath DeletePartitionIndex where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePartitionIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePartitionIndexResponse' smart constructor.
newtype DeletePartitionIndexResponse = DeletePartitionIndexResponse'
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

-- | Creates a value of 'DeletePartitionIndexResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePartitionIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePartitionIndexResponse
mkDeletePartitionIndexResponse pResponseStatus_ =
  DeletePartitionIndexResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpirsResponseStatus :: Lens.Lens' DeletePartitionIndexResponse Lude.Int
dpirsResponseStatus = Lens.lens (responseStatus :: DeletePartitionIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePartitionIndexResponse)
{-# DEPRECATED dpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
