{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a specified partition index in an existing table.
module Network.AWS.Glue.CreatePartitionIndex
  ( -- * Creating a request
    CreatePartitionIndex (..),
    mkCreatePartitionIndex,

    -- ** Request lenses
    cpiCatalogId,
    cpiDatabaseName,
    cpiTableName,
    cpiPartitionIndex,

    -- * Destructuring the response
    CreatePartitionIndexResponse (..),
    mkCreatePartitionIndexResponse,

    -- ** Response lenses
    cpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePartitionIndex' smart constructor.
data CreatePartitionIndex = CreatePartitionIndex'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionIndex :: PartitionIndex
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePartitionIndex' with the minimum fields required to make a request.
--
-- * 'catalogId' - The catalog ID where the table resides.
-- * 'databaseName' - Specifies the name of a database in which you want to create a partition index.
-- * 'partitionIndex' - Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
-- * 'tableName' - Specifies the name of a table in which you want to create a partition index.
mkCreatePartitionIndex ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'partitionIndex'
  PartitionIndex ->
  CreatePartitionIndex
mkCreatePartitionIndex pDatabaseName_ pTableName_ pPartitionIndex_ =
  CreatePartitionIndex'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionIndex = pPartitionIndex_
    }

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiCatalogId :: Lens.Lens' CreatePartitionIndex (Lude.Maybe Lude.Text)
cpiCatalogId = Lens.lens (catalogId :: CreatePartitionIndex -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreatePartitionIndex)
{-# DEPRECATED cpiCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | Specifies the name of a database in which you want to create a partition index.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiDatabaseName :: Lens.Lens' CreatePartitionIndex Lude.Text
cpiDatabaseName = Lens.lens (databaseName :: CreatePartitionIndex -> Lude.Text) (\s a -> s {databaseName = a} :: CreatePartitionIndex)
{-# DEPRECATED cpiDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies the name of a table in which you want to create a partition index.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiTableName :: Lens.Lens' CreatePartitionIndex Lude.Text
cpiTableName = Lens.lens (tableName :: CreatePartitionIndex -> Lude.Text) (\s a -> s {tableName = a} :: CreatePartitionIndex)
{-# DEPRECATED cpiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
--
-- /Note:/ Consider using 'partitionIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiPartitionIndex :: Lens.Lens' CreatePartitionIndex PartitionIndex
cpiPartitionIndex = Lens.lens (partitionIndex :: CreatePartitionIndex -> PartitionIndex) (\s a -> s {partitionIndex = a} :: CreatePartitionIndex)
{-# DEPRECATED cpiPartitionIndex "Use generic-lens or generic-optics with 'partitionIndex' instead." #-}

instance Lude.AWSRequest CreatePartitionIndex where
  type Rs CreatePartitionIndex = CreatePartitionIndexResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreatePartitionIndexResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePartitionIndex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreatePartitionIndex" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePartitionIndex where
  toJSON CreatePartitionIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionIndex" Lude..= partitionIndex)
          ]
      )

instance Lude.ToPath CreatePartitionIndex where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePartitionIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePartitionIndexResponse' smart constructor.
newtype CreatePartitionIndexResponse = CreatePartitionIndexResponse'
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

-- | Creates a value of 'CreatePartitionIndexResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreatePartitionIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePartitionIndexResponse
mkCreatePartitionIndexResponse pResponseStatus_ =
  CreatePartitionIndexResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirsResponseStatus :: Lens.Lens' CreatePartitionIndexResponse Lude.Int
cpirsResponseStatus = Lens.lens (responseStatus :: CreatePartitionIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePartitionIndexResponse)
{-# DEPRECATED cpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
