{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
module Network.AWS.Glue.CreatePartition
  ( -- * Creating a request
    CreatePartition (..),
    mkCreatePartition,

    -- ** Request lenses
    cpCatalogId,
    cpDatabaseName,
    cpTableName,
    cpPartitionInput,

    -- * Destructuring the response
    CreatePartitionResponse (..),
    mkCreatePartitionResponse,

    -- ** Response lenses
    cprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePartition' smart constructor.
data CreatePartition = CreatePartition'
  { catalogId ::
      Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    partitionInput :: PartitionInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The AWS account ID of the catalog in which the partition is to be created.
-- * 'databaseName' - The name of the metadata database in which the partition is to be created.
-- * 'partitionInput' - A @PartitionInput@ structure defining the partition to be created.
-- * 'tableName' - The name of the metadata table in which the partition is to be created.
mkCreatePartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'partitionInput'
  PartitionInput ->
  CreatePartition
mkCreatePartition pDatabaseName_ pTableName_ pPartitionInput_ =
  CreatePartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionInput = pPartitionInput_
    }

-- | The AWS account ID of the catalog in which the partition is to be created.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCatalogId :: Lens.Lens' CreatePartition (Lude.Maybe Lude.Text)
cpCatalogId = Lens.lens (catalogId :: CreatePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreatePartition)
{-# DEPRECATED cpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the metadata database in which the partition is to be created.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDatabaseName :: Lens.Lens' CreatePartition Lude.Text
cpDatabaseName = Lens.lens (databaseName :: CreatePartition -> Lude.Text) (\s a -> s {databaseName = a} :: CreatePartition)
{-# DEPRECATED cpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTableName :: Lens.Lens' CreatePartition Lude.Text
cpTableName = Lens.lens (tableName :: CreatePartition -> Lude.Text) (\s a -> s {tableName = a} :: CreatePartition)
{-# DEPRECATED cpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A @PartitionInput@ structure defining the partition to be created.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPartitionInput :: Lens.Lens' CreatePartition PartitionInput
cpPartitionInput = Lens.lens (partitionInput :: CreatePartition -> PartitionInput) (\s a -> s {partitionInput = a} :: CreatePartition)
{-# DEPRECATED cpPartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

instance Lude.AWSRequest CreatePartition where
  type Rs CreatePartition = CreatePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreatePartitionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreatePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePartition where
  toJSON CreatePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionInput" Lude..= partitionInput)
          ]
      )

instance Lude.ToPath CreatePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePartitionResponse' smart constructor.
newtype CreatePartitionResponse = CreatePartitionResponse'
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

-- | Creates a value of 'CreatePartitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreatePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePartitionResponse
mkCreatePartitionResponse pResponseStatus_ =
  CreatePartitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePartitionResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePartitionResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
