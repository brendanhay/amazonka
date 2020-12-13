{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a partition.
module Network.AWS.Glue.UpdatePartition
  ( -- * Creating a request
    UpdatePartition (..),
    mkUpdatePartition,

    -- ** Request lenses
    upPartitionInput,
    upCatalogId,
    upDatabaseName,
    upPartitionValueList,
    upTableName,

    -- * Destructuring the response
    UpdatePartitionResponse (..),
    mkUpdatePartitionResponse,

    -- ** Response lenses
    uprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePartition' smart constructor.
data UpdatePartition = UpdatePartition'
  { -- | The new partition object to update the partition to.
    --
    -- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
    partitionInput :: PartitionInput,
    -- | The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database in which the table in question resides.
    databaseName :: Lude.Text,
    -- | List of partition key values that define the partition to update.
    partitionValueList :: [Lude.Text],
    -- | The name of the table in which the partition to be updated is located.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePartition' with the minimum fields required to make a request.
--
-- * 'partitionInput' - The new partition object to update the partition to.
--
-- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
-- * 'catalogId' - The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database in which the table in question resides.
-- * 'partitionValueList' - List of partition key values that define the partition to update.
-- * 'tableName' - The name of the table in which the partition to be updated is located.
mkUpdatePartition ::
  -- | 'partitionInput'
  PartitionInput ->
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  UpdatePartition
mkUpdatePartition pPartitionInput_ pDatabaseName_ pTableName_ =
  UpdatePartition'
    { partitionInput = pPartitionInput_,
      catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      partitionValueList = Lude.mempty,
      tableName = pTableName_
    }

-- | The new partition object to update the partition to.
--
-- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionInput :: Lens.Lens' UpdatePartition PartitionInput
upPartitionInput = Lens.lens (partitionInput :: UpdatePartition -> PartitionInput) (\s a -> s {partitionInput = a} :: UpdatePartition)
{-# DEPRECATED upPartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

-- | The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCatalogId :: Lens.Lens' UpdatePartition (Lude.Maybe Lude.Text)
upCatalogId = Lens.lens (catalogId :: UpdatePartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdatePartition)
{-# DEPRECATED upCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDatabaseName :: Lens.Lens' UpdatePartition Lude.Text
upDatabaseName = Lens.lens (databaseName :: UpdatePartition -> Lude.Text) (\s a -> s {databaseName = a} :: UpdatePartition)
{-# DEPRECATED upDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | List of partition key values that define the partition to update.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionValueList :: Lens.Lens' UpdatePartition [Lude.Text]
upPartitionValueList = Lens.lens (partitionValueList :: UpdatePartition -> [Lude.Text]) (\s a -> s {partitionValueList = a} :: UpdatePartition)
{-# DEPRECATED upPartitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead." #-}

-- | The name of the table in which the partition to be updated is located.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTableName :: Lens.Lens' UpdatePartition Lude.Text
upTableName = Lens.lens (tableName :: UpdatePartition -> Lude.Text) (\s a -> s {tableName = a} :: UpdatePartition)
{-# DEPRECATED upTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest UpdatePartition where
  type Rs UpdatePartition = UpdatePartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdatePartitionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdatePartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePartition where
  toJSON UpdatePartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PartitionInput" Lude..= partitionInput),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("PartitionValueList" Lude..= partitionValueList),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath UpdatePartition where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePartitionResponse' smart constructor.
newtype UpdatePartitionResponse = UpdatePartitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePartitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdatePartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePartitionResponse
mkUpdatePartitionResponse pResponseStatus_ =
  UpdatePartitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePartitionResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePartitionResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
