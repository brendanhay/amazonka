{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified partition.
module Network.AWS.Glue.GetPartition
  ( -- * Creating a request
    GetPartition (..),
    mkGetPartition,

    -- ** Request lenses
    gpCatalogId,
    gpDatabaseName,
    gpTableName,
    gpPartitionValues,

    -- * Destructuring the response
    GetPartitionResponse (..),
    mkGetPartitionResponse,

    -- ** Response lenses
    gprsPartition,
    gprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPartition' smart constructor.
data GetPartition = GetPartition'
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

-- | Creates a value of 'GetPartition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the partition in question resides. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the partition resides.
-- * 'partitionValues' - The values that define the partition.
-- * 'tableName' - The name of the partition's table.
mkGetPartition ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetPartition
mkGetPartition pDatabaseName_ pTableName_ =
  GetPartition'
    { catalogId = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionValues = Lude.mempty
    }

-- | The ID of the Data Catalog where the partition in question resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpCatalogId :: Lens.Lens' GetPartition (Lude.Maybe Lude.Text)
gpCatalogId = Lens.lens (catalogId :: GetPartition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetPartition)
{-# DEPRECATED gpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the partition resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpDatabaseName :: Lens.Lens' GetPartition Lude.Text
gpDatabaseName = Lens.lens (databaseName :: GetPartition -> Lude.Text) (\s a -> s {databaseName = a} :: GetPartition)
{-# DEPRECATED gpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partition's table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpTableName :: Lens.Lens' GetPartition Lude.Text
gpTableName = Lens.lens (tableName :: GetPartition -> Lude.Text) (\s a -> s {tableName = a} :: GetPartition)
{-# DEPRECATED gpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPartitionValues :: Lens.Lens' GetPartition [Lude.Text]
gpPartitionValues = Lens.lens (partitionValues :: GetPartition -> [Lude.Text]) (\s a -> s {partitionValues = a} :: GetPartition)
{-# DEPRECATED gpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

instance Lude.AWSRequest GetPartition where
  type Rs GetPartition = GetPartitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPartitionResponse'
            Lude.<$> (x Lude..?> "Partition") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPartition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetPartition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPartition where
  toJSON GetPartition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("PartitionValues" Lude..= partitionValues)
          ]
      )

instance Lude.ToPath GetPartition where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPartition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPartitionResponse' smart constructor.
data GetPartitionResponse = GetPartitionResponse'
  { partition ::
      Lude.Maybe Partition,
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

-- | Creates a value of 'GetPartitionResponse' with the minimum fields required to make a request.
--
-- * 'partition' - The requested information, in the form of a @Partition@ object.
-- * 'responseStatus' - The response status code.
mkGetPartitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPartitionResponse
mkGetPartitionResponse pResponseStatus_ =
  GetPartitionResponse'
    { partition = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested information, in the form of a @Partition@ object.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPartition :: Lens.Lens' GetPartitionResponse (Lude.Maybe Partition)
gprsPartition = Lens.lens (partition :: GetPartitionResponse -> Lude.Maybe Partition) (\s a -> s {partition = a} :: GetPartitionResponse)
{-# DEPRECATED gprsPartition "Use generic-lens or generic-optics with 'partition' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPartitionResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPartitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPartitionResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
