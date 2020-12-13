{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartitionIndexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the partition indexes associated with a table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitionIndexes
  ( -- * Creating a request
    GetPartitionIndexes (..),
    mkGetPartitionIndexes,

    -- ** Request lenses
    gpiCatalogId,
    gpiNextToken,
    gpiDatabaseName,
    gpiTableName,

    -- * Destructuring the response
    GetPartitionIndexesResponse (..),
    mkGetPartitionIndexesResponse,

    -- ** Response lenses
    gpirsPartitionIndexDescriptorList,
    gpirsNextToken,
    gpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPartitionIndexes' smart constructor.
data GetPartitionIndexes = GetPartitionIndexes'
  { -- | The catalog ID where the table resides.
    catalogId :: Lude.Maybe Lude.Text,
    -- | A continuation token, included if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies the name of a database from which you want to retrieve partition indexes.
    databaseName :: Lude.Text,
    -- | Specifies the name of a table for which you want to retrieve the partition indexes.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPartitionIndexes' with the minimum fields required to make a request.
--
-- * 'catalogId' - The catalog ID where the table resides.
-- * 'nextToken' - A continuation token, included if this is a continuation call.
-- * 'databaseName' - Specifies the name of a database from which you want to retrieve partition indexes.
-- * 'tableName' - Specifies the name of a table for which you want to retrieve the partition indexes.
mkGetPartitionIndexes ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GetPartitionIndexes
mkGetPartitionIndexes pDatabaseName_ pTableName_ =
  GetPartitionIndexes'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiCatalogId :: Lens.Lens' GetPartitionIndexes (Lude.Maybe Lude.Text)
gpiCatalogId = Lens.lens (catalogId :: GetPartitionIndexes -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetPartitionIndexes)
{-# DEPRECATED gpiCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiNextToken :: Lens.Lens' GetPartitionIndexes (Lude.Maybe Lude.Text)
gpiNextToken = Lens.lens (nextToken :: GetPartitionIndexes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPartitionIndexes)
{-# DEPRECATED gpiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the name of a database from which you want to retrieve partition indexes.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiDatabaseName :: Lens.Lens' GetPartitionIndexes Lude.Text
gpiDatabaseName = Lens.lens (databaseName :: GetPartitionIndexes -> Lude.Text) (\s a -> s {databaseName = a} :: GetPartitionIndexes)
{-# DEPRECATED gpiDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies the name of a table for which you want to retrieve the partition indexes.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiTableName :: Lens.Lens' GetPartitionIndexes Lude.Text
gpiTableName = Lens.lens (tableName :: GetPartitionIndexes -> Lude.Text) (\s a -> s {tableName = a} :: GetPartitionIndexes)
{-# DEPRECATED gpiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Page.AWSPager GetPartitionIndexes where
  page rq rs
    | Page.stop (rs Lens.^. gpirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gpirsPartitionIndexDescriptorList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gpiNextToken Lens..~ rs Lens.^. gpirsNextToken

instance Lude.AWSRequest GetPartitionIndexes where
  type Rs GetPartitionIndexes = GetPartitionIndexesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPartitionIndexesResponse'
            Lude.<$> (x Lude..?> "PartitionIndexDescriptorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPartitionIndexes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetPartitionIndexes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPartitionIndexes where
  toJSON GetPartitionIndexes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetPartitionIndexes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPartitionIndexes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPartitionIndexesResponse' smart constructor.
data GetPartitionIndexesResponse = GetPartitionIndexesResponse'
  { -- | A list of index descriptors.
    partitionIndexDescriptorList :: Lude.Maybe [PartitionIndexDescriptor],
    -- | A continuation token, present if the current list segment is not the last.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPartitionIndexesResponse' with the minimum fields required to make a request.
--
-- * 'partitionIndexDescriptorList' - A list of index descriptors.
-- * 'nextToken' - A continuation token, present if the current list segment is not the last.
-- * 'responseStatus' - The response status code.
mkGetPartitionIndexesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPartitionIndexesResponse
mkGetPartitionIndexesResponse pResponseStatus_ =
  GetPartitionIndexesResponse'
    { partitionIndexDescriptorList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of index descriptors.
--
-- /Note:/ Consider using 'partitionIndexDescriptorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirsPartitionIndexDescriptorList :: Lens.Lens' GetPartitionIndexesResponse (Lude.Maybe [PartitionIndexDescriptor])
gpirsPartitionIndexDescriptorList = Lens.lens (partitionIndexDescriptorList :: GetPartitionIndexesResponse -> Lude.Maybe [PartitionIndexDescriptor]) (\s a -> s {partitionIndexDescriptorList = a} :: GetPartitionIndexesResponse)
{-# DEPRECATED gpirsPartitionIndexDescriptorList "Use generic-lens or generic-optics with 'partitionIndexDescriptorList' instead." #-}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirsNextToken :: Lens.Lens' GetPartitionIndexesResponse (Lude.Maybe Lude.Text)
gpirsNextToken = Lens.lens (nextToken :: GetPartitionIndexesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPartitionIndexesResponse)
{-# DEPRECATED gpirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirsResponseStatus :: Lens.Lens' GetPartitionIndexesResponse Lude.Int
gpirsResponseStatus = Lens.lens (responseStatus :: GetPartitionIndexesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPartitionIndexesResponse)
{-# DEPRECATED gpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
