{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetInventorySchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventorySchema
  ( -- * Creating a request
    GetInventorySchema (..),
    mkGetInventorySchema,

    -- ** Request lenses
    gisTypeName,
    gisAggregator,
    gisNextToken,
    gisSubType,
    gisMaxResults,

    -- * Destructuring the response
    GetInventorySchemaResponse (..),
    mkGetInventorySchemaResponse,

    -- ** Response lenses
    gisrsSchemas,
    gisrsNextToken,
    gisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { typeName ::
      Lude.Maybe Lude.Text,
    aggregator :: Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    subType :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInventorySchema' with the minimum fields required to make a request.
--
-- * 'aggregator' - Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'subType' - Returns the sub-type schema for a specified inventory type.
-- * 'typeName' - The type of inventory item to return.
mkGetInventorySchema ::
  GetInventorySchema
mkGetInventorySchema =
  GetInventorySchema'
    { typeName = Lude.Nothing,
      aggregator = Lude.Nothing,
      nextToken = Lude.Nothing,
      subType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The type of inventory item to return.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisTypeName :: Lens.Lens' GetInventorySchema (Lude.Maybe Lude.Text)
gisTypeName = Lens.lens (typeName :: GetInventorySchema -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: GetInventorySchema)
{-# DEPRECATED gisTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
--
-- /Note:/ Consider using 'aggregator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisAggregator :: Lens.Lens' GetInventorySchema (Lude.Maybe Lude.Bool)
gisAggregator = Lens.lens (aggregator :: GetInventorySchema -> Lude.Maybe Lude.Bool) (\s a -> s {aggregator = a} :: GetInventorySchema)
{-# DEPRECATED gisAggregator "Use generic-lens or generic-optics with 'aggregator' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisNextToken :: Lens.Lens' GetInventorySchema (Lude.Maybe Lude.Text)
gisNextToken = Lens.lens (nextToken :: GetInventorySchema -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInventorySchema)
{-# DEPRECATED gisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns the sub-type schema for a specified inventory type.
--
-- /Note:/ Consider using 'subType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisSubType :: Lens.Lens' GetInventorySchema (Lude.Maybe Lude.Bool)
gisSubType = Lens.lens (subType :: GetInventorySchema -> Lude.Maybe Lude.Bool) (\s a -> s {subType = a} :: GetInventorySchema)
{-# DEPRECATED gisSubType "Use generic-lens or generic-optics with 'subType' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisMaxResults :: Lens.Lens' GetInventorySchema (Lude.Maybe Lude.Natural)
gisMaxResults = Lens.lens (maxResults :: GetInventorySchema -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetInventorySchema)
{-# DEPRECATED gisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetInventorySchema where
  page rq rs
    | Page.stop (rs Lens.^. gisrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gisrsSchemas) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gisNextToken Lens..~ rs Lens.^. gisrsNextToken

instance Lude.AWSRequest GetInventorySchema where
  type Rs GetInventorySchema = GetInventorySchemaResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInventorySchemaResponse'
            Lude.<$> (x Lude..?> "Schemas" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInventorySchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetInventorySchema" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInventorySchema where
  toJSON GetInventorySchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TypeName" Lude..=) Lude.<$> typeName,
            ("Aggregator" Lude..=) Lude.<$> aggregator,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SubType" Lude..=) Lude.<$> subType,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetInventorySchema where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInventorySchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { schemas ::
      Lude.Maybe [InventoryItemSchema],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetInventorySchemaResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'schemas' - Inventory schemas returned by the request.
mkGetInventorySchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInventorySchemaResponse
mkGetInventorySchemaResponse pResponseStatus_ =
  GetInventorySchemaResponse'
    { schemas = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Inventory schemas returned by the request.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsSchemas :: Lens.Lens' GetInventorySchemaResponse (Lude.Maybe [InventoryItemSchema])
gisrsSchemas = Lens.lens (schemas :: GetInventorySchemaResponse -> Lude.Maybe [InventoryItemSchema]) (\s a -> s {schemas = a} :: GetInventorySchemaResponse)
{-# DEPRECATED gisrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsNextToken :: Lens.Lens' GetInventorySchemaResponse (Lude.Maybe Lude.Text)
gisrsNextToken = Lens.lens (nextToken :: GetInventorySchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInventorySchemaResponse)
{-# DEPRECATED gisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetInventorySchemaResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetInventorySchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInventorySchemaResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
