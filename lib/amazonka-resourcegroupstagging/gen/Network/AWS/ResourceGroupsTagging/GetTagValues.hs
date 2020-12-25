{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag values for the specified key in the specified Region for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagValues
  ( -- * Creating a request
    GetTagValues (..),
    mkGetTagValues,

    -- ** Request lenses
    gtvKey,
    gtvPaginationToken,

    -- * Destructuring the response
    GetTagValuesResponse (..),
    mkGetTagValuesResponse,

    -- ** Response lenses
    gtvrrsPaginationToken,
    gtvrrsTagValues,
    gtvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTagValues' smart constructor.
data GetTagValues = GetTagValues'
  { -- | The key for which you want to list all existing values in the specified Region for the AWS account.
    key :: Types.Key,
    -- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
    paginationToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagValues' value with any optional fields omitted.
mkGetTagValues ::
  -- | 'key'
  Types.Key ->
  GetTagValues
mkGetTagValues key =
  GetTagValues' {key, paginationToken = Core.Nothing}

-- | The key for which you want to list all existing values in the specified Region for the AWS account.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvKey :: Lens.Lens' GetTagValues Types.Key
gtvKey = Lens.field @"key"
{-# DEPRECATED gtvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvPaginationToken :: Lens.Lens' GetTagValues (Core.Maybe Types.PaginationToken)
gtvPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED gtvPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

instance Core.FromJSON GetTagValues where
  toJSON GetTagValues {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            ("PaginationToken" Core..=) Core.<$> paginationToken
          ]
      )

instance Core.AWSRequest GetTagValues where
  type Rs GetTagValues = GetTagValuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ResourceGroupsTaggingAPI_20170126.GetTagValues")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagValuesResponse'
            Core.<$> (x Core..:? "PaginationToken")
            Core.<*> (x Core..:? "TagValues")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTagValues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"paginationToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tagValues" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"paginationToken"
            Lens..~ rs Lens.^. Lens.field @"paginationToken"
        )

-- | /See:/ 'mkGetTagValuesResponse' smart constructor.
data GetTagValuesResponse = GetTagValuesResponse'
  { -- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
    paginationToken :: Core.Maybe Types.PaginationToken,
    -- | A list of all tag values for the specified key in the AWS account.
    tagValues :: Core.Maybe [Types.TagValue],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagValuesResponse' value with any optional fields omitted.
mkGetTagValuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTagValuesResponse
mkGetTagValuesResponse responseStatus =
  GetTagValuesResponse'
    { paginationToken = Core.Nothing,
      tagValues = Core.Nothing,
      responseStatus
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrrsPaginationToken :: Lens.Lens' GetTagValuesResponse (Core.Maybe Types.PaginationToken)
gtvrrsPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED gtvrrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A list of all tag values for the specified key in the AWS account.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrrsTagValues :: Lens.Lens' GetTagValuesResponse (Core.Maybe [Types.TagValue])
gtvrrsTagValues = Lens.field @"tagValues"
{-# DEPRECATED gtvrrsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrrsResponseStatus :: Lens.Lens' GetTagValuesResponse Core.Int
gtvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
