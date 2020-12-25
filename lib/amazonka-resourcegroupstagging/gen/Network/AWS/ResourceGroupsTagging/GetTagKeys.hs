{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag keys in the specified Region for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagKeys
  ( -- * Creating a request
    GetTagKeys (..),
    mkGetTagKeys,

    -- ** Request lenses
    gtkPaginationToken,

    -- * Destructuring the response
    GetTagKeysResponse (..),
    mkGetTagKeysResponse,

    -- ** Response lenses
    gtkrrsPaginationToken,
    gtkrrsTagKeys,
    gtkrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTagKeys' smart constructor.
newtype GetTagKeys = GetTagKeys'
  { -- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
    paginationToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagKeys' value with any optional fields omitted.
mkGetTagKeys ::
  GetTagKeys
mkGetTagKeys = GetTagKeys' {paginationToken = Core.Nothing}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkPaginationToken :: Lens.Lens' GetTagKeys (Core.Maybe Types.PaginationToken)
gtkPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED gtkPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

instance Core.FromJSON GetTagKeys where
  toJSON GetTagKeys {..} =
    Core.object
      ( Core.catMaybes
          [("PaginationToken" Core..=) Core.<$> paginationToken]
      )

instance Core.AWSRequest GetTagKeys where
  type Rs GetTagKeys = GetTagKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ResourceGroupsTaggingAPI_20170126.GetTagKeys")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagKeysResponse'
            Core.<$> (x Core..:? "PaginationToken")
            Core.<*> (x Core..:? "TagKeys")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTagKeys where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"paginationToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tagKeys" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"paginationToken"
            Lens..~ rs Lens.^. Lens.field @"paginationToken"
        )

-- | /See:/ 'mkGetTagKeysResponse' smart constructor.
data GetTagKeysResponse = GetTagKeysResponse'
  { -- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
    paginationToken :: Core.Maybe Types.PaginationToken,
    -- | A list of all tag keys in the AWS account.
    tagKeys :: Core.Maybe [Types.TagKey],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagKeysResponse' value with any optional fields omitted.
mkGetTagKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTagKeysResponse
mkGetTagKeysResponse responseStatus =
  GetTagKeysResponse'
    { paginationToken = Core.Nothing,
      tagKeys = Core.Nothing,
      responseStatus
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrrsPaginationToken :: Lens.Lens' GetTagKeysResponse (Core.Maybe Types.PaginationToken)
gtkrrsPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED gtkrrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A list of all tag keys in the AWS account.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrrsTagKeys :: Lens.Lens' GetTagKeysResponse (Core.Maybe [Types.TagKey])
gtkrrsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED gtkrrsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrrsResponseStatus :: Lens.Lens' GetTagKeysResponse Core.Int
gtkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
