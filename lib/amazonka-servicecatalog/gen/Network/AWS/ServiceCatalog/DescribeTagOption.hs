{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
module Network.AWS.ServiceCatalog.DescribeTagOption
  ( -- * Creating a request
    DescribeTagOption (..),
    mkDescribeTagOption,

    -- ** Request lenses
    dtoId,

    -- * Destructuring the response
    DescribeTagOptionResponse (..),
    mkDescribeTagOptionResponse,

    -- ** Response lenses
    dtorrsTagOptionDetail,
    dtorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeTagOption' smart constructor.
newtype DescribeTagOption = DescribeTagOption'
  { -- | The TagOption identifier.
    id :: Types.TagOptionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagOption' value with any optional fields omitted.
mkDescribeTagOption ::
  -- | 'id'
  Types.TagOptionId ->
  DescribeTagOption
mkDescribeTagOption id = DescribeTagOption' {id}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtoId :: Lens.Lens' DescribeTagOption Types.TagOptionId
dtoId = Lens.field @"id"
{-# DEPRECATED dtoId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DescribeTagOption where
  toJSON DescribeTagOption {..} =
    Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DescribeTagOption where
  type Rs DescribeTagOption = DescribeTagOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeTagOption")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagOptionResponse'
            Core.<$> (x Core..:? "TagOptionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Core.Maybe Types.TagOptionDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagOptionResponse' value with any optional fields omitted.
mkDescribeTagOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTagOptionResponse
mkDescribeTagOptionResponse responseStatus =
  DescribeTagOptionResponse'
    { tagOptionDetail = Core.Nothing,
      responseStatus
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorrsTagOptionDetail :: Lens.Lens' DescribeTagOptionResponse (Core.Maybe Types.TagOptionDetail)
dtorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# DEPRECATED dtorrsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorrsResponseStatus :: Lens.Lens' DescribeTagOptionResponse Core.Int
dtorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
