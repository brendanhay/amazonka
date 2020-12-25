{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or product.
module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
  ( -- * Creating a request
    AssociateTagOptionWithResource (..),
    mkAssociateTagOptionWithResource,

    -- ** Request lenses
    atowrResourceId,
    atowrTagOptionId,

    -- * Destructuring the response
    AssociateTagOptionWithResourceResponse (..),
    mkAssociateTagOptionWithResourceResponse,

    -- ** Response lenses
    atowrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { -- | The resource identifier.
    resourceId :: Types.ResourceId,
    -- | The TagOption identifier.
    tagOptionId :: Types.TagOptionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTagOptionWithResource' value with any optional fields omitted.
mkAssociateTagOptionWithResource ::
  -- | 'resourceId'
  Types.ResourceId ->
  -- | 'tagOptionId'
  Types.TagOptionId ->
  AssociateTagOptionWithResource
mkAssociateTagOptionWithResource resourceId tagOptionId =
  AssociateTagOptionWithResource' {resourceId, tagOptionId}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrResourceId :: Lens.Lens' AssociateTagOptionWithResource Types.ResourceId
atowrResourceId = Lens.field @"resourceId"
{-# DEPRECATED atowrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrTagOptionId :: Lens.Lens' AssociateTagOptionWithResource Types.TagOptionId
atowrTagOptionId = Lens.field @"tagOptionId"
{-# DEPRECATED atowrTagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead." #-}

instance Core.FromJSON AssociateTagOptionWithResource where
  toJSON AssociateTagOptionWithResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance Core.AWSRequest AssociateTagOptionWithResource where
  type
    Rs AssociateTagOptionWithResource =
      AssociateTagOptionWithResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.AssociateTagOptionWithResource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateTagOptionWithResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTagOptionWithResourceResponse' smart constructor.
newtype AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTagOptionWithResourceResponse' value with any optional fields omitted.
mkAssociateTagOptionWithResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTagOptionWithResourceResponse
mkAssociateTagOptionWithResourceResponse responseStatus =
  AssociateTagOptionWithResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrrrsResponseStatus :: Lens.Lens' AssociateTagOptionWithResourceResponse Core.Int
atowrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atowrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
