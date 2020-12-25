{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
  ( -- * Creating a request
    DisassociateTagOptionFromResource (..),
    mkDisassociateTagOptionFromResource,

    -- ** Request lenses
    dtofrResourceId,
    dtofrTagOptionId,

    -- * Destructuring the response
    DisassociateTagOptionFromResourceResponse (..),
    mkDisassociateTagOptionFromResourceResponse,

    -- ** Response lenses
    dtofrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { -- | The resource identifier.
    resourceId :: Types.ResourceId,
    -- | The TagOption identifier.
    tagOptionId :: Types.TagOptionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTagOptionFromResource' value with any optional fields omitted.
mkDisassociateTagOptionFromResource ::
  -- | 'resourceId'
  Types.ResourceId ->
  -- | 'tagOptionId'
  Types.TagOptionId ->
  DisassociateTagOptionFromResource
mkDisassociateTagOptionFromResource resourceId tagOptionId =
  DisassociateTagOptionFromResource' {resourceId, tagOptionId}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrResourceId :: Lens.Lens' DisassociateTagOptionFromResource Types.ResourceId
dtofrResourceId = Lens.field @"resourceId"
{-# DEPRECATED dtofrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrTagOptionId :: Lens.Lens' DisassociateTagOptionFromResource Types.TagOptionId
dtofrTagOptionId = Lens.field @"tagOptionId"
{-# DEPRECATED dtofrTagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead." #-}

instance Core.FromJSON DisassociateTagOptionFromResource where
  toJSON DisassociateTagOptionFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance Core.AWSRequest DisassociateTagOptionFromResource where
  type
    Rs DisassociateTagOptionFromResource =
      DisassociateTagOptionFromResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DisassociateTagOptionFromResource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTagOptionFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateTagOptionFromResourceResponse' smart constructor.
newtype DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTagOptionFromResourceResponse' value with any optional fields omitted.
mkDisassociateTagOptionFromResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateTagOptionFromResourceResponse
mkDisassociateTagOptionFromResourceResponse responseStatus =
  DisassociateTagOptionFromResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrrrsResponseStatus :: Lens.Lens' DisassociateTagOptionFromResourceResponse Core.Int
dtofrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtofrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
