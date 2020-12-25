{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
-- You cannot delete a TagOption if it is associated with a product or portfolio.
module Network.AWS.ServiceCatalog.DeleteTagOption
  ( -- * Creating a request
    DeleteTagOption (..),
    mkDeleteTagOption,

    -- ** Request lenses
    dtofId,

    -- * Destructuring the response
    DeleteTagOptionResponse (..),
    mkDeleteTagOptionResponse,

    -- ** Response lenses
    dtorfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteTagOption' smart constructor.
newtype DeleteTagOption = DeleteTagOption'
  { -- | The TagOption identifier.
    id :: Types.TagOptionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagOption' value with any optional fields omitted.
mkDeleteTagOption ::
  -- | 'id'
  Types.TagOptionId ->
  DeleteTagOption
mkDeleteTagOption id = DeleteTagOption' {id}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofId :: Lens.Lens' DeleteTagOption Types.TagOptionId
dtofId = Lens.field @"id"
{-# DEPRECATED dtofId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DeleteTagOption where
  toJSON DeleteTagOption {..} =
    Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteTagOption where
  type Rs DeleteTagOption = DeleteTagOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteTagOption")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagOptionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTagOptionResponse' smart constructor.
newtype DeleteTagOptionResponse = DeleteTagOptionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagOptionResponse' value with any optional fields omitted.
mkDeleteTagOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTagOptionResponse
mkDeleteTagOptionResponse responseStatus =
  DeleteTagOptionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorfrsResponseStatus :: Lens.Lens' DeleteTagOptionResponse Core.Int
dtorfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtorfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
