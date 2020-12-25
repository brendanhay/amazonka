{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates data for the resource. To have the latest information, it must be preceded by a 'DescribeResource' call. The dataset in the request should be the one expected when performing another @DescribeResource@ call.
module Network.AWS.WorkMail.UpdateResource
  ( -- * Creating a request
    UpdateResource (..),
    mkUpdateResource,

    -- ** Request lenses
    urOrganizationId,
    urResourceId,
    urBookingOptions,
    urName,

    -- * Destructuring the response
    UpdateResourceResponse (..),
    mkUpdateResourceResponse,

    -- ** Response lenses
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { -- | The identifier associated with the organization for which the resource is updated.
    organizationId :: Types.OrganizationId,
    -- | The identifier of the resource to be updated.
    resourceId :: Types.ResourceId,
    -- | The resource's booking options to be updated.
    bookingOptions :: Core.Maybe Types.BookingOptions,
    -- | The name of the resource to be updated.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResource' value with any optional fields omitted.
mkUpdateResource ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'resourceId'
  Types.ResourceId ->
  UpdateResource
mkUpdateResource organizationId resourceId =
  UpdateResource'
    { organizationId,
      resourceId,
      bookingOptions = Core.Nothing,
      name = Core.Nothing
    }

-- | The identifier associated with the organization for which the resource is updated.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urOrganizationId :: Lens.Lens' UpdateResource Types.OrganizationId
urOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED urOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the resource to be updated.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UpdateResource Types.ResourceId
urResourceId = Lens.field @"resourceId"
{-# DEPRECATED urResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource's booking options to be updated.
--
-- /Note:/ Consider using 'bookingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urBookingOptions :: Lens.Lens' UpdateResource (Core.Maybe Types.BookingOptions)
urBookingOptions = Lens.field @"bookingOptions"
{-# DEPRECATED urBookingOptions "Use generic-lens or generic-optics with 'bookingOptions' instead." #-}

-- | The name of the resource to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urName :: Lens.Lens' UpdateResource (Core.Maybe Types.Name)
urName = Lens.field @"name"
{-# DEPRECATED urName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateResource where
  toJSON UpdateResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("ResourceId" Core..= resourceId),
            ("BookingOptions" Core..=) Core.<$> bookingOptions,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateResource where
  type Rs UpdateResource = UpdateResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.UpdateResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateResourceResponse' smart constructor.
newtype UpdateResourceResponse = UpdateResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceResponse' value with any optional fields omitted.
mkUpdateResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateResourceResponse
mkUpdateResourceResponse responseStatus =
  UpdateResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateResourceResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
