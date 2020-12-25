{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the settings for a specified service.
module Network.AWS.Route53AutoNaming.GetService
  ( -- * Creating a request
    GetService (..),
    mkGetService,

    -- ** Request lenses
    gsId,

    -- * Destructuring the response
    GetServiceResponse (..),
    mkGetServiceResponse,

    -- ** Response lenses
    gsrrsService,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkGetService' smart constructor.
newtype GetService = GetService'
  { -- | The ID of the service that you want to get settings for.
    id :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetService' value with any optional fields omitted.
mkGetService ::
  -- | 'id'
  Types.ResourceId ->
  GetService
mkGetService id = GetService' {id}

-- | The ID of the service that you want to get settings for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsId :: Lens.Lens' GetService Types.ResourceId
gsId = Lens.field @"id"
{-# DEPRECATED gsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON GetService where
  toJSON GetService {..} =
    Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest GetService where
  type Rs GetService = GetServiceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.GetService")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceResponse'
            Core.<$> (x Core..:? "Service") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { -- | A complex type that contains information about the service.
    service :: Core.Maybe Types.ServiceInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetServiceResponse' value with any optional fields omitted.
mkGetServiceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetServiceResponse
mkGetServiceResponse responseStatus =
  GetServiceResponse' {service = Core.Nothing, responseStatus}

-- | A complex type that contains information about the service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsService :: Lens.Lens' GetServiceResponse (Core.Maybe Types.ServiceInfo)
gsrrsService = Lens.field @"service"
{-# DEPRECATED gsrrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetServiceResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
