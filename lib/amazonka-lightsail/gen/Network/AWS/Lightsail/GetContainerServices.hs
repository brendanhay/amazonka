{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail container services.
module Network.AWS.Lightsail.GetContainerServices
  ( -- * Creating a request
    GetContainerServices (..),
    mkGetContainerServices,

    -- ** Request lenses
    gcsServiceName,

    -- * Destructuring the response
    GetContainerServicesResponse (..),
    mkGetContainerServicesResponse,

    -- ** Response lenses
    gcsrrsContainerServices,
    gcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerServices' smart constructor.
newtype GetContainerServices = GetContainerServices'
  { -- | The name of the container service for which to return information.
    --
    -- When omitted, the response includes all of your container services in the AWS Region where the request is made.
    serviceName :: Core.Maybe Types.ContainerServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerServices' value with any optional fields omitted.
mkGetContainerServices ::
  GetContainerServices
mkGetContainerServices =
  GetContainerServices' {serviceName = Core.Nothing}

-- | The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsServiceName :: Lens.Lens' GetContainerServices (Core.Maybe Types.ContainerServiceName)
gcsServiceName = Lens.field @"serviceName"
{-# DEPRECATED gcsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromJSON GetContainerServices where
  toJSON GetContainerServices {..} =
    Core.object
      (Core.catMaybes [("serviceName" Core..=) Core.<$> serviceName])

instance Core.AWSRequest GetContainerServices where
  type Rs GetContainerServices = GetContainerServicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetContainerServices")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicesResponse'
            Core.<$> (x Core..:? "containerServices")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerServicesResponse' smart constructor.
data GetContainerServicesResponse = GetContainerServicesResponse'
  { -- | An array of objects that describe one or more container services.
    containerServices :: Core.Maybe [Types.ContainerService],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerServicesResponse' value with any optional fields omitted.
mkGetContainerServicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContainerServicesResponse
mkGetContainerServicesResponse responseStatus =
  GetContainerServicesResponse'
    { containerServices = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe one or more container services.
--
-- /Note:/ Consider using 'containerServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsContainerServices :: Lens.Lens' GetContainerServicesResponse (Core.Maybe [Types.ContainerService])
gcsrrsContainerServices = Lens.field @"containerServices"
{-# DEPRECATED gcsrrsContainerServices "Use generic-lens or generic-optics with 'containerServices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsResponseStatus :: Lens.Lens' GetContainerServicesResponse Core.Int
gcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
