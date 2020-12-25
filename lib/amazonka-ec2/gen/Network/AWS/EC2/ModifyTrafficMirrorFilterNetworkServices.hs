{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows or restricts mirroring network services.
--
-- By default, Amazon DNS network services are not eligible for Traffic Mirror. Use @AddNetworkServices@ to add network services to a Traffic Mirror filter. When a network service is added to the Traffic Mirror filter, all traffic related to that network service will be mirrored. When you no longer want to mirror network services, use @RemoveNetworkServices@ to remove the network services from the Traffic Mirror filter.
-- For information about filter rule properties, see <https://docs.aws.amazon.com/vpc/latest/mirroring/traffic-mirroring-considerations.html Network Services> in the /Traffic Mirroring User Guide / .
module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
  ( -- * Creating a request
    ModifyTrafficMirrorFilterNetworkServices (..),
    mkModifyTrafficMirrorFilterNetworkServices,

    -- ** Request lenses
    mtmfnsTrafficMirrorFilterId,
    mtmfnsAddNetworkServices,
    mtmfnsDryRun,
    mtmfnsRemoveNetworkServices,

    -- * Destructuring the response
    ModifyTrafficMirrorFilterNetworkServicesResponse (..),
    mkModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** Response lenses
    mtmfnsrrsTrafficMirrorFilter,
    mtmfnsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Types.TrafficMirrorFilterId,
    -- | The network service, for example Amazon DNS, that you want to mirror.
    addNetworkServices :: Core.Maybe [Types.TrafficMirrorNetworkService],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The network service, for example Amazon DNS, that you no longer want to mirror.
    removeNetworkServices :: Core.Maybe [Types.TrafficMirrorNetworkService]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterNetworkServices' value with any optional fields omitted.
mkModifyTrafficMirrorFilterNetworkServices ::
  -- | 'trafficMirrorFilterId'
  Types.TrafficMirrorFilterId ->
  ModifyTrafficMirrorFilterNetworkServices
mkModifyTrafficMirrorFilterNetworkServices trafficMirrorFilterId =
  ModifyTrafficMirrorFilterNetworkServices'
    { trafficMirrorFilterId,
      addNetworkServices = Core.Nothing,
      dryRun = Core.Nothing,
      removeNetworkServices = Core.Nothing
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices Types.TrafficMirrorFilterId
mtmfnsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED mtmfnsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The network service, for example Amazon DNS, that you want to mirror.
--
-- /Note:/ Consider using 'addNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsAddNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [Types.TrafficMirrorNetworkService])
mtmfnsAddNetworkServices = Lens.field @"addNetworkServices"
{-# DEPRECATED mtmfnsAddNetworkServices "Use generic-lens or generic-optics with 'addNetworkServices' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsDryRun :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe Core.Bool)
mtmfnsDryRun = Lens.field @"dryRun"
{-# DEPRECATED mtmfnsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The network service, for example Amazon DNS, that you no longer want to mirror.
--
-- /Note:/ Consider using 'removeNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsRemoveNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [Types.TrafficMirrorNetworkService])
mtmfnsRemoveNetworkServices = Lens.field @"removeNetworkServices"
{-# DEPRECATED mtmfnsRemoveNetworkServices "Use generic-lens or generic-optics with 'removeNetworkServices' instead." #-}

instance Core.AWSRequest ModifyTrafficMirrorFilterNetworkServices where
  type
    Rs ModifyTrafficMirrorFilterNetworkServices =
      ModifyTrafficMirrorFilterNetworkServicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyTrafficMirrorFilterNetworkServices")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorFilterId" trafficMirrorFilterId)
                Core.<> (Core.toQueryList "AddNetworkService" Core.<$> addNetworkServices)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryList "RemoveNetworkService"
                            Core.<$> removeNetworkServices
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterNetworkServicesResponse'
            Core.<$> (x Core..@? "trafficMirrorFilter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { -- | The Traffic Mirror filter that the network service is associated with.
    trafficMirrorFilter :: Core.Maybe Types.TrafficMirrorFilter,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterNetworkServicesResponse' value with any optional fields omitted.
mkModifyTrafficMirrorFilterNetworkServicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTrafficMirrorFilterNetworkServicesResponse
mkModifyTrafficMirrorFilterNetworkServicesResponse responseStatus =
  ModifyTrafficMirrorFilterNetworkServicesResponse'
    { trafficMirrorFilter =
        Core.Nothing,
      responseStatus
    }

-- | The Traffic Mirror filter that the network service is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrrsTrafficMirrorFilter :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Core.Maybe Types.TrafficMirrorFilter)
mtmfnsrrsTrafficMirrorFilter = Lens.field @"trafficMirrorFilter"
{-# DEPRECATED mtmfnsrrsTrafficMirrorFilter "Use generic-lens or generic-optics with 'trafficMirrorFilter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Core.Int
mtmfnsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtmfnsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
