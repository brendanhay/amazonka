{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyTrafficMirrorFilterNetworkServices (..)
    , mkModifyTrafficMirrorFilterNetworkServices
    -- ** Request lenses
    , mtmfnsTrafficMirrorFilterId
    , mtmfnsAddNetworkServices
    , mtmfnsDryRun
    , mtmfnsRemoveNetworkServices

    -- * Destructuring the response
    , ModifyTrafficMirrorFilterNetworkServicesResponse (..)
    , mkModifyTrafficMirrorFilterNetworkServicesResponse
    -- ** Response lenses
    , mtmfnsrrsTrafficMirrorFilter
    , mtmfnsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { trafficMirrorFilterId :: Types.TrafficMirrorFilterId
    -- ^ The ID of the Traffic Mirror filter.
  , addNetworkServices :: Core.Maybe [Types.TrafficMirrorNetworkService]
    -- ^ The network service, for example Amazon DNS, that you want to mirror.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , removeNetworkServices :: Core.Maybe [Types.TrafficMirrorNetworkService]
    -- ^ The network service, for example Amazon DNS, that you no longer want to mirror.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterNetworkServices' value with any optional fields omitted.
mkModifyTrafficMirrorFilterNetworkServices
    :: Types.TrafficMirrorFilterId -- ^ 'trafficMirrorFilterId'
    -> ModifyTrafficMirrorFilterNetworkServices
mkModifyTrafficMirrorFilterNetworkServices trafficMirrorFilterId
  = ModifyTrafficMirrorFilterNetworkServices'{trafficMirrorFilterId,
                                              addNetworkServices = Core.Nothing,
                                              dryRun = Core.Nothing,
                                              removeNetworkServices = Core.Nothing}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices Types.TrafficMirrorFilterId
mtmfnsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE mtmfnsTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The network service, for example Amazon DNS, that you want to mirror.
--
-- /Note:/ Consider using 'addNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsAddNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [Types.TrafficMirrorNetworkService])
mtmfnsAddNetworkServices = Lens.field @"addNetworkServices"
{-# INLINEABLE mtmfnsAddNetworkServices #-}
{-# DEPRECATED addNetworkServices "Use generic-lens or generic-optics with 'addNetworkServices' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsDryRun :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe Core.Bool)
mtmfnsDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtmfnsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The network service, for example Amazon DNS, that you no longer want to mirror.
--
-- /Note:/ Consider using 'removeNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsRemoveNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [Types.TrafficMirrorNetworkService])
mtmfnsRemoveNetworkServices = Lens.field @"removeNetworkServices"
{-# INLINEABLE mtmfnsRemoveNetworkServices #-}
{-# DEPRECATED removeNetworkServices "Use generic-lens or generic-optics with 'removeNetworkServices' instead"  #-}

instance Core.ToQuery ModifyTrafficMirrorFilterNetworkServices
         where
        toQuery ModifyTrafficMirrorFilterNetworkServices{..}
          = Core.toQueryPair "Action"
              ("ModifyTrafficMirrorFilterNetworkServices" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TrafficMirrorFilterId" trafficMirrorFilterId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddNetworkService")
                addNetworkServices
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveNetworkService")
                removeNetworkServices

instance Core.ToHeaders ModifyTrafficMirrorFilterNetworkServices
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTrafficMirrorFilterNetworkServices
         where
        type Rs ModifyTrafficMirrorFilterNetworkServices =
             ModifyTrafficMirrorFilterNetworkServicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyTrafficMirrorFilterNetworkServicesResponse' Core.<$>
                   (x Core..@? "trafficMirrorFilter") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { trafficMirrorFilter :: Core.Maybe Types.TrafficMirrorFilter
    -- ^ The Traffic Mirror filter that the network service is associated with.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterNetworkServicesResponse' value with any optional fields omitted.
mkModifyTrafficMirrorFilterNetworkServicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTrafficMirrorFilterNetworkServicesResponse
mkModifyTrafficMirrorFilterNetworkServicesResponse responseStatus
  = ModifyTrafficMirrorFilterNetworkServicesResponse'{trafficMirrorFilter
                                                        = Core.Nothing,
                                                      responseStatus}

-- | The Traffic Mirror filter that the network service is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrrsTrafficMirrorFilter :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Core.Maybe Types.TrafficMirrorFilter)
mtmfnsrrsTrafficMirrorFilter = Lens.field @"trafficMirrorFilter"
{-# INLINEABLE mtmfnsrrsTrafficMirrorFilter #-}
{-# DEPRECATED trafficMirrorFilter "Use generic-lens or generic-optics with 'trafficMirrorFilter' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Core.Int
mtmfnsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtmfnsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
