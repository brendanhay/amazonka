{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorFilter
    (
    -- * Creating a request
      DeleteTrafficMirrorFilter (..)
    , mkDeleteTrafficMirrorFilter
    -- ** Request lenses
    , dtmffTrafficMirrorFilterId
    , dtmffDryRun

    -- * Destructuring the response
    , DeleteTrafficMirrorFilterResponse (..)
    , mkDeleteTrafficMirrorFilterResponse
    -- ** Response lenses
    , dtmfrfrsTrafficMirrorFilterId
    , dtmfrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { trafficMirrorFilterId :: Types.TrafficMirrorFilterId
    -- ^ The ID of the Traffic Mirror filter.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilter' value with any optional fields omitted.
mkDeleteTrafficMirrorFilter
    :: Types.TrafficMirrorFilterId -- ^ 'trafficMirrorFilterId'
    -> DeleteTrafficMirrorFilter
mkDeleteTrafficMirrorFilter trafficMirrorFilterId
  = DeleteTrafficMirrorFilter'{trafficMirrorFilterId,
                               dryRun = Core.Nothing}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmffTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilter Types.TrafficMirrorFilterId
dtmffTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE dtmffTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmffDryRun :: Lens.Lens' DeleteTrafficMirrorFilter (Core.Maybe Core.Bool)
dtmffDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtmffDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTrafficMirrorFilter where
        toQuery DeleteTrafficMirrorFilter{..}
          = Core.toQueryPair "Action"
              ("DeleteTrafficMirrorFilter" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TrafficMirrorFilterId" trafficMirrorFilterId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTrafficMirrorFilter where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTrafficMirrorFilter where
        type Rs DeleteTrafficMirrorFilter =
             DeleteTrafficMirrorFilterResponse
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
                 DeleteTrafficMirrorFilterResponse' Core.<$>
                   (x Core..@? "trafficMirrorFilterId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { trafficMirrorFilterId :: Core.Maybe Core.Text
    -- ^ The ID of the Traffic Mirror filter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficMirrorFilterResponse' value with any optional fields omitted.
mkDeleteTrafficMirrorFilterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTrafficMirrorFilterResponse
mkDeleteTrafficMirrorFilterResponse responseStatus
  = DeleteTrafficMirrorFilterResponse'{trafficMirrorFilterId =
                                         Core.Nothing,
                                       responseStatus}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrfrsTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilterResponse (Core.Maybe Core.Text)
dtmfrfrsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE dtmfrfrsTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrfrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorFilterResponse Core.Int
dtmfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtmfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
