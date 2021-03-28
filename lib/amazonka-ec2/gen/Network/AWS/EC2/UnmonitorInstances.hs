{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables detailed monitoring for a running instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.UnmonitorInstances
    (
    -- * Creating a request
      UnmonitorInstances (..)
    , mkUnmonitorInstances
    -- ** Request lenses
    , uiInstanceIds
    , uiDryRun

    -- * Destructuring the response
    , UnmonitorInstancesResponse (..)
    , mkUnmonitorInstancesResponse
    -- ** Response lenses
    , uirrsInstanceMonitorings
    , uirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnmonitorInstances' smart constructor.
data UnmonitorInstances = UnmonitorInstances'
  { instanceIds :: [Types.InstanceId]
    -- ^ The IDs of the instances.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnmonitorInstances' value with any optional fields omitted.
mkUnmonitorInstances
    :: UnmonitorInstances
mkUnmonitorInstances
  = UnmonitorInstances'{instanceIds = Core.mempty,
                        dryRun = Core.Nothing}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstanceIds :: Lens.Lens' UnmonitorInstances [Types.InstanceId]
uiInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE uiInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDryRun :: Lens.Lens' UnmonitorInstances (Core.Maybe Core.Bool)
uiDryRun = Lens.field @"dryRun"
{-# INLINEABLE uiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery UnmonitorInstances where
        toQuery UnmonitorInstances{..}
          = Core.toQueryPair "Action" ("UnmonitorInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "InstanceId" instanceIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders UnmonitorInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UnmonitorInstances where
        type Rs UnmonitorInstances = UnmonitorInstancesResponse
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
                 UnmonitorInstancesResponse' Core.<$>
                   (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnmonitorInstancesResponse' smart constructor.
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
  { instanceMonitorings :: Core.Maybe [Types.InstanceMonitoring]
    -- ^ The monitoring information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnmonitorInstancesResponse' value with any optional fields omitted.
mkUnmonitorInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UnmonitorInstancesResponse
mkUnmonitorInstancesResponse responseStatus
  = UnmonitorInstancesResponse'{instanceMonitorings = Core.Nothing,
                                responseStatus}

-- | The monitoring information.
--
-- /Note:/ Consider using 'instanceMonitorings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsInstanceMonitorings :: Lens.Lens' UnmonitorInstancesResponse (Core.Maybe [Types.InstanceMonitoring])
uirrsInstanceMonitorings = Lens.field @"instanceMonitorings"
{-# INLINEABLE uirrsInstanceMonitorings #-}
{-# DEPRECATED instanceMonitorings "Use generic-lens or generic-optics with 'instanceMonitorings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsResponseStatus :: Lens.Lens' UnmonitorInstancesResponse Core.Int
uirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
