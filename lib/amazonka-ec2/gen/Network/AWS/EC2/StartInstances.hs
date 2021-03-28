{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon EBS-backed instance that you've previously stopped.
--
-- Instances that use Amazon EBS volumes as their root devices can be quickly stopped and started. When an instance is stopped, the compute resources are released and you are not billed for instance usage. However, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. You can restart your instance at any time. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
-- Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.
-- Performing this operation on an instance that uses an instance store as its root device returns an error.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stopping instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.StartInstances
    (
    -- * Creating a request
      StartInstances (..)
    , mkStartInstances
    -- ** Request lenses
    , sInstanceIds
    , sAdditionalInfo
    , sDryRun

    -- * Destructuring the response
    , StartInstancesResponse (..)
    , mkStartInstancesResponse
    -- ** Response lenses
    , srsStartingInstances
    , srsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartInstances' smart constructor.
data StartInstances = StartInstances'
  { instanceIds :: [Types.InstanceId]
    -- ^ The IDs of the instances.
  , additionalInfo :: Core.Maybe Core.Text
    -- ^ Reserved.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartInstances' value with any optional fields omitted.
mkStartInstances
    :: StartInstances
mkStartInstances
  = StartInstances'{instanceIds = Core.mempty,
                    additionalInfo = Core.Nothing, dryRun = Core.Nothing}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceIds :: Lens.Lens' StartInstances [Types.InstanceId]
sInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE sInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAdditionalInfo :: Lens.Lens' StartInstances (Core.Maybe Core.Text)
sAdditionalInfo = Lens.field @"additionalInfo"
{-# INLINEABLE sAdditionalInfo #-}
{-# DEPRECATED additionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDryRun :: Lens.Lens' StartInstances (Core.Maybe Core.Bool)
sDryRun = Lens.field @"dryRun"
{-# INLINEABLE sDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery StartInstances where
        toQuery StartInstances{..}
          = Core.toQueryPair "Action" ("StartInstances" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "InstanceId" instanceIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdditionalInfo")
                additionalInfo
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders StartInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest StartInstances where
        type Rs StartInstances = StartInstancesResponse
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
                 StartInstancesResponse' Core.<$>
                   (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartInstancesResponse' smart constructor.
data StartInstancesResponse = StartInstancesResponse'
  { startingInstances :: Core.Maybe [Types.InstanceStateChange]
    -- ^ Information about the started instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartInstancesResponse' value with any optional fields omitted.
mkStartInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartInstancesResponse
mkStartInstancesResponse responseStatus
  = StartInstancesResponse'{startingInstances = Core.Nothing,
                            responseStatus}

-- | Information about the started instances.
--
-- /Note:/ Consider using 'startingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStartingInstances :: Lens.Lens' StartInstancesResponse (Core.Maybe [Types.InstanceStateChange])
srsStartingInstances = Lens.field @"startingInstances"
{-# INLINEABLE srsStartingInstances #-}
{-# DEPRECATED startingInstances "Use generic-lens or generic-optics with 'startingInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartInstancesResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
