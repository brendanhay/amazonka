{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RebootInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a reboot of the specified instances. This operation is asynchronous; it only queues a request to reboot the specified instances. The operation succeeds if the instances are valid and belong to you. Requests to reboot terminated instances are ignored.
--
-- If an instance does not cleanly shut down within a few minutes, Amazon EC2 performs a hard reboot.
-- For more information about troubleshooting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html Getting console output and rebooting instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.RebootInstances
    (
    -- * Creating a request
      RebootInstances (..)
    , mkRebootInstances
    -- ** Request lenses
    , rInstanceIds
    , rDryRun

    -- * Destructuring the response
    , RebootInstancesResponse (..)
    , mkRebootInstancesResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootInstances' smart constructor.
data RebootInstances = RebootInstances'
  { instanceIds :: [Types.InstanceId]
    -- ^ The instance IDs.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootInstances' value with any optional fields omitted.
mkRebootInstances
    :: RebootInstances
mkRebootInstances
  = RebootInstances'{instanceIds = Core.mempty,
                     dryRun = Core.Nothing}

-- | The instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceIds :: Lens.Lens' RebootInstances [Types.InstanceId]
rInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE rInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDryRun :: Lens.Lens' RebootInstances (Core.Maybe Core.Bool)
rDryRun = Lens.field @"dryRun"
{-# INLINEABLE rDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RebootInstances where
        toQuery RebootInstances{..}
          = Core.toQueryPair "Action" ("RebootInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "InstanceId" instanceIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RebootInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RebootInstances where
        type Rs RebootInstances = RebootInstancesResponse
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
        parseResponse = Response.receiveNull RebootInstancesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootInstancesResponse' value with any optional fields omitted.
mkRebootInstancesResponse
    :: RebootInstancesResponse
mkRebootInstancesResponse = RebootInstancesResponse'
