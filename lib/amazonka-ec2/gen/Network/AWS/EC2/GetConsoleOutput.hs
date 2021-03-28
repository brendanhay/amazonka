{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the console output for the specified instance. For Linux instances, the instance console output displays the exact console output that would normally be displayed on a physical monitor attached to a computer. For Windows instances, the instance console output includes the last three system event log errors.
--
-- By default, the console output returns buffered information that was posted shortly after an instance transition state (start, stop, reboot, or terminate). This information is available for at least one hour after the most recent post. Only the most recent 64 KB of console output is available.
-- You can optionally retrieve the latest serial console output at any time during the instance lifecycle. This option is supported on instance types that use the Nitro hypervisor.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html#instance-console-console-output Instance Console Output> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetConsoleOutput
    (
    -- * Creating a request
      GetConsoleOutput (..)
    , mkGetConsoleOutput
    -- ** Request lenses
    , gcoInstanceId
    , gcoDryRun
    , gcoLatest

    -- * Destructuring the response
    , GetConsoleOutputResponse (..)
    , mkGetConsoleOutputResponse
    -- ** Response lenses
    , gcorrsInstanceId
    , gcorrsOutput
    , gcorrsTimestamp
    , gcorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConsoleOutput' smart constructor.
data GetConsoleOutput = GetConsoleOutput'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , latest :: Core.Maybe Core.Bool
    -- ^ When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@ )
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConsoleOutput' value with any optional fields omitted.
mkGetConsoleOutput
    :: Types.InstanceId -- ^ 'instanceId'
    -> GetConsoleOutput
mkGetConsoleOutput instanceId
  = GetConsoleOutput'{instanceId, dryRun = Core.Nothing,
                      latest = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoInstanceId :: Lens.Lens' GetConsoleOutput Types.InstanceId
gcoInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gcoInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoDryRun :: Lens.Lens' GetConsoleOutput (Core.Maybe Core.Bool)
gcoDryRun = Lens.field @"dryRun"
{-# INLINEABLE gcoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@ )
--
-- /Note:/ Consider using 'latest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoLatest :: Lens.Lens' GetConsoleOutput (Core.Maybe Core.Bool)
gcoLatest = Lens.field @"latest"
{-# INLINEABLE gcoLatest #-}
{-# DEPRECATED latest "Use generic-lens or generic-optics with 'latest' instead"  #-}

instance Core.ToQuery GetConsoleOutput where
        toQuery GetConsoleOutput{..}
          = Core.toQueryPair "Action" ("GetConsoleOutput" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Latest") latest

instance Core.ToHeaders GetConsoleOutput where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetConsoleOutput where
        type Rs GetConsoleOutput = GetConsoleOutputResponse
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
                 GetConsoleOutputResponse' Core.<$>
                   (x Core..@? "instanceId") Core.<*> x Core..@? "output" Core.<*>
                     x Core..@? "timestamp"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConsoleOutputResponse' smart constructor.
data GetConsoleOutputResponse = GetConsoleOutputResponse'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , output :: Core.Maybe Core.Text
    -- ^ The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
  , timestamp :: Core.Maybe Core.UTCTime
    -- ^ The time at which the output was last updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetConsoleOutputResponse' value with any optional fields omitted.
mkGetConsoleOutputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConsoleOutputResponse
mkGetConsoleOutputResponse responseStatus
  = GetConsoleOutputResponse'{instanceId = Core.Nothing,
                              output = Core.Nothing, timestamp = Core.Nothing, responseStatus}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorrsInstanceId :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.Text)
gcorrsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gcorrsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorrsOutput :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.Text)
gcorrsOutput = Lens.field @"output"
{-# INLINEABLE gcorrsOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | The time at which the output was last updated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorrsTimestamp :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.UTCTime)
gcorrsTimestamp = Lens.field @"timestamp"
{-# INLINEABLE gcorrsTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorrsResponseStatus :: Lens.Lens' GetConsoleOutputResponse Core.Int
gcorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
