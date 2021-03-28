{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SendDiagnosticInterrupt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a diagnostic interrupt to the specified Amazon EC2 instance to trigger a /kernel panic/ (on Linux instances), or a /blue screen/ //stop error/ (on Windows instances). For instances based on Intel and AMD processors, the interrupt is received as a /non-maskable interrupt/ (NMI).
--
-- In general, the operating system crashes and reboots when a kernel panic or stop error is triggered. The operating system can also be configured to perform diagnostic tasks, such as generating a memory dump file, loading a secondary kernel, or obtaining a call trace.
-- Before sending a diagnostic interrupt to your instance, ensure that its operating system is configured to perform the required diagnostic tasks.
-- For more information about configuring your operating system to generate a crash dump when a kernel panic or stop error occurs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/diagnostic-interrupt.html Send a diagnostic interrupt> (Linux instances) or <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/diagnostic-interrupt.html Send a Diagnostic Interrupt> (Windows instances).
module Network.AWS.EC2.SendDiagnosticInterrupt
    (
    -- * Creating a request
      SendDiagnosticInterrupt (..)
    , mkSendDiagnosticInterrupt
    -- ** Request lenses
    , sdiInstanceId
    , sdiDryRun

    -- * Destructuring the response
    , SendDiagnosticInterruptResponse (..)
    , mkSendDiagnosticInterruptResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendDiagnosticInterrupt' smart constructor.
data SendDiagnosticInterrupt = SendDiagnosticInterrupt'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendDiagnosticInterrupt' value with any optional fields omitted.
mkSendDiagnosticInterrupt
    :: Types.InstanceId -- ^ 'instanceId'
    -> SendDiagnosticInterrupt
mkSendDiagnosticInterrupt instanceId
  = SendDiagnosticInterrupt'{instanceId, dryRun = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiInstanceId :: Lens.Lens' SendDiagnosticInterrupt Types.InstanceId
sdiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sdiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiDryRun :: Lens.Lens' SendDiagnosticInterrupt (Core.Maybe Core.Bool)
sdiDryRun = Lens.field @"dryRun"
{-# INLINEABLE sdiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery SendDiagnosticInterrupt where
        toQuery SendDiagnosticInterrupt{..}
          = Core.toQueryPair "Action"
              ("SendDiagnosticInterrupt" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders SendDiagnosticInterrupt where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SendDiagnosticInterrupt where
        type Rs SendDiagnosticInterrupt = SendDiagnosticInterruptResponse
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
          = Response.receiveNull SendDiagnosticInterruptResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendDiagnosticInterruptResponse' smart constructor.
data SendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendDiagnosticInterruptResponse' value with any optional fields omitted.
mkSendDiagnosticInterruptResponse
    :: SendDiagnosticInterruptResponse
mkSendDiagnosticInterruptResponse
  = SendDiagnosticInterruptResponse'
