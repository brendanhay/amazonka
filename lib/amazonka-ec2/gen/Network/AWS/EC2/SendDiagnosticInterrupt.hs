{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SendDiagnosticInterrupt (..),
    mkSendDiagnosticInterrupt,

    -- ** Request lenses
    sdiInstanceId,
    sdiDryRun,

    -- * Destructuring the response
    SendDiagnosticInterruptResponse (..),
    mkSendDiagnosticInterruptResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendDiagnosticInterrupt' smart constructor.
data SendDiagnosticInterrupt = SendDiagnosticInterrupt'
  { -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendDiagnosticInterrupt' value with any optional fields omitted.
mkSendDiagnosticInterrupt ::
  -- | 'instanceId'
  Types.InstanceId ->
  SendDiagnosticInterrupt
mkSendDiagnosticInterrupt instanceId =
  SendDiagnosticInterrupt' {instanceId, dryRun = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiInstanceId :: Lens.Lens' SendDiagnosticInterrupt Types.InstanceId
sdiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED sdiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiDryRun :: Lens.Lens' SendDiagnosticInterrupt (Core.Maybe Core.Bool)
sdiDryRun = Lens.field @"dryRun"
{-# DEPRECATED sdiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest SendDiagnosticInterrupt where
  type Rs SendDiagnosticInterrupt = SendDiagnosticInterruptResponse
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
            ( Core.pure ("Action", "SendDiagnosticInterrupt")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull SendDiagnosticInterruptResponse'

-- | /See:/ 'mkSendDiagnosticInterruptResponse' smart constructor.
data SendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendDiagnosticInterruptResponse' value with any optional fields omitted.
mkSendDiagnosticInterruptResponse ::
  SendDiagnosticInterruptResponse
mkSendDiagnosticInterruptResponse =
  SendDiagnosticInterruptResponse'
