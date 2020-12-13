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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendDiagnosticInterrupt' smart constructor.
data SendDiagnosticInterrupt = SendDiagnosticInterrupt'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendDiagnosticInterrupt' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkSendDiagnosticInterrupt ::
  -- | 'instanceId'
  Lude.Text ->
  SendDiagnosticInterrupt
mkSendDiagnosticInterrupt pInstanceId_ =
  SendDiagnosticInterrupt'
    { instanceId = pInstanceId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiInstanceId :: Lens.Lens' SendDiagnosticInterrupt Lude.Text
sdiInstanceId = Lens.lens (instanceId :: SendDiagnosticInterrupt -> Lude.Text) (\s a -> s {instanceId = a} :: SendDiagnosticInterrupt)
{-# DEPRECATED sdiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiDryRun :: Lens.Lens' SendDiagnosticInterrupt (Lude.Maybe Lude.Bool)
sdiDryRun = Lens.lens (dryRun :: SendDiagnosticInterrupt -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: SendDiagnosticInterrupt)
{-# DEPRECATED sdiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest SendDiagnosticInterrupt where
  type Rs SendDiagnosticInterrupt = SendDiagnosticInterruptResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull SendDiagnosticInterruptResponse'

instance Lude.ToHeaders SendDiagnosticInterrupt where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendDiagnosticInterrupt where
  toPath = Lude.const "/"

instance Lude.ToQuery SendDiagnosticInterrupt where
  toQuery SendDiagnosticInterrupt' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendDiagnosticInterrupt" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkSendDiagnosticInterruptResponse' smart constructor.
data SendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendDiagnosticInterruptResponse' with the minimum fields required to make a request.
mkSendDiagnosticInterruptResponse ::
  SendDiagnosticInterruptResponse
mkSendDiagnosticInterruptResponse =
  SendDiagnosticInterruptResponse'
