{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetConsoleOutput (..),
    mkGetConsoleOutput,

    -- ** Request lenses
    gcoInstanceId,
    gcoLatest,
    gcoDryRun,

    -- * Destructuring the response
    GetConsoleOutputResponse (..),
    mkGetConsoleOutputResponse,

    -- ** Response lenses
    gcorsInstanceId,
    gcorsOutput,
    gcorsTimestamp,
    gcorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConsoleOutput' smart constructor.
data GetConsoleOutput = GetConsoleOutput'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | When enabled, retrieves the latest console output for the instance.
    --
    -- Default: disabled (@false@ )
    latest :: Lude.Maybe Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConsoleOutput' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'latest' - When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@ )
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetConsoleOutput ::
  -- | 'instanceId'
  Lude.Text ->
  GetConsoleOutput
mkGetConsoleOutput pInstanceId_ =
  GetConsoleOutput'
    { instanceId = pInstanceId_,
      latest = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoInstanceId :: Lens.Lens' GetConsoleOutput Lude.Text
gcoInstanceId = Lens.lens (instanceId :: GetConsoleOutput -> Lude.Text) (\s a -> s {instanceId = a} :: GetConsoleOutput)
{-# DEPRECATED gcoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@ )
--
-- /Note:/ Consider using 'latest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoLatest :: Lens.Lens' GetConsoleOutput (Lude.Maybe Lude.Bool)
gcoLatest = Lens.lens (latest :: GetConsoleOutput -> Lude.Maybe Lude.Bool) (\s a -> s {latest = a} :: GetConsoleOutput)
{-# DEPRECATED gcoLatest "Use generic-lens or generic-optics with 'latest' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcoDryRun :: Lens.Lens' GetConsoleOutput (Lude.Maybe Lude.Bool)
gcoDryRun = Lens.lens (dryRun :: GetConsoleOutput -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetConsoleOutput)
{-# DEPRECATED gcoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetConsoleOutput where
  type Rs GetConsoleOutput = GetConsoleOutputResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetConsoleOutputResponse'
            Lude.<$> (x Lude..@? "instanceId")
            Lude.<*> (x Lude..@? "output")
            Lude.<*> (x Lude..@? "timestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConsoleOutput where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetConsoleOutput where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConsoleOutput where
  toQuery GetConsoleOutput' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetConsoleOutput" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "Latest" Lude.=: latest,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetConsoleOutputResponse' smart constructor.
data GetConsoleOutputResponse = GetConsoleOutputResponse'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
    output :: Lude.Maybe Lude.Text,
    -- | The time at which the output was last updated.
    timestamp :: Lude.Maybe Lude.DateTime,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConsoleOutputResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'output' - The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
-- * 'timestamp' - The time at which the output was last updated.
-- * 'responseStatus' - The response status code.
mkGetConsoleOutputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConsoleOutputResponse
mkGetConsoleOutputResponse pResponseStatus_ =
  GetConsoleOutputResponse'
    { instanceId = Lude.Nothing,
      output = Lude.Nothing,
      timestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsInstanceId :: Lens.Lens' GetConsoleOutputResponse (Lude.Maybe Lude.Text)
gcorsInstanceId = Lens.lens (instanceId :: GetConsoleOutputResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GetConsoleOutputResponse)
{-# DEPRECATED gcorsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsOutput :: Lens.Lens' GetConsoleOutputResponse (Lude.Maybe Lude.Text)
gcorsOutput = Lens.lens (output :: GetConsoleOutputResponse -> Lude.Maybe Lude.Text) (\s a -> s {output = a} :: GetConsoleOutputResponse)
{-# DEPRECATED gcorsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The time at which the output was last updated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsTimestamp :: Lens.Lens' GetConsoleOutputResponse (Lude.Maybe Lude.DateTime)
gcorsTimestamp = Lens.lens (timestamp :: GetConsoleOutputResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: GetConsoleOutputResponse)
{-# DEPRECATED gcorsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsResponseStatus :: Lens.Lens' GetConsoleOutputResponse Lude.Int
gcorsResponseStatus = Lens.lens (responseStatus :: GetConsoleOutputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConsoleOutputResponse)
{-# DEPRECATED gcorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
