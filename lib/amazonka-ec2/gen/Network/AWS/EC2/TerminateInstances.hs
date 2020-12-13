{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down the specified instances. This operation is idempotent; if you terminate an instance more than once, each call succeeds.
--
-- If you specify multiple instances and the request fails (for example, because of a single incorrect instance ID), none of the instances are terminated.
-- Terminated instances remain visible after termination (for approximately one hour).
-- By default, Amazon EC2 deletes all EBS volumes that were attached when the instance launched. Volumes attached after instance launch continue running.
-- You can stop, start, and terminate EBS-backed instances. You can only terminate instance store-backed instances. What happens to an instance differs if you stop it or terminate it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, any attached EBS volumes with the @DeleteOnTermination@ block device mapping parameter set to @true@ are automatically deleted. For more information about the differences between stopping and terminating instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
-- For more information about troubleshooting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html Troubleshooting terminating your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.TerminateInstances
  ( -- * Creating a request
    TerminateInstances (..),
    mkTerminateInstances,

    -- ** Request lenses
    tiInstanceIds,
    tiDryRun,

    -- * Destructuring the response
    TerminateInstancesResponse (..),
    mkTerminateInstancesResponse,

    -- ** Response lenses
    tirsTerminatingInstances,
    tirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTerminateInstances' smart constructor.
data TerminateInstances = TerminateInstances'
  { -- | The IDs of the instances.
    --
    -- Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
    instanceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances.
--
-- Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkTerminateInstances ::
  TerminateInstances
mkTerminateInstances =
  TerminateInstances'
    { instanceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | The IDs of the instances.
--
-- Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiInstanceIds :: Lens.Lens' TerminateInstances [Lude.Text]
tiInstanceIds = Lens.lens (instanceIds :: TerminateInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: TerminateInstances)
{-# DEPRECATED tiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDryRun :: Lens.Lens' TerminateInstances (Lude.Maybe Lude.Bool)
tiDryRun = Lens.lens (dryRun :: TerminateInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: TerminateInstances)
{-# DEPRECATED tiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest TerminateInstances where
  type Rs TerminateInstances = TerminateInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          TerminateInstancesResponse'
            Lude.<$> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TerminateInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateInstances where
  toQuery TerminateInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TerminateInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "InstanceId" instanceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkTerminateInstancesResponse' smart constructor.
data TerminateInstancesResponse = TerminateInstancesResponse'
  { -- | Information about the terminated instances.
    terminatingInstances :: Lude.Maybe [InstanceStateChange],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateInstancesResponse' with the minimum fields required to make a request.
--
-- * 'terminatingInstances' - Information about the terminated instances.
-- * 'responseStatus' - The response status code.
mkTerminateInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateInstancesResponse
mkTerminateInstancesResponse pResponseStatus_ =
  TerminateInstancesResponse'
    { terminatingInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the terminated instances.
--
-- /Note:/ Consider using 'terminatingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tirsTerminatingInstances :: Lens.Lens' TerminateInstancesResponse (Lude.Maybe [InstanceStateChange])
tirsTerminatingInstances = Lens.lens (terminatingInstances :: TerminateInstancesResponse -> Lude.Maybe [InstanceStateChange]) (\s a -> s {terminatingInstances = a} :: TerminateInstancesResponse)
{-# DEPRECATED tirsTerminatingInstances "Use generic-lens or generic-optics with 'terminatingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tirsResponseStatus :: Lens.Lens' TerminateInstancesResponse Lude.Int
tirsResponseStatus = Lens.lens (responseStatus :: TerminateInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateInstancesResponse)
{-# DEPRECATED tirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
