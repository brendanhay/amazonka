{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RebootInstances (..),
    mkRebootInstances,

    -- ** Request lenses
    rInstanceIds,
    rDryRun,

    -- * Destructuring the response
    RebootInstancesResponse (..),
    mkRebootInstancesResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootInstances' smart constructor.
data RebootInstances = RebootInstances'
  { -- | The instance IDs.
    instanceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The instance IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRebootInstances ::
  RebootInstances
mkRebootInstances =
  RebootInstances'
    { instanceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | The instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceIds :: Lens.Lens' RebootInstances [Lude.Text]
rInstanceIds = Lens.lens (instanceIds :: RebootInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: RebootInstances)
{-# DEPRECATED rInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDryRun :: Lens.Lens' RebootInstances (Lude.Maybe Lude.Bool)
rDryRun = Lens.lens (dryRun :: RebootInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RebootInstances)
{-# DEPRECATED rDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RebootInstances where
  type Rs RebootInstances = RebootInstancesResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull RebootInstancesResponse'

instance Lude.ToHeaders RebootInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebootInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootInstances where
  toQuery RebootInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RebootInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "InstanceId" instanceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootInstancesResponse' with the minimum fields required to make a request.
mkRebootInstancesResponse ::
  RebootInstancesResponse
mkRebootInstancesResponse = RebootInstancesResponse'
