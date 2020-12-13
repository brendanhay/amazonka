{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UnmonitorInstances (..),
    mkUnmonitorInstances,

    -- ** Request lenses
    uiInstanceIds,
    uiDryRun,

    -- * Destructuring the response
    UnmonitorInstancesResponse (..),
    mkUnmonitorInstancesResponse,

    -- ** Response lenses
    uirsInstanceMonitorings,
    uirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnmonitorInstances' smart constructor.
data UnmonitorInstances = UnmonitorInstances'
  { -- | The IDs of the instances.
    instanceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnmonitorInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkUnmonitorInstances ::
  UnmonitorInstances
mkUnmonitorInstances =
  UnmonitorInstances'
    { instanceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstanceIds :: Lens.Lens' UnmonitorInstances [Lude.Text]
uiInstanceIds = Lens.lens (instanceIds :: UnmonitorInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: UnmonitorInstances)
{-# DEPRECATED uiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDryRun :: Lens.Lens' UnmonitorInstances (Lude.Maybe Lude.Bool)
uiDryRun = Lens.lens (dryRun :: UnmonitorInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UnmonitorInstances)
{-# DEPRECATED uiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest UnmonitorInstances where
  type Rs UnmonitorInstances = UnmonitorInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          UnmonitorInstancesResponse'
            Lude.<$> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnmonitorInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UnmonitorInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery UnmonitorInstances where
  toQuery UnmonitorInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UnmonitorInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "InstanceId" instanceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkUnmonitorInstancesResponse' smart constructor.
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
  { -- | The monitoring information.
    instanceMonitorings :: Lude.Maybe [InstanceMonitoring],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnmonitorInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instanceMonitorings' - The monitoring information.
-- * 'responseStatus' - The response status code.
mkUnmonitorInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnmonitorInstancesResponse
mkUnmonitorInstancesResponse pResponseStatus_ =
  UnmonitorInstancesResponse'
    { instanceMonitorings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The monitoring information.
--
-- /Note:/ Consider using 'instanceMonitorings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsInstanceMonitorings :: Lens.Lens' UnmonitorInstancesResponse (Lude.Maybe [InstanceMonitoring])
uirsInstanceMonitorings = Lens.lens (instanceMonitorings :: UnmonitorInstancesResponse -> Lude.Maybe [InstanceMonitoring]) (\s a -> s {instanceMonitorings = a} :: UnmonitorInstancesResponse)
{-# DEPRECATED uirsInstanceMonitorings "Use generic-lens or generic-optics with 'instanceMonitorings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsResponseStatus :: Lens.Lens' UnmonitorInstancesResponse Lude.Int
uirsResponseStatus = Lens.lens (responseStatus :: UnmonitorInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnmonitorInstancesResponse)
{-# DEPRECATED uirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
