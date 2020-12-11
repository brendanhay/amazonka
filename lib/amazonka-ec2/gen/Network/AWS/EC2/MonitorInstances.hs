{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables detailed monitoring for a running instance. Otherwise, basic monitoring is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- To disable detailed monitoring, see .
module Network.AWS.EC2.MonitorInstances
  ( -- * Creating a request
    MonitorInstances (..),
    mkMonitorInstances,

    -- ** Request lenses
    miDryRun,
    miInstanceIds,

    -- * Destructuring the response
    MonitorInstancesResponse (..),
    mkMonitorInstancesResponse,

    -- ** Response lenses
    mirsInstanceMonitorings,
    mirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMonitorInstances' smart constructor.
data MonitorInstances = MonitorInstances'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitorInstances' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceIds' - The IDs of the instances.
mkMonitorInstances ::
  MonitorInstances
mkMonitorInstances =
  MonitorInstances'
    { dryRun = Lude.Nothing,
      instanceIds = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miDryRun :: Lens.Lens' MonitorInstances (Lude.Maybe Lude.Bool)
miDryRun = Lens.lens (dryRun :: MonitorInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: MonitorInstances)
{-# DEPRECATED miDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miInstanceIds :: Lens.Lens' MonitorInstances [Lude.Text]
miInstanceIds = Lens.lens (instanceIds :: MonitorInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: MonitorInstances)
{-# DEPRECATED miInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Lude.AWSRequest MonitorInstances where
  type Rs MonitorInstances = MonitorInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          MonitorInstancesResponse'
            Lude.<$> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MonitorInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath MonitorInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery MonitorInstances where
  toQuery MonitorInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("MonitorInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'mkMonitorInstancesResponse' smart constructor.
data MonitorInstancesResponse = MonitorInstancesResponse'
  { instanceMonitorings ::
      Lude.Maybe [InstanceMonitoring],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitorInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instanceMonitorings' - The monitoring information.
-- * 'responseStatus' - The response status code.
mkMonitorInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MonitorInstancesResponse
mkMonitorInstancesResponse pResponseStatus_ =
  MonitorInstancesResponse'
    { instanceMonitorings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The monitoring information.
--
-- /Note:/ Consider using 'instanceMonitorings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mirsInstanceMonitorings :: Lens.Lens' MonitorInstancesResponse (Lude.Maybe [InstanceMonitoring])
mirsInstanceMonitorings = Lens.lens (instanceMonitorings :: MonitorInstancesResponse -> Lude.Maybe [InstanceMonitoring]) (\s a -> s {instanceMonitorings = a} :: MonitorInstancesResponse)
{-# DEPRECATED mirsInstanceMonitorings "Use generic-lens or generic-optics with 'instanceMonitorings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mirsResponseStatus :: Lens.Lens' MonitorInstancesResponse Lude.Int
mirsResponseStatus = Lens.lens (responseStatus :: MonitorInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MonitorInstancesResponse)
{-# DEPRECATED mirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
