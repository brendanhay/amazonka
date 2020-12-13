{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon EBS-backed instance that you've previously stopped.
--
-- Instances that use Amazon EBS volumes as their root devices can be quickly stopped and started. When an instance is stopped, the compute resources are released and you are not billed for instance usage. However, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. You can restart your instance at any time. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
-- Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.
-- Performing this operation on an instance that uses an instance store as its root device returns an error.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stopping instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.StartInstances
  ( -- * Creating a request
    StartInstances (..),
    mkStartInstances,

    -- ** Request lenses
    siAdditionalInfo,
    siInstanceIds,
    siDryRun,

    -- * Destructuring the response
    StartInstancesResponse (..),
    mkStartInstancesResponse,

    -- ** Response lenses
    srsStartingInstances,
    srsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartInstances' smart constructor.
data StartInstances = StartInstances'
  { -- | Reserved.
    additionalInfo :: Lude.Maybe Lude.Text,
    -- | The IDs of the instances.
    instanceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstances' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Reserved.
-- * 'instanceIds' - The IDs of the instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkStartInstances ::
  StartInstances
mkStartInstances =
  StartInstances'
    { additionalInfo = Lude.Nothing,
      instanceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAdditionalInfo :: Lens.Lens' StartInstances (Lude.Maybe Lude.Text)
siAdditionalInfo = Lens.lens (additionalInfo :: StartInstances -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: StartInstances)
{-# DEPRECATED siAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceIds :: Lens.Lens' StartInstances [Lude.Text]
siInstanceIds = Lens.lens (instanceIds :: StartInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: StartInstances)
{-# DEPRECATED siInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDryRun :: Lens.Lens' StartInstances (Lude.Maybe Lude.Bool)
siDryRun = Lens.lens (dryRun :: StartInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: StartInstances)
{-# DEPRECATED siDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest StartInstances where
  type Rs StartInstances = StartInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          StartInstancesResponse'
            Lude.<$> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery StartInstances where
  toQuery StartInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AdditionalInfo" Lude.=: additionalInfo,
        Lude.toQueryList "InstanceId" instanceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkStartInstancesResponse' smart constructor.
data StartInstancesResponse = StartInstancesResponse'
  { -- | Information about the started instances.
    startingInstances :: Lude.Maybe [InstanceStateChange],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstancesResponse' with the minimum fields required to make a request.
--
-- * 'startingInstances' - Information about the started instances.
-- * 'responseStatus' - The response status code.
mkStartInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartInstancesResponse
mkStartInstancesResponse pResponseStatus_ =
  StartInstancesResponse'
    { startingInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the started instances.
--
-- /Note:/ Consider using 'startingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStartingInstances :: Lens.Lens' StartInstancesResponse (Lude.Maybe [InstanceStateChange])
srsStartingInstances = Lens.lens (startingInstances :: StartInstancesResponse -> Lude.Maybe [InstanceStateChange]) (\s a -> s {startingInstances = a} :: StartInstancesResponse)
{-# DEPRECATED srsStartingInstances "Use generic-lens or generic-optics with 'startingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartInstancesResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartInstancesResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
