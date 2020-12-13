{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon EBS-backed instance.
--
-- You can use the Stop action to hibernate an instance if the instance is <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#enabling-hibernation enabled for hibernation> and it meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
-- We don't charge usage for a stopped instance, or data transfer fees; however, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
-- You can't stop or hibernate instance store-backed instances. You can't use the Stop action to hibernate Spot Instances, but you can specify that Amazon EC2 should hibernate Spot Instances when they are interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-interruptions.html#hibernate-spot-instances Hibernating interrupted Spot Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
-- When you stop or hibernate an instance, we shut it down. You can restart your instance at any time. Before stopping or hibernating an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM, but hibernating an instance does preserve data stored in RAM. If an instance cannot hibernate successfully, a normal shutdown occurs.
-- Stopping and hibernating an instance is different to rebooting or terminating it. For example, when you stop or hibernate an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, the root device and any other devices attached during the instance launch are automatically deleted. For more information about the differences between rebooting, stopping, hibernating, and terminating instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
-- When you stop an instance, we attempt to shut it down forcibly after a short while. If your instance appears stuck in the stopping state after a period of time, there may be an issue with the underlying host computer. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html Troubleshooting stopping your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.StopInstances
  ( -- * Creating a request
    StopInstances (..),
    mkStopInstances,

    -- ** Request lenses
    sHibernate,
    sForce,
    sInstanceIds,
    sDryRun,

    -- * Destructuring the response
    StopInstancesResponse (..),
    mkStopInstancesResponse,

    -- ** Response lenses
    sirsStoppingInstances,
    sirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopInstances' smart constructor.
data StopInstances = StopInstances'
  { -- | Hibernates the instance if the instance was enabled for hibernation at launch. If the instance cannot hibernate successfully, a normal shutdown occurs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Default: @false@
    hibernate :: Lude.Maybe Lude.Bool,
    -- | Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances.
    --
    -- Default: @false@
    force :: Lude.Maybe Lude.Bool,
    -- | The IDs of the instances.
    instanceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopInstances' with the minimum fields required to make a request.
--
-- * 'hibernate' - Hibernates the instance if the instance was enabled for hibernation at launch. If the instance cannot hibernate successfully, a normal shutdown occurs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @false@
-- * 'force' - Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances.
--
-- Default: @false@
-- * 'instanceIds' - The IDs of the instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkStopInstances ::
  StopInstances
mkStopInstances =
  StopInstances'
    { hibernate = Lude.Nothing,
      force = Lude.Nothing,
      instanceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | Hibernates the instance if the instance was enabled for hibernation at launch. If the instance cannot hibernate successfully, a normal shutdown occurs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @false@
--
-- /Note:/ Consider using 'hibernate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHibernate :: Lens.Lens' StopInstances (Lude.Maybe Lude.Bool)
sHibernate = Lens.lens (hibernate :: StopInstances -> Lude.Maybe Lude.Bool) (\s a -> s {hibernate = a} :: StopInstances)
{-# DEPRECATED sHibernate "Use generic-lens or generic-optics with 'hibernate' instead." #-}

-- | Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances.
--
-- Default: @false@
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sForce :: Lens.Lens' StopInstances (Lude.Maybe Lude.Bool)
sForce = Lens.lens (force :: StopInstances -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: StopInstances)
{-# DEPRECATED sForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceIds :: Lens.Lens' StopInstances [Lude.Text]
sInstanceIds = Lens.lens (instanceIds :: StopInstances -> [Lude.Text]) (\s a -> s {instanceIds = a} :: StopInstances)
{-# DEPRECATED sInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDryRun :: Lens.Lens' StopInstances (Lude.Maybe Lude.Bool)
sDryRun = Lens.lens (dryRun :: StopInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: StopInstances)
{-# DEPRECATED sDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest StopInstances where
  type Rs StopInstances = StopInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          StopInstancesResponse'
            Lude.<$> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StopInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery StopInstances where
  toQuery StopInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StopInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Hibernate" Lude.=: hibernate,
        "Force" Lude.=: force,
        Lude.toQueryList "InstanceId" instanceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkStopInstancesResponse' smart constructor.
data StopInstancesResponse = StopInstancesResponse'
  { -- | Information about the stopped instances.
    stoppingInstances :: Lude.Maybe [InstanceStateChange],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopInstancesResponse' with the minimum fields required to make a request.
--
-- * 'stoppingInstances' - Information about the stopped instances.
-- * 'responseStatus' - The response status code.
mkStopInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopInstancesResponse
mkStopInstancesResponse pResponseStatus_ =
  StopInstancesResponse'
    { stoppingInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the stopped instances.
--
-- /Note:/ Consider using 'stoppingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsStoppingInstances :: Lens.Lens' StopInstancesResponse (Lude.Maybe [InstanceStateChange])
sirsStoppingInstances = Lens.lens (stoppingInstances :: StopInstancesResponse -> Lude.Maybe [InstanceStateChange]) (\s a -> s {stoppingInstances = a} :: StopInstancesResponse)
{-# DEPRECATED sirsStoppingInstances "Use generic-lens or generic-optics with 'stoppingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResponseStatus :: Lens.Lens' StopInstancesResponse Lude.Int
sirsResponseStatus = Lens.lens (responseStatus :: StopInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopInstancesResponse)
{-# DEPRECATED sirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
