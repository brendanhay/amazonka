-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration
  ( AWSVPCConfiguration (..),

    -- * Smart constructor
    mkAWSVPCConfiguration,

    -- * Lenses
    avcSecurityGroups,
    avcAssignPublicIP,
    avcSubnets,
  )
where

import Network.AWS.CloudWatchEvents.Types.AssignPublicIP
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure specifies the VPC subnets and security groups for the task, and whether a public IP address is to be used. This structure is relevant only for ECS tasks that use the @awsvpc@ network mode.
--
-- /See:/ 'mkAWSVPCConfiguration' smart constructor.
data AWSVPCConfiguration = AWSVPCConfiguration'
  { securityGroups ::
      Lude.Maybe [Lude.Text],
    assignPublicIP :: Lude.Maybe AssignPublicIP,
    subnets :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSVPCConfiguration' with the minimum fields required to make a request.
--
-- * 'assignPublicIP' - Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
-- * 'securityGroups' - Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
-- * 'subnets' - Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
mkAWSVPCConfiguration ::
  AWSVPCConfiguration
mkAWSVPCConfiguration =
  AWSVPCConfiguration'
    { securityGroups = Lude.Nothing,
      assignPublicIP = Lude.Nothing,
      subnets = Lude.mempty
    }

-- | Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSecurityGroups :: Lens.Lens' AWSVPCConfiguration (Lude.Maybe [Lude.Text])
avcSecurityGroups = Lens.lens (securityGroups :: AWSVPCConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: AWSVPCConfiguration)
{-# DEPRECATED avcSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
--
-- /Note:/ Consider using 'assignPublicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcAssignPublicIP :: Lens.Lens' AWSVPCConfiguration (Lude.Maybe AssignPublicIP)
avcAssignPublicIP = Lens.lens (assignPublicIP :: AWSVPCConfiguration -> Lude.Maybe AssignPublicIP) (\s a -> s {assignPublicIP = a} :: AWSVPCConfiguration)
{-# DEPRECATED avcAssignPublicIP "Use generic-lens or generic-optics with 'assignPublicIP' instead." #-}

-- | Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSubnets :: Lens.Lens' AWSVPCConfiguration [Lude.Text]
avcSubnets = Lens.lens (subnets :: AWSVPCConfiguration -> [Lude.Text]) (\s a -> s {subnets = a} :: AWSVPCConfiguration)
{-# DEPRECATED avcSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Lude.FromJSON AWSVPCConfiguration where
  parseJSON =
    Lude.withObject
      "AWSVPCConfiguration"
      ( \x ->
          AWSVPCConfiguration'
            Lude.<$> (x Lude..:? "SecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AssignPublicIp")
            Lude.<*> (x Lude..:? "Subnets" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AWSVPCConfiguration where
  toJSON AWSVPCConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroups" Lude..=) Lude.<$> securityGroups,
            ("AssignPublicIp" Lude..=) Lude.<$> assignPublicIP,
            Lude.Just ("Subnets" Lude..= subnets)
          ]
      )
