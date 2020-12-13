{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VPCConfig
  ( VPCConfig (..),

    -- * Smart constructor
    mkVPCConfig,

    -- * Lenses
    vcSecurityGroupIds,
    vcSubnets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a VPC that your training jobs and hosted models have access to. Control access to and from your training and model containers by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /See:/ 'mkVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { -- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
    securityGroupIds :: Lude.NonEmpty Lude.Text,
    -- | The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
    subnets :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
-- * 'subnets' - The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
mkVPCConfig ::
  -- | 'securityGroupIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'subnets'
  Lude.NonEmpty Lude.Text ->
  VPCConfig
mkVPCConfig pSecurityGroupIds_ pSubnets_ =
  VPCConfig'
    { securityGroupIds = pSecurityGroupIds_,
      subnets = pSubnets_
    }

-- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VPCConfig (Lude.NonEmpty Lude.Text)
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfig -> Lude.NonEmpty Lude.Text) (\s a -> s {securityGroupIds = a} :: VPCConfig)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnets :: Lens.Lens' VPCConfig (Lude.NonEmpty Lude.Text)
vcSubnets = Lens.lens (subnets :: VPCConfig -> Lude.NonEmpty Lude.Text) (\s a -> s {subnets = a} :: VPCConfig)
{-# DEPRECATED vcSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Lude.FromJSON VPCConfig where
  parseJSON =
    Lude.withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            Lude.<$> (x Lude..: "SecurityGroupIds") Lude.<*> (x Lude..: "Subnets")
      )

instance Lude.ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SecurityGroupIds" Lude..= securityGroupIds),
            Lude.Just ("Subnets" Lude..= subnets)
          ]
      )
