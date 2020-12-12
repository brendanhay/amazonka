{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.VPCConfig
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

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for the job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /See:/ 'mkVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { securityGroupIds ::
      Lude.NonEmpty Lude.Text,
    subnets :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> .
-- * 'subnets' - The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> .
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

-- | The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VPCConfig (Lude.NonEmpty Lude.Text)
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfig -> Lude.NonEmpty Lude.Text) (\s a -> s {securityGroupIds = a} :: VPCConfig)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> .
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
