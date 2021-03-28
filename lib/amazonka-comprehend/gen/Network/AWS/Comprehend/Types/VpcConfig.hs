{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.VpcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.VpcConfig
  ( VpcConfig (..)
  -- * Smart constructor
  , mkVpcConfig
  -- * Lenses
  , vcSecurityGroupIds
  , vcSubnets
  ) where

import qualified Network.AWS.Comprehend.Types.SecurityGroupId as Types
import qualified Network.AWS.Comprehend.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for the job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /See:/ 'mkVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { securityGroupIds :: Core.NonEmpty Types.SecurityGroupId
    -- ^ The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> . 
  , subnets :: Core.NonEmpty Types.SubnetId
    -- ^ The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfig' value with any optional fields omitted.
mkVpcConfig
    :: Core.NonEmpty Types.SecurityGroupId -- ^ 'securityGroupIds'
    -> Core.NonEmpty Types.SubnetId -- ^ 'subnets'
    -> VpcConfig
mkVpcConfig securityGroupIds subnets
  = VpcConfig'{securityGroupIds, subnets}

-- | The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> . 
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VpcConfig (Core.NonEmpty Types.SecurityGroupId)
vcSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> . 
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnets :: Lens.Lens' VpcConfig (Core.NonEmpty Types.SubnetId)
vcSubnets = Lens.field @"subnets"
{-# INLINEABLE vcSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

instance Core.FromJSON VpcConfig where
        toJSON VpcConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecurityGroupIds" Core..= securityGroupIds),
                  Core.Just ("Subnets" Core..= subnets)])

instance Core.FromJSON VpcConfig where
        parseJSON
          = Core.withObject "VpcConfig" Core.$
              \ x ->
                VpcConfig' Core.<$>
                  (x Core..: "SecurityGroupIds") Core.<*> x Core..: "Subnets"
