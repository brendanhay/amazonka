{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VpcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.VpcConfig
  ( VpcConfig (..)
  -- * Smart constructor
  , mkVpcConfig
  -- * Lenses
  , vcSecurityGroupIds
  , vcSubnets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SecurityGroupId as Types
import qualified Network.AWS.SageMaker.Types.SubnetId as Types

-- | Specifies a VPC that your training jobs and hosted models have access to. Control access to and from your training and model containers by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> . 
--
-- /See:/ 'mkVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { securityGroupIds :: Core.NonEmpty Types.SecurityGroupId
    -- ^ The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
  , subnets :: Core.NonEmpty Types.SubnetId
    -- ^ The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
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

-- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VpcConfig (Core.NonEmpty Types.SecurityGroupId)
vcSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
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
