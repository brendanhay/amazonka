{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.VpcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.VpcConfig
  ( VpcConfig (..)
  -- * Smart constructor
  , mkVpcConfig
  -- * Lenses
  , vcSecurityGroupIds
  , vcSubnets
  , vcVpcId
  ) where

import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
-- /See:/ 'mkVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { securityGroupIds :: Core.Maybe [Types.NonEmptyString]
    -- ^ A list of one or more security groups IDs in your Amazon VPC.
  , subnets :: Core.Maybe [Types.NonEmptyString]
    -- ^ A list of one or more subnet IDs in your Amazon VPC.
  , vpcId :: Core.Maybe Types.NonEmptyString
    -- ^ The ID of the Amazon VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfig' value with any optional fields omitted.
mkVpcConfig
    :: VpcConfig
mkVpcConfig
  = VpcConfig'{securityGroupIds = Core.Nothing,
               subnets = Core.Nothing, vpcId = Core.Nothing}

-- | A list of one or more security groups IDs in your Amazon VPC.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VpcConfig (Core.Maybe [Types.NonEmptyString])
vcSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | A list of one or more subnet IDs in your Amazon VPC.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnets :: Lens.Lens' VpcConfig (Core.Maybe [Types.NonEmptyString])
vcSubnets = Lens.field @"subnets"
{-# INLINEABLE vcSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The ID of the Amazon VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVpcId :: Lens.Lens' VpcConfig (Core.Maybe Types.NonEmptyString)
vcVpcId = Lens.field @"vpcId"
{-# INLINEABLE vcVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON VpcConfig where
        toJSON VpcConfig{..}
          = Core.object
              (Core.catMaybes
                 [("securityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("subnets" Core..=) Core.<$> subnets,
                  ("vpcId" Core..=) Core.<$> vpcId])

instance Core.FromJSON VpcConfig where
        parseJSON
          = Core.withObject "VpcConfig" Core.$
              \ x ->
                VpcConfig' Core.<$>
                  (x Core..:? "securityGroupIds") Core.<*> x Core..:? "subnets"
                    Core.<*> x Core..:? "vpcId"
