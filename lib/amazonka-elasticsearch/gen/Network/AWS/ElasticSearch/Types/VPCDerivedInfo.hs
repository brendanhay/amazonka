{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.VPCDerivedInfo
  ( VPCDerivedInfo (..)
  -- * Smart constructor
  , mkVPCDerivedInfo
  -- * Lenses
  , vpcdiAvailabilityZones
  , vpcdiSecurityGroupIds
  , vpcdiSubnetIds
  , vpcdiVPCId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /See:/ 'mkVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { availabilityZones :: Core.Maybe [Core.Text]
    -- ^ The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ Specifies the security groups for VPC endpoint.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ Specifies the subnets for VPC endpoint.
  , vPCId :: Core.Maybe Core.Text
    -- ^ The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VPCDerivedInfo' value with any optional fields omitted.
mkVPCDerivedInfo
    :: VPCDerivedInfo
mkVPCDerivedInfo
  = VPCDerivedInfo'{availabilityZones = Core.Nothing,
                    securityGroupIds = Core.Nothing, subnetIds = Core.Nothing,
                    vPCId = Core.Nothing}

-- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiAvailabilityZones :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vpcdiAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE vpcdiAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | Specifies the security groups for VPC endpoint.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiSecurityGroupIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vpcdiSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vpcdiSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | Specifies the subnets for VPC endpoint.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiSubnetIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vpcdiSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE vpcdiSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiVPCId :: Lens.Lens' VPCDerivedInfo (Core.Maybe Core.Text)
vpcdiVPCId = Lens.field @"vPCId"
{-# INLINEABLE vpcdiVPCId #-}
{-# DEPRECATED vPCId "Use generic-lens or generic-optics with 'vPCId' instead"  #-}

instance Core.FromJSON VPCDerivedInfo where
        parseJSON
          = Core.withObject "VPCDerivedInfo" Core.$
              \ x ->
                VPCDerivedInfo' Core.<$>
                  (x Core..:? "AvailabilityZones") Core.<*>
                    x Core..:? "SecurityGroupIds"
                    Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "VPCId"
