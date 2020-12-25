{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfo
  ( VPCDerivedInfo (..),

    -- * Smart constructor
    mkVPCDerivedInfo,

    -- * Lenses
    vpcdiAvailabilityZones,
    vpcdiSecurityGroupIds,
    vpcdiSubnetIds,
    vpcdiVPCId,
  )
where

import qualified Network.AWS.ElasticSearch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /See:/ 'mkVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { -- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
    availabilityZones :: Core.Maybe [Types.String],
    -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Core.Maybe [Types.String],
    -- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
    vPCId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VPCDerivedInfo' value with any optional fields omitted.
mkVPCDerivedInfo ::
  VPCDerivedInfo
mkVPCDerivedInfo =
  VPCDerivedInfo'
    { availabilityZones = Core.Nothing,
      securityGroupIds = Core.Nothing,
      subnetIds = Core.Nothing,
      vPCId = Core.Nothing
    }

-- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiAvailabilityZones :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Types.String])
vpcdiAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED vpcdiAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Specifies the security groups for VPC endpoint.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiSecurityGroupIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Types.String])
vpcdiSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED vpcdiSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Specifies the subnets for VPC endpoint.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiSubnetIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Types.String])
vpcdiSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED vpcdiSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdiVPCId :: Lens.Lens' VPCDerivedInfo (Core.Maybe Types.String)
vpcdiVPCId = Lens.field @"vPCId"
{-# DEPRECATED vpcdiVPCId "Use generic-lens or generic-optics with 'vPCId' instead." #-}

instance Core.FromJSON VPCDerivedInfo where
  parseJSON =
    Core.withObject "VPCDerivedInfo" Core.$
      \x ->
        VPCDerivedInfo'
          Core.<$> (x Core..:? "AvailabilityZones")
          Core.<*> (x Core..:? "SecurityGroupIds")
          Core.<*> (x Core..:? "SubnetIds")
          Core.<*> (x Core..:? "VPCId")
