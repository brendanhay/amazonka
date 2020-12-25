{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCOptions
  ( VPCOptions (..),

    -- * Smart constructor
    mkVPCOptions,

    -- * Lenses
    vpcoSecurityGroupIds,
    vpcoSubnetIds,
  )
where

import qualified Network.AWS.ElasticSearch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /See:/ 'mkVPCOptions' smart constructor.
data VPCOptions = VPCOptions'
  { -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VPCOptions' value with any optional fields omitted.
mkVPCOptions ::
  VPCOptions
mkVPCOptions =
  VPCOptions'
    { securityGroupIds = Core.Nothing,
      subnetIds = Core.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcoSecurityGroupIds :: Lens.Lens' VPCOptions (Core.Maybe [Types.String])
vpcoSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED vpcoSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Specifies the subnets for VPC endpoint.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcoSubnetIds :: Lens.Lens' VPCOptions (Core.Maybe [Types.String])
vpcoSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED vpcoSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Core.FromJSON VPCOptions where
  toJSON VPCOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
            ("SubnetIds" Core..=) Core.<$> subnetIds
          ]
      )
