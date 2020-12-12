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
    voSecurityGroupIds,
    voSubnetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /See:/ 'mkVPCOptions' smart constructor.
data VPCOptions = VPCOptions'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    subnetIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCOptions' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - Specifies the security groups for VPC endpoint.
-- * 'subnetIds' - Specifies the subnets for VPC endpoint.
mkVPCOptions ::
  VPCOptions
mkVPCOptions =
  VPCOptions'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voSecurityGroupIds :: Lens.Lens' VPCOptions (Lude.Maybe [Lude.Text])
voSecurityGroupIds = Lens.lens (securityGroupIds :: VPCOptions -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCOptions)
{-# DEPRECATED voSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Specifies the subnets for VPC endpoint.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voSubnetIds :: Lens.Lens' VPCOptions (Lude.Maybe [Lude.Text])
voSubnetIds = Lens.lens (subnetIds :: VPCOptions -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCOptions)
{-# DEPRECATED voSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("SubnetIds" Lude..=) Lude.<$> subnetIds
          ]
      )
