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
    vdiSecurityGroupIds,
    vdiSubnetIds,
    vdiVPCId,
    vdiAvailabilityZones,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- /See:/ 'mkVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    subnetIds :: Lude.Maybe [Lude.Text],
    vpcId :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCDerivedInfo' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
-- * 'securityGroupIds' - Specifies the security groups for VPC endpoint.
-- * 'subnetIds' - Specifies the subnets for VPC endpoint.
-- * 'vpcId' - The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
mkVPCDerivedInfo ::
  VPCDerivedInfo
mkVPCDerivedInfo =
  VPCDerivedInfo'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      availabilityZones = Lude.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiSecurityGroupIds :: Lens.Lens' VPCDerivedInfo (Lude.Maybe [Lude.Text])
vdiSecurityGroupIds = Lens.lens (securityGroupIds :: VPCDerivedInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCDerivedInfo)
{-# DEPRECATED vdiSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Specifies the subnets for VPC endpoint.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiSubnetIds :: Lens.Lens' VPCDerivedInfo (Lude.Maybe [Lude.Text])
vdiSubnetIds = Lens.lens (subnetIds :: VPCDerivedInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCDerivedInfo)
{-# DEPRECATED vdiSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiVPCId :: Lens.Lens' VPCDerivedInfo (Lude.Maybe Lude.Text)
vdiVPCId = Lens.lens (vpcId :: VPCDerivedInfo -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCDerivedInfo)
{-# DEPRECATED vdiVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiAvailabilityZones :: Lens.Lens' VPCDerivedInfo (Lude.Maybe [Lude.Text])
vdiAvailabilityZones = Lens.lens (availabilityZones :: VPCDerivedInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: VPCDerivedInfo)
{-# DEPRECATED vdiAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Lude.FromJSON VPCDerivedInfo where
  parseJSON =
    Lude.withObject
      "VPCDerivedInfo"
      ( \x ->
          VPCDerivedInfo'
            Lude.<$> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VPCId")
            Lude.<*> (x Lude..:? "AvailabilityZones" Lude..!= Lude.mempty)
      )
