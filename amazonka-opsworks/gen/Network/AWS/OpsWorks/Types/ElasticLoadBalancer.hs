{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ElasticLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticLoadBalancer where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'newElasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { -- | A list of Availability Zones.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the stack that the instance is associated with.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet IDs, if the stack is running in a VPC.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance\'s public DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the layer that the instance is attached to.
    layerId :: Prelude.Maybe Prelude.Text,
    -- | A list of the EC2 instances that the Elastic Load Balancing instance is
    -- managing traffic for.
    ec2InstanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance\'s AWS region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'elasticLoadBalancer_availabilityZones' - A list of Availability Zones.
--
-- 'stackId', 'elasticLoadBalancer_stackId' - The ID of the stack that the instance is associated with.
--
-- 'elasticLoadBalancerName', 'elasticLoadBalancer_elasticLoadBalancerName' - The Elastic Load Balancing instance\'s name.
--
-- 'subnetIds', 'elasticLoadBalancer_subnetIds' - A list of subnet IDs, if the stack is running in a VPC.
--
-- 'dnsName', 'elasticLoadBalancer_dnsName' - The instance\'s public DNS name.
--
-- 'layerId', 'elasticLoadBalancer_layerId' - The ID of the layer that the instance is attached to.
--
-- 'ec2InstanceIds', 'elasticLoadBalancer_ec2InstanceIds' - A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
--
-- 'region', 'elasticLoadBalancer_region' - The instance\'s AWS region.
--
-- 'vpcId', 'elasticLoadBalancer_vpcId' - The VPC ID.
newElasticLoadBalancer ::
  ElasticLoadBalancer
newElasticLoadBalancer =
  ElasticLoadBalancer'
    { availabilityZones =
        Prelude.Nothing,
      stackId = Prelude.Nothing,
      elasticLoadBalancerName = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      layerId = Prelude.Nothing,
      ec2InstanceIds = Prelude.Nothing,
      region = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A list of Availability Zones.
elasticLoadBalancer_availabilityZones :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_availabilityZones = Lens.lens (\ElasticLoadBalancer' {availabilityZones} -> availabilityZones) (\s@ElasticLoadBalancer' {} a -> s {availabilityZones = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the stack that the instance is associated with.
elasticLoadBalancer_stackId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_stackId = Lens.lens (\ElasticLoadBalancer' {stackId} -> stackId) (\s@ElasticLoadBalancer' {} a -> s {stackId = a} :: ElasticLoadBalancer)

-- | The Elastic Load Balancing instance\'s name.
elasticLoadBalancer_elasticLoadBalancerName :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_elasticLoadBalancerName = Lens.lens (\ElasticLoadBalancer' {elasticLoadBalancerName} -> elasticLoadBalancerName) (\s@ElasticLoadBalancer' {} a -> s {elasticLoadBalancerName = a} :: ElasticLoadBalancer)

-- | A list of subnet IDs, if the stack is running in a VPC.
elasticLoadBalancer_subnetIds :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_subnetIds = Lens.lens (\ElasticLoadBalancer' {subnetIds} -> subnetIds) (\s@ElasticLoadBalancer' {} a -> s {subnetIds = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance\'s public DNS name.
elasticLoadBalancer_dnsName :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_dnsName = Lens.lens (\ElasticLoadBalancer' {dnsName} -> dnsName) (\s@ElasticLoadBalancer' {} a -> s {dnsName = a} :: ElasticLoadBalancer)

-- | The ID of the layer that the instance is attached to.
elasticLoadBalancer_layerId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_layerId = Lens.lens (\ElasticLoadBalancer' {layerId} -> layerId) (\s@ElasticLoadBalancer' {} a -> s {layerId = a} :: ElasticLoadBalancer)

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elasticLoadBalancer_ec2InstanceIds :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_ec2InstanceIds = Lens.lens (\ElasticLoadBalancer' {ec2InstanceIds} -> ec2InstanceIds) (\s@ElasticLoadBalancer' {} a -> s {ec2InstanceIds = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance\'s AWS region.
elasticLoadBalancer_region :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_region = Lens.lens (\ElasticLoadBalancer' {region} -> region) (\s@ElasticLoadBalancer' {} a -> s {region = a} :: ElasticLoadBalancer)

-- | The VPC ID.
elasticLoadBalancer_vpcId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_vpcId = Lens.lens (\ElasticLoadBalancer' {vpcId} -> vpcId) (\s@ElasticLoadBalancer' {} a -> s {vpcId = a} :: ElasticLoadBalancer)

instance Prelude.FromJSON ElasticLoadBalancer where
  parseJSON =
    Prelude.withObject
      "ElasticLoadBalancer"
      ( \x ->
          ElasticLoadBalancer'
            Prelude.<$> ( x Prelude..:? "AvailabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "StackId")
            Prelude.<*> (x Prelude..:? "ElasticLoadBalancerName")
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DnsName")
            Prelude.<*> (x Prelude..:? "LayerId")
            Prelude.<*> ( x Prelude..:? "Ec2InstanceIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Region")
            Prelude.<*> (x Prelude..:? "VpcId")
      )

instance Prelude.Hashable ElasticLoadBalancer

instance Prelude.NFData ElasticLoadBalancer
