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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'newElasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { -- | A list of Availability Zones.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The ID of the stack that the instance is associated with.
    stackId :: Core.Maybe Core.Text,
    -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Core.Maybe Core.Text,
    -- | A list of subnet IDs, if the stack is running in a VPC.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The instance\'s public DNS name.
    dnsName :: Core.Maybe Core.Text,
    -- | The ID of the layer that the instance is attached to.
    layerId :: Core.Maybe Core.Text,
    -- | A list of the EC2 instances that the Elastic Load Balancing instance is
    -- managing traffic for.
    ec2InstanceIds :: Core.Maybe [Core.Text],
    -- | The instance\'s AWS region.
    region :: Core.Maybe Core.Text,
    -- | The VPC ID.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      stackId = Core.Nothing,
      elasticLoadBalancerName = Core.Nothing,
      subnetIds = Core.Nothing,
      dnsName = Core.Nothing,
      layerId = Core.Nothing,
      ec2InstanceIds = Core.Nothing,
      region = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A list of Availability Zones.
elasticLoadBalancer_availabilityZones :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Core.Text])
elasticLoadBalancer_availabilityZones = Lens.lens (\ElasticLoadBalancer' {availabilityZones} -> availabilityZones) (\s@ElasticLoadBalancer' {} a -> s {availabilityZones = a} :: ElasticLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The ID of the stack that the instance is associated with.
elasticLoadBalancer_stackId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_stackId = Lens.lens (\ElasticLoadBalancer' {stackId} -> stackId) (\s@ElasticLoadBalancer' {} a -> s {stackId = a} :: ElasticLoadBalancer)

-- | The Elastic Load Balancing instance\'s name.
elasticLoadBalancer_elasticLoadBalancerName :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_elasticLoadBalancerName = Lens.lens (\ElasticLoadBalancer' {elasticLoadBalancerName} -> elasticLoadBalancerName) (\s@ElasticLoadBalancer' {} a -> s {elasticLoadBalancerName = a} :: ElasticLoadBalancer)

-- | A list of subnet IDs, if the stack is running in a VPC.
elasticLoadBalancer_subnetIds :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Core.Text])
elasticLoadBalancer_subnetIds = Lens.lens (\ElasticLoadBalancer' {subnetIds} -> subnetIds) (\s@ElasticLoadBalancer' {} a -> s {subnetIds = a} :: ElasticLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The instance\'s public DNS name.
elasticLoadBalancer_dnsName :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_dnsName = Lens.lens (\ElasticLoadBalancer' {dnsName} -> dnsName) (\s@ElasticLoadBalancer' {} a -> s {dnsName = a} :: ElasticLoadBalancer)

-- | The ID of the layer that the instance is attached to.
elasticLoadBalancer_layerId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_layerId = Lens.lens (\ElasticLoadBalancer' {layerId} -> layerId) (\s@ElasticLoadBalancer' {} a -> s {layerId = a} :: ElasticLoadBalancer)

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elasticLoadBalancer_ec2InstanceIds :: Lens.Lens' ElasticLoadBalancer (Core.Maybe [Core.Text])
elasticLoadBalancer_ec2InstanceIds = Lens.lens (\ElasticLoadBalancer' {ec2InstanceIds} -> ec2InstanceIds) (\s@ElasticLoadBalancer' {} a -> s {ec2InstanceIds = a} :: ElasticLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The instance\'s AWS region.
elasticLoadBalancer_region :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_region = Lens.lens (\ElasticLoadBalancer' {region} -> region) (\s@ElasticLoadBalancer' {} a -> s {region = a} :: ElasticLoadBalancer)

-- | The VPC ID.
elasticLoadBalancer_vpcId :: Lens.Lens' ElasticLoadBalancer (Core.Maybe Core.Text)
elasticLoadBalancer_vpcId = Lens.lens (\ElasticLoadBalancer' {vpcId} -> vpcId) (\s@ElasticLoadBalancer' {} a -> s {vpcId = a} :: ElasticLoadBalancer)

instance Core.FromJSON ElasticLoadBalancer where
  parseJSON =
    Core.withObject
      "ElasticLoadBalancer"
      ( \x ->
          ElasticLoadBalancer'
            Core.<$> (x Core..:? "AvailabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "ElasticLoadBalancerName")
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DnsName")
            Core.<*> (x Core..:? "LayerId")
            Core.<*> (x Core..:? "Ec2InstanceIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Region")
            Core.<*> (x Core..:? "VpcId")
      )

instance Core.Hashable ElasticLoadBalancer

instance Core.NFData ElasticLoadBalancer
