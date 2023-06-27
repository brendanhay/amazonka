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
-- Module      : Amazonka.OpsWorks.Types.ElasticLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ElasticLoadBalancer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'newElasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { -- | A list of Availability Zones.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The instance\'s public DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | A list of the EC2 instances that the Elastic Load Balancing instance is
    -- managing traffic for.
    ec2InstanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the layer that the instance is attached to.
    layerId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s AWS region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack that the instance is associated with.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet IDs, if the stack is running in a VPC.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dnsName', 'elasticLoadBalancer_dnsName' - The instance\'s public DNS name.
--
-- 'ec2InstanceIds', 'elasticLoadBalancer_ec2InstanceIds' - A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
--
-- 'elasticLoadBalancerName', 'elasticLoadBalancer_elasticLoadBalancerName' - The Elastic Load Balancing instance\'s name.
--
-- 'layerId', 'elasticLoadBalancer_layerId' - The ID of the layer that the instance is attached to.
--
-- 'region', 'elasticLoadBalancer_region' - The instance\'s AWS region.
--
-- 'stackId', 'elasticLoadBalancer_stackId' - The ID of the stack that the instance is associated with.
--
-- 'subnetIds', 'elasticLoadBalancer_subnetIds' - A list of subnet IDs, if the stack is running in a VPC.
--
-- 'vpcId', 'elasticLoadBalancer_vpcId' - The VPC ID.
newElasticLoadBalancer ::
  ElasticLoadBalancer
newElasticLoadBalancer =
  ElasticLoadBalancer'
    { availabilityZones =
        Prelude.Nothing,
      dnsName = Prelude.Nothing,
      ec2InstanceIds = Prelude.Nothing,
      elasticLoadBalancerName = Prelude.Nothing,
      layerId = Prelude.Nothing,
      region = Prelude.Nothing,
      stackId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A list of Availability Zones.
elasticLoadBalancer_availabilityZones :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_availabilityZones = Lens.lens (\ElasticLoadBalancer' {availabilityZones} -> availabilityZones) (\s@ElasticLoadBalancer' {} a -> s {availabilityZones = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The instance\'s public DNS name.
elasticLoadBalancer_dnsName :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_dnsName = Lens.lens (\ElasticLoadBalancer' {dnsName} -> dnsName) (\s@ElasticLoadBalancer' {} a -> s {dnsName = a} :: ElasticLoadBalancer)

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elasticLoadBalancer_ec2InstanceIds :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_ec2InstanceIds = Lens.lens (\ElasticLoadBalancer' {ec2InstanceIds} -> ec2InstanceIds) (\s@ElasticLoadBalancer' {} a -> s {ec2InstanceIds = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The Elastic Load Balancing instance\'s name.
elasticLoadBalancer_elasticLoadBalancerName :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_elasticLoadBalancerName = Lens.lens (\ElasticLoadBalancer' {elasticLoadBalancerName} -> elasticLoadBalancerName) (\s@ElasticLoadBalancer' {} a -> s {elasticLoadBalancerName = a} :: ElasticLoadBalancer)

-- | The ID of the layer that the instance is attached to.
elasticLoadBalancer_layerId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_layerId = Lens.lens (\ElasticLoadBalancer' {layerId} -> layerId) (\s@ElasticLoadBalancer' {} a -> s {layerId = a} :: ElasticLoadBalancer)

-- | The instance\'s AWS region.
elasticLoadBalancer_region :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_region = Lens.lens (\ElasticLoadBalancer' {region} -> region) (\s@ElasticLoadBalancer' {} a -> s {region = a} :: ElasticLoadBalancer)

-- | The ID of the stack that the instance is associated with.
elasticLoadBalancer_stackId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_stackId = Lens.lens (\ElasticLoadBalancer' {stackId} -> stackId) (\s@ElasticLoadBalancer' {} a -> s {stackId = a} :: ElasticLoadBalancer)

-- | A list of subnet IDs, if the stack is running in a VPC.
elasticLoadBalancer_subnetIds :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe [Prelude.Text])
elasticLoadBalancer_subnetIds = Lens.lens (\ElasticLoadBalancer' {subnetIds} -> subnetIds) (\s@ElasticLoadBalancer' {} a -> s {subnetIds = a} :: ElasticLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID.
elasticLoadBalancer_vpcId :: Lens.Lens' ElasticLoadBalancer (Prelude.Maybe Prelude.Text)
elasticLoadBalancer_vpcId = Lens.lens (\ElasticLoadBalancer' {vpcId} -> vpcId) (\s@ElasticLoadBalancer' {} a -> s {vpcId = a} :: ElasticLoadBalancer)

instance Data.FromJSON ElasticLoadBalancer where
  parseJSON =
    Data.withObject
      "ElasticLoadBalancer"
      ( \x ->
          ElasticLoadBalancer'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "Ec2InstanceIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ElasticLoadBalancerName")
            Prelude.<*> (x Data..:? "LayerId")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable ElasticLoadBalancer where
  hashWithSalt _salt ElasticLoadBalancer' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` ec2InstanceIds
      `Prelude.hashWithSalt` elasticLoadBalancerName
      `Prelude.hashWithSalt` layerId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ElasticLoadBalancer where
  rnf ElasticLoadBalancer' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf ec2InstanceIds
      `Prelude.seq` Prelude.rnf elasticLoadBalancerName
      `Prelude.seq` Prelude.rnf layerId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
