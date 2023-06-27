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
-- Module      : Amazonka.EC2.Types.IpamDiscoveredResourceCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamDiscoveredResourceCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamResourceTag
import Amazonka.EC2.Types.IpamResourceType
import qualified Amazonka.Prelude as Prelude

-- | An IPAM discovered resource CIDR. A discovered resource is a resource
-- CIDR monitored under a resource discovery. The following resources can
-- be discovered: VPCs, Public IPv4 pools, VPC subnets, and Elastic IP
-- addresses. The discovered resource CIDR is the IP address range in CIDR
-- notation that is associated with the resource.
--
-- /See:/ 'newIpamDiscoveredResourceCidr' smart constructor.
data IpamDiscoveredResourceCidr = IpamDiscoveredResourceCidr'
  { -- | The percentage of IP address space in use. To convert the decimal to a
    -- percentage, multiply the decimal by 100. Note the following:
    --
    -- -   For resources that are VPCs, this is the percentage of IP address
    --     space in the VPC that\'s taken up by subnet CIDRs.
    --
    -- -   For resources that are subnets, if the subnet has an IPv4 CIDR
    --     provisioned to it, this is the percentage of IPv4 address space in
    --     the subnet that\'s in use. If the subnet has an IPv6 CIDR
    --     provisioned to it, the percentage of IPv6 address space in use is
    --     not represented. The percentage of IPv6 address space in use cannot
    --     currently be calculated.
    --
    -- -   For resources that are public IPv4 pools, this is the percentage of
    --     IP address space in the pool that\'s been allocated to Elastic IP
    --     addresses (EIPs).
    ipUsage :: Prelude.Maybe Prelude.Double,
    -- | The resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Maybe Prelude.Text,
    -- | The resource CIDR.
    resourceCidr :: Prelude.Maybe Prelude.Text,
    -- | The resource ID.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource owner ID.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The resource Region.
    resourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The resource tags.
    resourceTags :: Prelude.Maybe [IpamResourceTag],
    -- | The resource type.
    resourceType :: Prelude.Maybe IpamResourceType,
    -- | The last successful resource discovery time.
    sampleTime :: Prelude.Maybe Data.ISO8601,
    -- | The VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamDiscoveredResourceCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipUsage', 'ipamDiscoveredResourceCidr_ipUsage' - The percentage of IP address space in use. To convert the decimal to a
-- percentage, multiply the decimal by 100. Note the following:
--
-- -   For resources that are VPCs, this is the percentage of IP address
--     space in the VPC that\'s taken up by subnet CIDRs.
--
-- -   For resources that are subnets, if the subnet has an IPv4 CIDR
--     provisioned to it, this is the percentage of IPv4 address space in
--     the subnet that\'s in use. If the subnet has an IPv6 CIDR
--     provisioned to it, the percentage of IPv6 address space in use is
--     not represented. The percentage of IPv6 address space in use cannot
--     currently be calculated.
--
-- -   For resources that are public IPv4 pools, this is the percentage of
--     IP address space in the pool that\'s been allocated to Elastic IP
--     addresses (EIPs).
--
-- 'ipamResourceDiscoveryId', 'ipamDiscoveredResourceCidr_ipamResourceDiscoveryId' - The resource discovery ID.
--
-- 'resourceCidr', 'ipamDiscoveredResourceCidr_resourceCidr' - The resource CIDR.
--
-- 'resourceId', 'ipamDiscoveredResourceCidr_resourceId' - The resource ID.
--
-- 'resourceOwnerId', 'ipamDiscoveredResourceCidr_resourceOwnerId' - The resource owner ID.
--
-- 'resourceRegion', 'ipamDiscoveredResourceCidr_resourceRegion' - The resource Region.
--
-- 'resourceTags', 'ipamDiscoveredResourceCidr_resourceTags' - The resource tags.
--
-- 'resourceType', 'ipamDiscoveredResourceCidr_resourceType' - The resource type.
--
-- 'sampleTime', 'ipamDiscoveredResourceCidr_sampleTime' - The last successful resource discovery time.
--
-- 'vpcId', 'ipamDiscoveredResourceCidr_vpcId' - The VPC ID.
newIpamDiscoveredResourceCidr ::
  IpamDiscoveredResourceCidr
newIpamDiscoveredResourceCidr =
  IpamDiscoveredResourceCidr'
    { ipUsage =
        Prelude.Nothing,
      ipamResourceDiscoveryId = Prelude.Nothing,
      resourceCidr = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sampleTime = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The percentage of IP address space in use. To convert the decimal to a
-- percentage, multiply the decimal by 100. Note the following:
--
-- -   For resources that are VPCs, this is the percentage of IP address
--     space in the VPC that\'s taken up by subnet CIDRs.
--
-- -   For resources that are subnets, if the subnet has an IPv4 CIDR
--     provisioned to it, this is the percentage of IPv4 address space in
--     the subnet that\'s in use. If the subnet has an IPv6 CIDR
--     provisioned to it, the percentage of IPv6 address space in use is
--     not represented. The percentage of IPv6 address space in use cannot
--     currently be calculated.
--
-- -   For resources that are public IPv4 pools, this is the percentage of
--     IP address space in the pool that\'s been allocated to Elastic IP
--     addresses (EIPs).
ipamDiscoveredResourceCidr_ipUsage :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Double)
ipamDiscoveredResourceCidr_ipUsage = Lens.lens (\IpamDiscoveredResourceCidr' {ipUsage} -> ipUsage) (\s@IpamDiscoveredResourceCidr' {} a -> s {ipUsage = a} :: IpamDiscoveredResourceCidr)

-- | The resource discovery ID.
ipamDiscoveredResourceCidr_ipamResourceDiscoveryId :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_ipamResourceDiscoveryId = Lens.lens (\IpamDiscoveredResourceCidr' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@IpamDiscoveredResourceCidr' {} a -> s {ipamResourceDiscoveryId = a} :: IpamDiscoveredResourceCidr)

-- | The resource CIDR.
ipamDiscoveredResourceCidr_resourceCidr :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_resourceCidr = Lens.lens (\IpamDiscoveredResourceCidr' {resourceCidr} -> resourceCidr) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceCidr = a} :: IpamDiscoveredResourceCidr)

-- | The resource ID.
ipamDiscoveredResourceCidr_resourceId :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_resourceId = Lens.lens (\IpamDiscoveredResourceCidr' {resourceId} -> resourceId) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceId = a} :: IpamDiscoveredResourceCidr)

-- | The resource owner ID.
ipamDiscoveredResourceCidr_resourceOwnerId :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_resourceOwnerId = Lens.lens (\IpamDiscoveredResourceCidr' {resourceOwnerId} -> resourceOwnerId) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceOwnerId = a} :: IpamDiscoveredResourceCidr)

-- | The resource Region.
ipamDiscoveredResourceCidr_resourceRegion :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_resourceRegion = Lens.lens (\IpamDiscoveredResourceCidr' {resourceRegion} -> resourceRegion) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceRegion = a} :: IpamDiscoveredResourceCidr)

-- | The resource tags.
ipamDiscoveredResourceCidr_resourceTags :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe [IpamResourceTag])
ipamDiscoveredResourceCidr_resourceTags = Lens.lens (\IpamDiscoveredResourceCidr' {resourceTags} -> resourceTags) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceTags = a} :: IpamDiscoveredResourceCidr) Prelude.. Lens.mapping Lens.coerced

-- | The resource type.
ipamDiscoveredResourceCidr_resourceType :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe IpamResourceType)
ipamDiscoveredResourceCidr_resourceType = Lens.lens (\IpamDiscoveredResourceCidr' {resourceType} -> resourceType) (\s@IpamDiscoveredResourceCidr' {} a -> s {resourceType = a} :: IpamDiscoveredResourceCidr)

-- | The last successful resource discovery time.
ipamDiscoveredResourceCidr_sampleTime :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.UTCTime)
ipamDiscoveredResourceCidr_sampleTime = Lens.lens (\IpamDiscoveredResourceCidr' {sampleTime} -> sampleTime) (\s@IpamDiscoveredResourceCidr' {} a -> s {sampleTime = a} :: IpamDiscoveredResourceCidr) Prelude.. Lens.mapping Data._Time

-- | The VPC ID.
ipamDiscoveredResourceCidr_vpcId :: Lens.Lens' IpamDiscoveredResourceCidr (Prelude.Maybe Prelude.Text)
ipamDiscoveredResourceCidr_vpcId = Lens.lens (\IpamDiscoveredResourceCidr' {vpcId} -> vpcId) (\s@IpamDiscoveredResourceCidr' {} a -> s {vpcId = a} :: IpamDiscoveredResourceCidr)

instance Data.FromXML IpamDiscoveredResourceCidr where
  parseXML x =
    IpamDiscoveredResourceCidr'
      Prelude.<$> (x Data..@? "ipUsage")
      Prelude.<*> (x Data..@? "ipamResourceDiscoveryId")
      Prelude.<*> (x Data..@? "resourceCidr")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceOwnerId")
      Prelude.<*> (x Data..@? "resourceRegion")
      Prelude.<*> ( x
                      Data..@? "resourceTagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "sampleTime")
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable IpamDiscoveredResourceCidr where
  hashWithSalt _salt IpamDiscoveredResourceCidr' {..} =
    _salt
      `Prelude.hashWithSalt` ipUsage
      `Prelude.hashWithSalt` ipamResourceDiscoveryId
      `Prelude.hashWithSalt` resourceCidr
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sampleTime
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData IpamDiscoveredResourceCidr where
  rnf IpamDiscoveredResourceCidr' {..} =
    Prelude.rnf ipUsage
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId
      `Prelude.seq` Prelude.rnf resourceCidr
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceRegion
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sampleTime
      `Prelude.seq` Prelude.rnf vpcId
