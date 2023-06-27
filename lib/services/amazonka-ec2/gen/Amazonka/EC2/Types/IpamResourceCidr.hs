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
-- Module      : Amazonka.EC2.Types.IpamResourceCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamComplianceStatus
import Amazonka.EC2.Types.IpamManagementState
import Amazonka.EC2.Types.IpamOverlapStatus
import Amazonka.EC2.Types.IpamResourceTag
import Amazonka.EC2.Types.IpamResourceType
import qualified Amazonka.Prelude as Prelude

-- | The CIDR for an IPAM resource.
--
-- /See:/ 'newIpamResourceCidr' smart constructor.
data IpamResourceCidr = IpamResourceCidr'
  { -- | The compliance status of the IPAM resource. For more information on
    -- compliance statuses, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
    -- in the /Amazon VPC IPAM User Guide/.
    complianceStatus :: Prelude.Maybe IpamComplianceStatus,
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
    ipUsage :: Prelude.Maybe Prelude.Double,
    -- | The IPAM ID for an IPAM resource.
    ipamId :: Prelude.Maybe Prelude.Text,
    -- | The pool ID for an IPAM resource.
    ipamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The scope ID for an IPAM resource.
    ipamScopeId :: Prelude.Maybe Prelude.Text,
    -- | The management state of the resource. For more information about
    -- management states, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
    -- in the /Amazon VPC IPAM User Guide/.
    managementState :: Prelude.Maybe IpamManagementState,
    -- | The overlap status of an IPAM resource. The overlap status tells you if
    -- the CIDR for a resource overlaps with another CIDR in the scope. For
    -- more information on overlap statuses, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
    -- in the /Amazon VPC IPAM User Guide/.
    overlapStatus :: Prelude.Maybe IpamOverlapStatus,
    -- | The CIDR for an IPAM resource.
    resourceCidr :: Prelude.Maybe Prelude.Text,
    -- | The ID of an IPAM resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of an IPAM resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account number of the owner of an IPAM resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region for an IPAM resource.
    resourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The tags for an IPAM resource.
    resourceTags :: Prelude.Maybe [IpamResourceTag],
    -- | The type of IPAM resource.
    resourceType :: Prelude.Maybe IpamResourceType,
    -- | The ID of a VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamResourceCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceStatus', 'ipamResourceCidr_complianceStatus' - The compliance status of the IPAM resource. For more information on
-- compliance statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'ipUsage', 'ipamResourceCidr_ipUsage' - The percentage of IP address space in use. To convert the decimal to a
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
-- 'ipamId', 'ipamResourceCidr_ipamId' - The IPAM ID for an IPAM resource.
--
-- 'ipamPoolId', 'ipamResourceCidr_ipamPoolId' - The pool ID for an IPAM resource.
--
-- 'ipamScopeId', 'ipamResourceCidr_ipamScopeId' - The scope ID for an IPAM resource.
--
-- 'managementState', 'ipamResourceCidr_managementState' - The management state of the resource. For more information about
-- management states, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'overlapStatus', 'ipamResourceCidr_overlapStatus' - The overlap status of an IPAM resource. The overlap status tells you if
-- the CIDR for a resource overlaps with another CIDR in the scope. For
-- more information on overlap statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'resourceCidr', 'ipamResourceCidr_resourceCidr' - The CIDR for an IPAM resource.
--
-- 'resourceId', 'ipamResourceCidr_resourceId' - The ID of an IPAM resource.
--
-- 'resourceName', 'ipamResourceCidr_resourceName' - The name of an IPAM resource.
--
-- 'resourceOwnerId', 'ipamResourceCidr_resourceOwnerId' - The Amazon Web Services account number of the owner of an IPAM resource.
--
-- 'resourceRegion', 'ipamResourceCidr_resourceRegion' - The Amazon Web Services Region for an IPAM resource.
--
-- 'resourceTags', 'ipamResourceCidr_resourceTags' - The tags for an IPAM resource.
--
-- 'resourceType', 'ipamResourceCidr_resourceType' - The type of IPAM resource.
--
-- 'vpcId', 'ipamResourceCidr_vpcId' - The ID of a VPC.
newIpamResourceCidr ::
  IpamResourceCidr
newIpamResourceCidr =
  IpamResourceCidr'
    { complianceStatus =
        Prelude.Nothing,
      ipUsage = Prelude.Nothing,
      ipamId = Prelude.Nothing,
      ipamPoolId = Prelude.Nothing,
      ipamScopeId = Prelude.Nothing,
      managementState = Prelude.Nothing,
      overlapStatus = Prelude.Nothing,
      resourceCidr = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The compliance status of the IPAM resource. For more information on
-- compliance statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
ipamResourceCidr_complianceStatus :: Lens.Lens' IpamResourceCidr (Prelude.Maybe IpamComplianceStatus)
ipamResourceCidr_complianceStatus = Lens.lens (\IpamResourceCidr' {complianceStatus} -> complianceStatus) (\s@IpamResourceCidr' {} a -> s {complianceStatus = a} :: IpamResourceCidr)

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
ipamResourceCidr_ipUsage :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Double)
ipamResourceCidr_ipUsage = Lens.lens (\IpamResourceCidr' {ipUsage} -> ipUsage) (\s@IpamResourceCidr' {} a -> s {ipUsage = a} :: IpamResourceCidr)

-- | The IPAM ID for an IPAM resource.
ipamResourceCidr_ipamId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_ipamId = Lens.lens (\IpamResourceCidr' {ipamId} -> ipamId) (\s@IpamResourceCidr' {} a -> s {ipamId = a} :: IpamResourceCidr)

-- | The pool ID for an IPAM resource.
ipamResourceCidr_ipamPoolId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_ipamPoolId = Lens.lens (\IpamResourceCidr' {ipamPoolId} -> ipamPoolId) (\s@IpamResourceCidr' {} a -> s {ipamPoolId = a} :: IpamResourceCidr)

-- | The scope ID for an IPAM resource.
ipamResourceCidr_ipamScopeId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_ipamScopeId = Lens.lens (\IpamResourceCidr' {ipamScopeId} -> ipamScopeId) (\s@IpamResourceCidr' {} a -> s {ipamScopeId = a} :: IpamResourceCidr)

-- | The management state of the resource. For more information about
-- management states, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
ipamResourceCidr_managementState :: Lens.Lens' IpamResourceCidr (Prelude.Maybe IpamManagementState)
ipamResourceCidr_managementState = Lens.lens (\IpamResourceCidr' {managementState} -> managementState) (\s@IpamResourceCidr' {} a -> s {managementState = a} :: IpamResourceCidr)

-- | The overlap status of an IPAM resource. The overlap status tells you if
-- the CIDR for a resource overlaps with another CIDR in the scope. For
-- more information on overlap statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
ipamResourceCidr_overlapStatus :: Lens.Lens' IpamResourceCidr (Prelude.Maybe IpamOverlapStatus)
ipamResourceCidr_overlapStatus = Lens.lens (\IpamResourceCidr' {overlapStatus} -> overlapStatus) (\s@IpamResourceCidr' {} a -> s {overlapStatus = a} :: IpamResourceCidr)

-- | The CIDR for an IPAM resource.
ipamResourceCidr_resourceCidr :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_resourceCidr = Lens.lens (\IpamResourceCidr' {resourceCidr} -> resourceCidr) (\s@IpamResourceCidr' {} a -> s {resourceCidr = a} :: IpamResourceCidr)

-- | The ID of an IPAM resource.
ipamResourceCidr_resourceId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_resourceId = Lens.lens (\IpamResourceCidr' {resourceId} -> resourceId) (\s@IpamResourceCidr' {} a -> s {resourceId = a} :: IpamResourceCidr)

-- | The name of an IPAM resource.
ipamResourceCidr_resourceName :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_resourceName = Lens.lens (\IpamResourceCidr' {resourceName} -> resourceName) (\s@IpamResourceCidr' {} a -> s {resourceName = a} :: IpamResourceCidr)

-- | The Amazon Web Services account number of the owner of an IPAM resource.
ipamResourceCidr_resourceOwnerId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_resourceOwnerId = Lens.lens (\IpamResourceCidr' {resourceOwnerId} -> resourceOwnerId) (\s@IpamResourceCidr' {} a -> s {resourceOwnerId = a} :: IpamResourceCidr)

-- | The Amazon Web Services Region for an IPAM resource.
ipamResourceCidr_resourceRegion :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_resourceRegion = Lens.lens (\IpamResourceCidr' {resourceRegion} -> resourceRegion) (\s@IpamResourceCidr' {} a -> s {resourceRegion = a} :: IpamResourceCidr)

-- | The tags for an IPAM resource.
ipamResourceCidr_resourceTags :: Lens.Lens' IpamResourceCidr (Prelude.Maybe [IpamResourceTag])
ipamResourceCidr_resourceTags = Lens.lens (\IpamResourceCidr' {resourceTags} -> resourceTags) (\s@IpamResourceCidr' {} a -> s {resourceTags = a} :: IpamResourceCidr) Prelude.. Lens.mapping Lens.coerced

-- | The type of IPAM resource.
ipamResourceCidr_resourceType :: Lens.Lens' IpamResourceCidr (Prelude.Maybe IpamResourceType)
ipamResourceCidr_resourceType = Lens.lens (\IpamResourceCidr' {resourceType} -> resourceType) (\s@IpamResourceCidr' {} a -> s {resourceType = a} :: IpamResourceCidr)

-- | The ID of a VPC.
ipamResourceCidr_vpcId :: Lens.Lens' IpamResourceCidr (Prelude.Maybe Prelude.Text)
ipamResourceCidr_vpcId = Lens.lens (\IpamResourceCidr' {vpcId} -> vpcId) (\s@IpamResourceCidr' {} a -> s {vpcId = a} :: IpamResourceCidr)

instance Data.FromXML IpamResourceCidr where
  parseXML x =
    IpamResourceCidr'
      Prelude.<$> (x Data..@? "complianceStatus")
      Prelude.<*> (x Data..@? "ipUsage")
      Prelude.<*> (x Data..@? "ipamId")
      Prelude.<*> (x Data..@? "ipamPoolId")
      Prelude.<*> (x Data..@? "ipamScopeId")
      Prelude.<*> (x Data..@? "managementState")
      Prelude.<*> (x Data..@? "overlapStatus")
      Prelude.<*> (x Data..@? "resourceCidr")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceName")
      Prelude.<*> (x Data..@? "resourceOwnerId")
      Prelude.<*> (x Data..@? "resourceRegion")
      Prelude.<*> ( x
                      Data..@? "resourceTagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable IpamResourceCidr where
  hashWithSalt _salt IpamResourceCidr' {..} =
    _salt
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` ipUsage
      `Prelude.hashWithSalt` ipamId
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` ipamScopeId
      `Prelude.hashWithSalt` managementState
      `Prelude.hashWithSalt` overlapStatus
      `Prelude.hashWithSalt` resourceCidr
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData IpamResourceCidr where
  rnf IpamResourceCidr' {..} =
    Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf ipUsage
      `Prelude.seq` Prelude.rnf ipamId
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf ipamScopeId
      `Prelude.seq` Prelude.rnf managementState
      `Prelude.seq` Prelude.rnf overlapStatus
      `Prelude.seq` Prelude.rnf resourceCidr
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceRegion
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf vpcId
