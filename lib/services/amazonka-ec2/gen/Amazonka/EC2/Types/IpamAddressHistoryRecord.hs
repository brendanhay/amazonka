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
-- Module      : Amazonka.EC2.Types.IpamAddressHistoryRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamAddressHistoryRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamAddressHistoryResourceType
import Amazonka.EC2.Types.IpamComplianceStatus
import Amazonka.EC2.Types.IpamOverlapStatus
import qualified Amazonka.Prelude as Prelude

-- | The historical record of a CIDR within an IPAM scope. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/view-history-cidr-ipam.html View the history of IP addresses>
-- in the /Amazon VPC IPAM User Guide/.
--
-- /See:/ 'newIpamAddressHistoryRecord' smart constructor.
data IpamAddressHistoryRecord = IpamAddressHistoryRecord'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource.
    resourceType :: Prelude.Maybe IpamAddressHistoryResourceType,
    -- | The overlap status of an IPAM resource. The overlap status tells you if
    -- the CIDR for a resource overlaps with another CIDR in the scope. For
    -- more information on overlap statuses, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
    -- in the /Amazon VPC IPAM User Guide/.
    resourceOverlapStatus :: Prelude.Maybe IpamOverlapStatus,
    -- | The ID of the resource owner.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The CIDR of the resource.
    resourceCidr :: Prelude.Maybe Prelude.Text,
    -- | Sampled end time of the resource-to-CIDR association within the IPAM
    -- scope. Changes are picked up in periodic snapshots, so the end time may
    -- have occurred before this specific time.
    sampledEndTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Web Services Region of the resource.
    resourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The compliance status of a resource. For more information on compliance
    -- statuses, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
    -- in the /Amazon VPC IPAM User Guide/.
    resourceComplianceStatus :: Prelude.Maybe IpamComplianceStatus,
    -- | The VPC ID of the resource.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Sampled start time of the resource-to-CIDR association within the IPAM
    -- scope. Changes are picked up in periodic snapshots, so the start time
    -- may have occurred before this specific time.
    sampledStartTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamAddressHistoryRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'ipamAddressHistoryRecord_resourceId' - The ID of the resource.
--
-- 'resourceType', 'ipamAddressHistoryRecord_resourceType' - The type of the resource.
--
-- 'resourceOverlapStatus', 'ipamAddressHistoryRecord_resourceOverlapStatus' - The overlap status of an IPAM resource. The overlap status tells you if
-- the CIDR for a resource overlaps with another CIDR in the scope. For
-- more information on overlap statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'resourceOwnerId', 'ipamAddressHistoryRecord_resourceOwnerId' - The ID of the resource owner.
--
-- 'resourceName', 'ipamAddressHistoryRecord_resourceName' - The name of the resource.
--
-- 'resourceCidr', 'ipamAddressHistoryRecord_resourceCidr' - The CIDR of the resource.
--
-- 'sampledEndTime', 'ipamAddressHistoryRecord_sampledEndTime' - Sampled end time of the resource-to-CIDR association within the IPAM
-- scope. Changes are picked up in periodic snapshots, so the end time may
-- have occurred before this specific time.
--
-- 'resourceRegion', 'ipamAddressHistoryRecord_resourceRegion' - The Amazon Web Services Region of the resource.
--
-- 'resourceComplianceStatus', 'ipamAddressHistoryRecord_resourceComplianceStatus' - The compliance status of a resource. For more information on compliance
-- statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'vpcId', 'ipamAddressHistoryRecord_vpcId' - The VPC ID of the resource.
--
-- 'sampledStartTime', 'ipamAddressHistoryRecord_sampledStartTime' - Sampled start time of the resource-to-CIDR association within the IPAM
-- scope. Changes are picked up in periodic snapshots, so the start time
-- may have occurred before this specific time.
newIpamAddressHistoryRecord ::
  IpamAddressHistoryRecord
newIpamAddressHistoryRecord =
  IpamAddressHistoryRecord'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceOverlapStatus = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceCidr = Prelude.Nothing,
      sampledEndTime = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      resourceComplianceStatus = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      sampledStartTime = Prelude.Nothing
    }

-- | The ID of the resource.
ipamAddressHistoryRecord_resourceId :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_resourceId = Lens.lens (\IpamAddressHistoryRecord' {resourceId} -> resourceId) (\s@IpamAddressHistoryRecord' {} a -> s {resourceId = a} :: IpamAddressHistoryRecord)

-- | The type of the resource.
ipamAddressHistoryRecord_resourceType :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe IpamAddressHistoryResourceType)
ipamAddressHistoryRecord_resourceType = Lens.lens (\IpamAddressHistoryRecord' {resourceType} -> resourceType) (\s@IpamAddressHistoryRecord' {} a -> s {resourceType = a} :: IpamAddressHistoryRecord)

-- | The overlap status of an IPAM resource. The overlap status tells you if
-- the CIDR for a resource overlaps with another CIDR in the scope. For
-- more information on overlap statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
ipamAddressHistoryRecord_resourceOverlapStatus :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe IpamOverlapStatus)
ipamAddressHistoryRecord_resourceOverlapStatus = Lens.lens (\IpamAddressHistoryRecord' {resourceOverlapStatus} -> resourceOverlapStatus) (\s@IpamAddressHistoryRecord' {} a -> s {resourceOverlapStatus = a} :: IpamAddressHistoryRecord)

-- | The ID of the resource owner.
ipamAddressHistoryRecord_resourceOwnerId :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_resourceOwnerId = Lens.lens (\IpamAddressHistoryRecord' {resourceOwnerId} -> resourceOwnerId) (\s@IpamAddressHistoryRecord' {} a -> s {resourceOwnerId = a} :: IpamAddressHistoryRecord)

-- | The name of the resource.
ipamAddressHistoryRecord_resourceName :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_resourceName = Lens.lens (\IpamAddressHistoryRecord' {resourceName} -> resourceName) (\s@IpamAddressHistoryRecord' {} a -> s {resourceName = a} :: IpamAddressHistoryRecord)

-- | The CIDR of the resource.
ipamAddressHistoryRecord_resourceCidr :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_resourceCidr = Lens.lens (\IpamAddressHistoryRecord' {resourceCidr} -> resourceCidr) (\s@IpamAddressHistoryRecord' {} a -> s {resourceCidr = a} :: IpamAddressHistoryRecord)

-- | Sampled end time of the resource-to-CIDR association within the IPAM
-- scope. Changes are picked up in periodic snapshots, so the end time may
-- have occurred before this specific time.
ipamAddressHistoryRecord_sampledEndTime :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.UTCTime)
ipamAddressHistoryRecord_sampledEndTime = Lens.lens (\IpamAddressHistoryRecord' {sampledEndTime} -> sampledEndTime) (\s@IpamAddressHistoryRecord' {} a -> s {sampledEndTime = a} :: IpamAddressHistoryRecord) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services Region of the resource.
ipamAddressHistoryRecord_resourceRegion :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_resourceRegion = Lens.lens (\IpamAddressHistoryRecord' {resourceRegion} -> resourceRegion) (\s@IpamAddressHistoryRecord' {} a -> s {resourceRegion = a} :: IpamAddressHistoryRecord)

-- | The compliance status of a resource. For more information on compliance
-- statuses, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/monitor-cidr-compliance-ipam.html Monitor CIDR usage by resource>
-- in the /Amazon VPC IPAM User Guide/.
ipamAddressHistoryRecord_resourceComplianceStatus :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe IpamComplianceStatus)
ipamAddressHistoryRecord_resourceComplianceStatus = Lens.lens (\IpamAddressHistoryRecord' {resourceComplianceStatus} -> resourceComplianceStatus) (\s@IpamAddressHistoryRecord' {} a -> s {resourceComplianceStatus = a} :: IpamAddressHistoryRecord)

-- | The VPC ID of the resource.
ipamAddressHistoryRecord_vpcId :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.Text)
ipamAddressHistoryRecord_vpcId = Lens.lens (\IpamAddressHistoryRecord' {vpcId} -> vpcId) (\s@IpamAddressHistoryRecord' {} a -> s {vpcId = a} :: IpamAddressHistoryRecord)

-- | Sampled start time of the resource-to-CIDR association within the IPAM
-- scope. Changes are picked up in periodic snapshots, so the start time
-- may have occurred before this specific time.
ipamAddressHistoryRecord_sampledStartTime :: Lens.Lens' IpamAddressHistoryRecord (Prelude.Maybe Prelude.UTCTime)
ipamAddressHistoryRecord_sampledStartTime = Lens.lens (\IpamAddressHistoryRecord' {sampledStartTime} -> sampledStartTime) (\s@IpamAddressHistoryRecord' {} a -> s {sampledStartTime = a} :: IpamAddressHistoryRecord) Prelude.. Lens.mapping Data._Time

instance Data.FromXML IpamAddressHistoryRecord where
  parseXML x =
    IpamAddressHistoryRecord'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "resourceOverlapStatus")
      Prelude.<*> (x Data..@? "resourceOwnerId")
      Prelude.<*> (x Data..@? "resourceName")
      Prelude.<*> (x Data..@? "resourceCidr")
      Prelude.<*> (x Data..@? "sampledEndTime")
      Prelude.<*> (x Data..@? "resourceRegion")
      Prelude.<*> (x Data..@? "resourceComplianceStatus")
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@? "sampledStartTime")

instance Prelude.Hashable IpamAddressHistoryRecord where
  hashWithSalt _salt IpamAddressHistoryRecord' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceOverlapStatus
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceCidr
      `Prelude.hashWithSalt` sampledEndTime
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` resourceComplianceStatus
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` sampledStartTime

instance Prelude.NFData IpamAddressHistoryRecord where
  rnf IpamAddressHistoryRecord' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceOverlapStatus
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceCidr
      `Prelude.seq` Prelude.rnf sampledEndTime
      `Prelude.seq` Prelude.rnf resourceRegion
      `Prelude.seq` Prelude.rnf resourceComplianceStatus
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf sampledStartTime
