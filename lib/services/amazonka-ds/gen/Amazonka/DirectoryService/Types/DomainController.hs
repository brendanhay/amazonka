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
-- Module      : Amazonka.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DomainController where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.DomainControllerStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the domain controllers for a specified
-- directory.
--
-- /See:/ 'newDomainController' smart constructor.
data DomainController = DomainController'
  { -- | The Availability Zone where the domain controller is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the directory where the domain controller resides.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the domain controller.
    dnsIpAddr :: Prelude.Maybe Prelude.Text,
    -- | Identifies a specific domain controller in the directory.
    domainControllerId :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the domain controller was created.
    launchTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the domain controller.
    status :: Prelude.Maybe DomainControllerStatus,
    -- | The date and time that the status was last updated.
    statusLastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | A description of the domain controller state.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the subnet in the VPC that contains the domain controller.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that contains the domain controller.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainController' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'domainController_availabilityZone' - The Availability Zone where the domain controller is located.
--
-- 'directoryId', 'domainController_directoryId' - Identifier of the directory where the domain controller resides.
--
-- 'dnsIpAddr', 'domainController_dnsIpAddr' - The IP address of the domain controller.
--
-- 'domainControllerId', 'domainController_domainControllerId' - Identifies a specific domain controller in the directory.
--
-- 'launchTime', 'domainController_launchTime' - Specifies when the domain controller was created.
--
-- 'status', 'domainController_status' - The status of the domain controller.
--
-- 'statusLastUpdatedDateTime', 'domainController_statusLastUpdatedDateTime' - The date and time that the status was last updated.
--
-- 'statusReason', 'domainController_statusReason' - A description of the domain controller state.
--
-- 'subnetId', 'domainController_subnetId' - Identifier of the subnet in the VPC that contains the domain controller.
--
-- 'vpcId', 'domainController_vpcId' - The identifier of the VPC that contains the domain controller.
newDomainController ::
  DomainController
newDomainController =
  DomainController'
    { availabilityZone =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      dnsIpAddr = Prelude.Nothing,
      domainControllerId = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusLastUpdatedDateTime = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The Availability Zone where the domain controller is located.
domainController_availabilityZone :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_availabilityZone = Lens.lens (\DomainController' {availabilityZone} -> availabilityZone) (\s@DomainController' {} a -> s {availabilityZone = a} :: DomainController)

-- | Identifier of the directory where the domain controller resides.
domainController_directoryId :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_directoryId = Lens.lens (\DomainController' {directoryId} -> directoryId) (\s@DomainController' {} a -> s {directoryId = a} :: DomainController)

-- | The IP address of the domain controller.
domainController_dnsIpAddr :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_dnsIpAddr = Lens.lens (\DomainController' {dnsIpAddr} -> dnsIpAddr) (\s@DomainController' {} a -> s {dnsIpAddr = a} :: DomainController)

-- | Identifies a specific domain controller in the directory.
domainController_domainControllerId :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_domainControllerId = Lens.lens (\DomainController' {domainControllerId} -> domainControllerId) (\s@DomainController' {} a -> s {domainControllerId = a} :: DomainController)

-- | Specifies when the domain controller was created.
domainController_launchTime :: Lens.Lens' DomainController (Prelude.Maybe Prelude.UTCTime)
domainController_launchTime = Lens.lens (\DomainController' {launchTime} -> launchTime) (\s@DomainController' {} a -> s {launchTime = a} :: DomainController) Prelude.. Lens.mapping Data._Time

-- | The status of the domain controller.
domainController_status :: Lens.Lens' DomainController (Prelude.Maybe DomainControllerStatus)
domainController_status = Lens.lens (\DomainController' {status} -> status) (\s@DomainController' {} a -> s {status = a} :: DomainController)

-- | The date and time that the status was last updated.
domainController_statusLastUpdatedDateTime :: Lens.Lens' DomainController (Prelude.Maybe Prelude.UTCTime)
domainController_statusLastUpdatedDateTime = Lens.lens (\DomainController' {statusLastUpdatedDateTime} -> statusLastUpdatedDateTime) (\s@DomainController' {} a -> s {statusLastUpdatedDateTime = a} :: DomainController) Prelude.. Lens.mapping Data._Time

-- | A description of the domain controller state.
domainController_statusReason :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_statusReason = Lens.lens (\DomainController' {statusReason} -> statusReason) (\s@DomainController' {} a -> s {statusReason = a} :: DomainController)

-- | Identifier of the subnet in the VPC that contains the domain controller.
domainController_subnetId :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_subnetId = Lens.lens (\DomainController' {subnetId} -> subnetId) (\s@DomainController' {} a -> s {subnetId = a} :: DomainController)

-- | The identifier of the VPC that contains the domain controller.
domainController_vpcId :: Lens.Lens' DomainController (Prelude.Maybe Prelude.Text)
domainController_vpcId = Lens.lens (\DomainController' {vpcId} -> vpcId) (\s@DomainController' {} a -> s {vpcId = a} :: DomainController)

instance Data.FromJSON DomainController where
  parseJSON =
    Data.withObject
      "DomainController"
      ( \x ->
          DomainController'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "DnsIpAddr")
            Prelude.<*> (x Data..:? "DomainControllerId")
            Prelude.<*> (x Data..:? "LaunchTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusLastUpdatedDateTime")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable DomainController where
  hashWithSalt _salt DomainController' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` dnsIpAddr
      `Prelude.hashWithSalt` domainControllerId
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusLastUpdatedDateTime
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DomainController where
  rnf DomainController' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf directoryId `Prelude.seq`
        Prelude.rnf dnsIpAddr `Prelude.seq`
          Prelude.rnf domainControllerId `Prelude.seq`
            Prelude.rnf launchTime `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf statusLastUpdatedDateTime `Prelude.seq`
                  Prelude.rnf statusReason `Prelude.seq`
                    Prelude.rnf subnetId `Prelude.seq`
                      Prelude.rnf vpcId
