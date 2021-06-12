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
-- Module      : Network.AWS.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainController where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.DomainControllerStatus
import qualified Network.AWS.Lens as Lens

-- | Contains information about the domain controllers for a specified
-- directory.
--
-- /See:/ 'newDomainController' smart constructor.
data DomainController = DomainController'
  { -- | The status of the domain controller.
    status :: Core.Maybe DomainControllerStatus,
    -- | The IP address of the domain controller.
    dnsIpAddr :: Core.Maybe Core.Text,
    -- | Specifies when the domain controller was created.
    launchTime :: Core.Maybe Core.POSIX,
    -- | The date and time that the status was last updated.
    statusLastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | The Availability Zone where the domain controller is located.
    availabilityZone :: Core.Maybe Core.Text,
    -- | Identifier of the directory where the domain controller resides.
    directoryId :: Core.Maybe Core.Text,
    -- | Identifies a specific domain controller in the directory.
    domainControllerId :: Core.Maybe Core.Text,
    -- | Identifier of the subnet in the VPC that contains the domain controller.
    subnetId :: Core.Maybe Core.Text,
    -- | The identifier of the VPC that contains the domain controller.
    vpcId :: Core.Maybe Core.Text,
    -- | A description of the domain controller state.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainController' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'domainController_status' - The status of the domain controller.
--
-- 'dnsIpAddr', 'domainController_dnsIpAddr' - The IP address of the domain controller.
--
-- 'launchTime', 'domainController_launchTime' - Specifies when the domain controller was created.
--
-- 'statusLastUpdatedDateTime', 'domainController_statusLastUpdatedDateTime' - The date and time that the status was last updated.
--
-- 'availabilityZone', 'domainController_availabilityZone' - The Availability Zone where the domain controller is located.
--
-- 'directoryId', 'domainController_directoryId' - Identifier of the directory where the domain controller resides.
--
-- 'domainControllerId', 'domainController_domainControllerId' - Identifies a specific domain controller in the directory.
--
-- 'subnetId', 'domainController_subnetId' - Identifier of the subnet in the VPC that contains the domain controller.
--
-- 'vpcId', 'domainController_vpcId' - The identifier of the VPC that contains the domain controller.
--
-- 'statusReason', 'domainController_statusReason' - A description of the domain controller state.
newDomainController ::
  DomainController
newDomainController =
  DomainController'
    { status = Core.Nothing,
      dnsIpAddr = Core.Nothing,
      launchTime = Core.Nothing,
      statusLastUpdatedDateTime = Core.Nothing,
      availabilityZone = Core.Nothing,
      directoryId = Core.Nothing,
      domainControllerId = Core.Nothing,
      subnetId = Core.Nothing,
      vpcId = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The status of the domain controller.
domainController_status :: Lens.Lens' DomainController (Core.Maybe DomainControllerStatus)
domainController_status = Lens.lens (\DomainController' {status} -> status) (\s@DomainController' {} a -> s {status = a} :: DomainController)

-- | The IP address of the domain controller.
domainController_dnsIpAddr :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_dnsIpAddr = Lens.lens (\DomainController' {dnsIpAddr} -> dnsIpAddr) (\s@DomainController' {} a -> s {dnsIpAddr = a} :: DomainController)

-- | Specifies when the domain controller was created.
domainController_launchTime :: Lens.Lens' DomainController (Core.Maybe Core.UTCTime)
domainController_launchTime = Lens.lens (\DomainController' {launchTime} -> launchTime) (\s@DomainController' {} a -> s {launchTime = a} :: DomainController) Core.. Lens.mapping Core._Time

-- | The date and time that the status was last updated.
domainController_statusLastUpdatedDateTime :: Lens.Lens' DomainController (Core.Maybe Core.UTCTime)
domainController_statusLastUpdatedDateTime = Lens.lens (\DomainController' {statusLastUpdatedDateTime} -> statusLastUpdatedDateTime) (\s@DomainController' {} a -> s {statusLastUpdatedDateTime = a} :: DomainController) Core.. Lens.mapping Core._Time

-- | The Availability Zone where the domain controller is located.
domainController_availabilityZone :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_availabilityZone = Lens.lens (\DomainController' {availabilityZone} -> availabilityZone) (\s@DomainController' {} a -> s {availabilityZone = a} :: DomainController)

-- | Identifier of the directory where the domain controller resides.
domainController_directoryId :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_directoryId = Lens.lens (\DomainController' {directoryId} -> directoryId) (\s@DomainController' {} a -> s {directoryId = a} :: DomainController)

-- | Identifies a specific domain controller in the directory.
domainController_domainControllerId :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_domainControllerId = Lens.lens (\DomainController' {domainControllerId} -> domainControllerId) (\s@DomainController' {} a -> s {domainControllerId = a} :: DomainController)

-- | Identifier of the subnet in the VPC that contains the domain controller.
domainController_subnetId :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_subnetId = Lens.lens (\DomainController' {subnetId} -> subnetId) (\s@DomainController' {} a -> s {subnetId = a} :: DomainController)

-- | The identifier of the VPC that contains the domain controller.
domainController_vpcId :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_vpcId = Lens.lens (\DomainController' {vpcId} -> vpcId) (\s@DomainController' {} a -> s {vpcId = a} :: DomainController)

-- | A description of the domain controller state.
domainController_statusReason :: Lens.Lens' DomainController (Core.Maybe Core.Text)
domainController_statusReason = Lens.lens (\DomainController' {statusReason} -> statusReason) (\s@DomainController' {} a -> s {statusReason = a} :: DomainController)

instance Core.FromJSON DomainController where
  parseJSON =
    Core.withObject
      "DomainController"
      ( \x ->
          DomainController'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "DnsIpAddr")
            Core.<*> (x Core..:? "LaunchTime")
            Core.<*> (x Core..:? "StatusLastUpdatedDateTime")
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "DomainControllerId")
            Core.<*> (x Core..:? "SubnetId")
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (x Core..:? "StatusReason")
      )

instance Core.Hashable DomainController

instance Core.NFData DomainController
