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
-- Module      : Amazonka.BackupGateway.Types.GatewayDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.GatewayDetails where

import Amazonka.BackupGateway.Types.GatewayType
import Amazonka.BackupGateway.Types.MaintenanceStartTime
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of gateway.
--
-- /See:/ 'newGatewayDetails' smart constructor.
data GatewayDetails = GatewayDetails'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the gateway.
    gatewayDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The type of the gateway type.
    gatewayType :: Prelude.Maybe GatewayType,
    -- | The hypervisor ID of the gateway.
    hypervisorId :: Prelude.Maybe Prelude.Text,
    -- | Details showing the last time Backup gateway communicated with the
    -- cloud, in Unix format and UTC time.
    lastSeenTime :: Prelude.Maybe Data.POSIX,
    -- | Returns your gateway\'s weekly maintenance start time including the day
    -- and time of the week. Note that values are in terms of the gateway\'s
    -- time zone. Can be weekly or monthly.
    maintenanceStartTime :: Prelude.Maybe MaintenanceStartTime,
    -- | Details showing the next update availability time of the gateway.
    nextUpdateAvailabilityTime :: Prelude.Maybe Data.POSIX,
    -- | The DNS name for the virtual private cloud (VPC) endpoint the gateway
    -- uses to connect to the cloud for backup gateway.
    vpcEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'gatewayDetails_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'gatewayDisplayName', 'gatewayDetails_gatewayDisplayName' - The display name of the gateway.
--
-- 'gatewayType', 'gatewayDetails_gatewayType' - The type of the gateway type.
--
-- 'hypervisorId', 'gatewayDetails_hypervisorId' - The hypervisor ID of the gateway.
--
-- 'lastSeenTime', 'gatewayDetails_lastSeenTime' - Details showing the last time Backup gateway communicated with the
-- cloud, in Unix format and UTC time.
--
-- 'maintenanceStartTime', 'gatewayDetails_maintenanceStartTime' - Returns your gateway\'s weekly maintenance start time including the day
-- and time of the week. Note that values are in terms of the gateway\'s
-- time zone. Can be weekly or monthly.
--
-- 'nextUpdateAvailabilityTime', 'gatewayDetails_nextUpdateAvailabilityTime' - Details showing the next update availability time of the gateway.
--
-- 'vpcEndpoint', 'gatewayDetails_vpcEndpoint' - The DNS name for the virtual private cloud (VPC) endpoint the gateway
-- uses to connect to the cloud for backup gateway.
newGatewayDetails ::
  GatewayDetails
newGatewayDetails =
  GatewayDetails'
    { gatewayArn = Prelude.Nothing,
      gatewayDisplayName = Prelude.Nothing,
      gatewayType = Prelude.Nothing,
      hypervisorId = Prelude.Nothing,
      lastSeenTime = Prelude.Nothing,
      maintenanceStartTime = Prelude.Nothing,
      nextUpdateAvailabilityTime = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
gatewayDetails_gatewayArn :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.Text)
gatewayDetails_gatewayArn = Lens.lens (\GatewayDetails' {gatewayArn} -> gatewayArn) (\s@GatewayDetails' {} a -> s {gatewayArn = a} :: GatewayDetails)

-- | The display name of the gateway.
gatewayDetails_gatewayDisplayName :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.Text)
gatewayDetails_gatewayDisplayName = Lens.lens (\GatewayDetails' {gatewayDisplayName} -> gatewayDisplayName) (\s@GatewayDetails' {} a -> s {gatewayDisplayName = a} :: GatewayDetails)

-- | The type of the gateway type.
gatewayDetails_gatewayType :: Lens.Lens' GatewayDetails (Prelude.Maybe GatewayType)
gatewayDetails_gatewayType = Lens.lens (\GatewayDetails' {gatewayType} -> gatewayType) (\s@GatewayDetails' {} a -> s {gatewayType = a} :: GatewayDetails)

-- | The hypervisor ID of the gateway.
gatewayDetails_hypervisorId :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.Text)
gatewayDetails_hypervisorId = Lens.lens (\GatewayDetails' {hypervisorId} -> hypervisorId) (\s@GatewayDetails' {} a -> s {hypervisorId = a} :: GatewayDetails)

-- | Details showing the last time Backup gateway communicated with the
-- cloud, in Unix format and UTC time.
gatewayDetails_lastSeenTime :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.UTCTime)
gatewayDetails_lastSeenTime = Lens.lens (\GatewayDetails' {lastSeenTime} -> lastSeenTime) (\s@GatewayDetails' {} a -> s {lastSeenTime = a} :: GatewayDetails) Prelude.. Lens.mapping Data._Time

-- | Returns your gateway\'s weekly maintenance start time including the day
-- and time of the week. Note that values are in terms of the gateway\'s
-- time zone. Can be weekly or monthly.
gatewayDetails_maintenanceStartTime :: Lens.Lens' GatewayDetails (Prelude.Maybe MaintenanceStartTime)
gatewayDetails_maintenanceStartTime = Lens.lens (\GatewayDetails' {maintenanceStartTime} -> maintenanceStartTime) (\s@GatewayDetails' {} a -> s {maintenanceStartTime = a} :: GatewayDetails)

-- | Details showing the next update availability time of the gateway.
gatewayDetails_nextUpdateAvailabilityTime :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.UTCTime)
gatewayDetails_nextUpdateAvailabilityTime = Lens.lens (\GatewayDetails' {nextUpdateAvailabilityTime} -> nextUpdateAvailabilityTime) (\s@GatewayDetails' {} a -> s {nextUpdateAvailabilityTime = a} :: GatewayDetails) Prelude.. Lens.mapping Data._Time

-- | The DNS name for the virtual private cloud (VPC) endpoint the gateway
-- uses to connect to the cloud for backup gateway.
gatewayDetails_vpcEndpoint :: Lens.Lens' GatewayDetails (Prelude.Maybe Prelude.Text)
gatewayDetails_vpcEndpoint = Lens.lens (\GatewayDetails' {vpcEndpoint} -> vpcEndpoint) (\s@GatewayDetails' {} a -> s {vpcEndpoint = a} :: GatewayDetails)

instance Data.FromJSON GatewayDetails where
  parseJSON =
    Data.withObject
      "GatewayDetails"
      ( \x ->
          GatewayDetails'
            Prelude.<$> (x Data..:? "GatewayArn")
            Prelude.<*> (x Data..:? "GatewayDisplayName")
            Prelude.<*> (x Data..:? "GatewayType")
            Prelude.<*> (x Data..:? "HypervisorId")
            Prelude.<*> (x Data..:? "LastSeenTime")
            Prelude.<*> (x Data..:? "MaintenanceStartTime")
            Prelude.<*> (x Data..:? "NextUpdateAvailabilityTime")
            Prelude.<*> (x Data..:? "VpcEndpoint")
      )

instance Prelude.Hashable GatewayDetails where
  hashWithSalt _salt GatewayDetails' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` gatewayDisplayName
      `Prelude.hashWithSalt` gatewayType
      `Prelude.hashWithSalt` hypervisorId
      `Prelude.hashWithSalt` lastSeenTime
      `Prelude.hashWithSalt` maintenanceStartTime
      `Prelude.hashWithSalt` nextUpdateAvailabilityTime
      `Prelude.hashWithSalt` vpcEndpoint

instance Prelude.NFData GatewayDetails where
  rnf GatewayDetails' {..} =
    Prelude.rnf gatewayArn `Prelude.seq`
      Prelude.rnf gatewayDisplayName `Prelude.seq`
        Prelude.rnf gatewayType `Prelude.seq`
          Prelude.rnf hypervisorId `Prelude.seq`
            Prelude.rnf lastSeenTime `Prelude.seq`
              Prelude.rnf maintenanceStartTime `Prelude.seq`
                Prelude.rnf nextUpdateAvailabilityTime `Prelude.seq`
                  Prelude.rnf vpcEndpoint
