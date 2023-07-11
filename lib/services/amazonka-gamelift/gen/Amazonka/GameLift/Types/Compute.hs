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
-- Module      : Amazonka.GameLift.Types.Compute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Compute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.ComputeStatus
import Amazonka.GameLift.Types.EC2InstanceType
import Amazonka.GameLift.Types.OperatingSystem
import qualified Amazonka.Prelude as Prelude

-- | Resources used to host your game servers. A compute resource can be
-- managed GameLift Amazon EC2 instances or your own resources.
--
-- /See:/ 'newCompute' smart constructor.
data Compute = Compute'
  { -- | The ARN that is assigned to the compute resource and uniquely identifies
    -- it. ARNs are unique across locations.
    computeArn :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with the compute resource
    -- registered to your fleet.
    computeName :: Prelude.Maybe Prelude.Text,
    -- | Current status of the compute. A compute must have an @ACTIVE@ status to
    -- host game sessions.
    computeStatus :: Prelude.Maybe ComputeStatus,
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The DNS name of the compute resource. GameLift requires the DNS name or
    -- IP address to manage your compute resource.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet that the compute is
    -- registered to.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that the compute is registered to.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The endpoint connection details of the GameLift SDK endpoint that your
    -- game server connects to.
    gameLiftServiceSdkEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the compute resource. GameLift requires the DNS name
    -- or IP address to manage your compute resource.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom location you added to the fleet that this compute
    -- resource resides in.
    location :: Prelude.Maybe Prelude.Text,
    -- | The type of operating system on your compute resource.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | Which compute type that the fleet uses. A fleet can use Anywhere compute
    -- resources owned by you or managed Amazon EC2 instances.
    type' :: Prelude.Maybe EC2InstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Compute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeArn', 'compute_computeArn' - The ARN that is assigned to the compute resource and uniquely identifies
-- it. ARNs are unique across locations.
--
-- 'computeName', 'compute_computeName' - A descriptive label that is associated with the compute resource
-- registered to your fleet.
--
-- 'computeStatus', 'compute_computeStatus' - Current status of the compute. A compute must have an @ACTIVE@ status to
-- host game sessions.
--
-- 'creationTime', 'compute_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'dnsName', 'compute_dnsName' - The DNS name of the compute resource. GameLift requires the DNS name or
-- IP address to manage your compute resource.
--
-- 'fleetArn', 'compute_fleetArn' - The Amazon Resource Name (ARN) of the fleet that the compute is
-- registered to.
--
-- 'fleetId', 'compute_fleetId' - A unique identifier for the fleet that the compute is registered to.
--
-- 'gameLiftServiceSdkEndpoint', 'compute_gameLiftServiceSdkEndpoint' - The endpoint connection details of the GameLift SDK endpoint that your
-- game server connects to.
--
-- 'ipAddress', 'compute_ipAddress' - The IP address of the compute resource. GameLift requires the DNS name
-- or IP address to manage your compute resource.
--
-- 'location', 'compute_location' - The name of the custom location you added to the fleet that this compute
-- resource resides in.
--
-- 'operatingSystem', 'compute_operatingSystem' - The type of operating system on your compute resource.
--
-- 'type'', 'compute_type' - Which compute type that the fleet uses. A fleet can use Anywhere compute
-- resources owned by you or managed Amazon EC2 instances.
newCompute ::
  Compute
newCompute =
  Compute'
    { computeArn = Prelude.Nothing,
      computeName = Prelude.Nothing,
      computeStatus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameLiftServiceSdkEndpoint = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      location = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN that is assigned to the compute resource and uniquely identifies
-- it. ARNs are unique across locations.
compute_computeArn :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_computeArn = Lens.lens (\Compute' {computeArn} -> computeArn) (\s@Compute' {} a -> s {computeArn = a} :: Compute)

-- | A descriptive label that is associated with the compute resource
-- registered to your fleet.
compute_computeName :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_computeName = Lens.lens (\Compute' {computeName} -> computeName) (\s@Compute' {} a -> s {computeName = a} :: Compute)

-- | Current status of the compute. A compute must have an @ACTIVE@ status to
-- host game sessions.
compute_computeStatus :: Lens.Lens' Compute (Prelude.Maybe ComputeStatus)
compute_computeStatus = Lens.lens (\Compute' {computeStatus} -> computeStatus) (\s@Compute' {} a -> s {computeStatus = a} :: Compute)

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
compute_creationTime :: Lens.Lens' Compute (Prelude.Maybe Prelude.UTCTime)
compute_creationTime = Lens.lens (\Compute' {creationTime} -> creationTime) (\s@Compute' {} a -> s {creationTime = a} :: Compute) Prelude.. Lens.mapping Data._Time

-- | The DNS name of the compute resource. GameLift requires the DNS name or
-- IP address to manage your compute resource.
compute_dnsName :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_dnsName = Lens.lens (\Compute' {dnsName} -> dnsName) (\s@Compute' {} a -> s {dnsName = a} :: Compute)

-- | The Amazon Resource Name (ARN) of the fleet that the compute is
-- registered to.
compute_fleetArn :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_fleetArn = Lens.lens (\Compute' {fleetArn} -> fleetArn) (\s@Compute' {} a -> s {fleetArn = a} :: Compute)

-- | A unique identifier for the fleet that the compute is registered to.
compute_fleetId :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_fleetId = Lens.lens (\Compute' {fleetId} -> fleetId) (\s@Compute' {} a -> s {fleetId = a} :: Compute)

-- | The endpoint connection details of the GameLift SDK endpoint that your
-- game server connects to.
compute_gameLiftServiceSdkEndpoint :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_gameLiftServiceSdkEndpoint = Lens.lens (\Compute' {gameLiftServiceSdkEndpoint} -> gameLiftServiceSdkEndpoint) (\s@Compute' {} a -> s {gameLiftServiceSdkEndpoint = a} :: Compute)

-- | The IP address of the compute resource. GameLift requires the DNS name
-- or IP address to manage your compute resource.
compute_ipAddress :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_ipAddress = Lens.lens (\Compute' {ipAddress} -> ipAddress) (\s@Compute' {} a -> s {ipAddress = a} :: Compute)

-- | The name of the custom location you added to the fleet that this compute
-- resource resides in.
compute_location :: Lens.Lens' Compute (Prelude.Maybe Prelude.Text)
compute_location = Lens.lens (\Compute' {location} -> location) (\s@Compute' {} a -> s {location = a} :: Compute)

-- | The type of operating system on your compute resource.
compute_operatingSystem :: Lens.Lens' Compute (Prelude.Maybe OperatingSystem)
compute_operatingSystem = Lens.lens (\Compute' {operatingSystem} -> operatingSystem) (\s@Compute' {} a -> s {operatingSystem = a} :: Compute)

-- | Which compute type that the fleet uses. A fleet can use Anywhere compute
-- resources owned by you or managed Amazon EC2 instances.
compute_type :: Lens.Lens' Compute (Prelude.Maybe EC2InstanceType)
compute_type = Lens.lens (\Compute' {type'} -> type') (\s@Compute' {} a -> s {type' = a} :: Compute)

instance Data.FromJSON Compute where
  parseJSON =
    Data.withObject
      "Compute"
      ( \x ->
          Compute'
            Prelude.<$> (x Data..:? "ComputeArn")
            Prelude.<*> (x Data..:? "ComputeName")
            Prelude.<*> (x Data..:? "ComputeStatus")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "GameLiftServiceSdkEndpoint")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "OperatingSystem")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Compute where
  hashWithSalt _salt Compute' {..} =
    _salt
      `Prelude.hashWithSalt` computeArn
      `Prelude.hashWithSalt` computeName
      `Prelude.hashWithSalt` computeStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` gameLiftServiceSdkEndpoint
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Compute where
  rnf Compute' {..} =
    Prelude.rnf computeArn
      `Prelude.seq` Prelude.rnf computeName
      `Prelude.seq` Prelude.rnf computeStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf gameLiftServiceSdkEndpoint
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf type'
