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
-- Module      : Amazonka.GameLift.Types.InstanceAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.InstanceAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.InstanceCredentials
import Amazonka.GameLift.Types.OperatingSystem
import qualified Amazonka.Prelude as Prelude

-- | Information required to remotely connect to a fleet instance.
--
-- /See:/ 'newInstanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
  { -- | Credentials required to access the instance.
    credentials :: Prelude.Maybe (Data.Sensitive InstanceCredentials),
    -- | A unique identifier for the fleet containing the instance being
    -- accessed.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the instance being accessed.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | IP address that is assigned to the instance.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Operating system that is running on the instance.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'instanceAccess_credentials' - Credentials required to access the instance.
--
-- 'fleetId', 'instanceAccess_fleetId' - A unique identifier for the fleet containing the instance being
-- accessed.
--
-- 'instanceId', 'instanceAccess_instanceId' - A unique identifier for the instance being accessed.
--
-- 'ipAddress', 'instanceAccess_ipAddress' - IP address that is assigned to the instance.
--
-- 'operatingSystem', 'instanceAccess_operatingSystem' - Operating system that is running on the instance.
newInstanceAccess ::
  InstanceAccess
newInstanceAccess =
  InstanceAccess'
    { credentials = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      operatingSystem = Prelude.Nothing
    }

-- | Credentials required to access the instance.
instanceAccess_credentials :: Lens.Lens' InstanceAccess (Prelude.Maybe InstanceCredentials)
instanceAccess_credentials = Lens.lens (\InstanceAccess' {credentials} -> credentials) (\s@InstanceAccess' {} a -> s {credentials = a} :: InstanceAccess) Prelude.. Lens.mapping Data._Sensitive

-- | A unique identifier for the fleet containing the instance being
-- accessed.
instanceAccess_fleetId :: Lens.Lens' InstanceAccess (Prelude.Maybe Prelude.Text)
instanceAccess_fleetId = Lens.lens (\InstanceAccess' {fleetId} -> fleetId) (\s@InstanceAccess' {} a -> s {fleetId = a} :: InstanceAccess)

-- | A unique identifier for the instance being accessed.
instanceAccess_instanceId :: Lens.Lens' InstanceAccess (Prelude.Maybe Prelude.Text)
instanceAccess_instanceId = Lens.lens (\InstanceAccess' {instanceId} -> instanceId) (\s@InstanceAccess' {} a -> s {instanceId = a} :: InstanceAccess)

-- | IP address that is assigned to the instance.
instanceAccess_ipAddress :: Lens.Lens' InstanceAccess (Prelude.Maybe Prelude.Text)
instanceAccess_ipAddress = Lens.lens (\InstanceAccess' {ipAddress} -> ipAddress) (\s@InstanceAccess' {} a -> s {ipAddress = a} :: InstanceAccess)

-- | Operating system that is running on the instance.
instanceAccess_operatingSystem :: Lens.Lens' InstanceAccess (Prelude.Maybe OperatingSystem)
instanceAccess_operatingSystem = Lens.lens (\InstanceAccess' {operatingSystem} -> operatingSystem) (\s@InstanceAccess' {} a -> s {operatingSystem = a} :: InstanceAccess)

instance Data.FromJSON InstanceAccess where
  parseJSON =
    Data.withObject
      "InstanceAccess"
      ( \x ->
          InstanceAccess'
            Prelude.<$> (x Data..:? "Credentials")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "OperatingSystem")
      )

instance Prelude.Hashable InstanceAccess where
  hashWithSalt _salt InstanceAccess' {..} =
    _salt
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` operatingSystem

instance Prelude.NFData InstanceAccess where
  rnf InstanceAccess' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf operatingSystem
