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
-- Module      : Network.AWS.GameLift.Types.InstanceAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceAccess where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens

-- | Information required to remotely connect to a fleet instance. Access is
-- requested by calling GetInstanceAccess.
--
-- /See:/ 'newInstanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
  { -- | A unique identifier for an instance being accessed.
    instanceId :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet containing the instance being accessed.
    fleetId :: Core.Maybe Core.Text,
    -- | IP address that is assigned to the instance.
    ipAddress :: Core.Maybe Core.Text,
    -- | Operating system that is running on the instance.
    operatingSystem :: Core.Maybe OperatingSystem,
    -- | Credentials required to access the instance.
    credentials :: Core.Maybe (Core.Sensitive InstanceCredentials)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceAccess_instanceId' - A unique identifier for an instance being accessed.
--
-- 'fleetId', 'instanceAccess_fleetId' - A unique identifier for a fleet containing the instance being accessed.
--
-- 'ipAddress', 'instanceAccess_ipAddress' - IP address that is assigned to the instance.
--
-- 'operatingSystem', 'instanceAccess_operatingSystem' - Operating system that is running on the instance.
--
-- 'credentials', 'instanceAccess_credentials' - Credentials required to access the instance.
newInstanceAccess ::
  InstanceAccess
newInstanceAccess =
  InstanceAccess'
    { instanceId = Core.Nothing,
      fleetId = Core.Nothing,
      ipAddress = Core.Nothing,
      operatingSystem = Core.Nothing,
      credentials = Core.Nothing
    }

-- | A unique identifier for an instance being accessed.
instanceAccess_instanceId :: Lens.Lens' InstanceAccess (Core.Maybe Core.Text)
instanceAccess_instanceId = Lens.lens (\InstanceAccess' {instanceId} -> instanceId) (\s@InstanceAccess' {} a -> s {instanceId = a} :: InstanceAccess)

-- | A unique identifier for a fleet containing the instance being accessed.
instanceAccess_fleetId :: Lens.Lens' InstanceAccess (Core.Maybe Core.Text)
instanceAccess_fleetId = Lens.lens (\InstanceAccess' {fleetId} -> fleetId) (\s@InstanceAccess' {} a -> s {fleetId = a} :: InstanceAccess)

-- | IP address that is assigned to the instance.
instanceAccess_ipAddress :: Lens.Lens' InstanceAccess (Core.Maybe Core.Text)
instanceAccess_ipAddress = Lens.lens (\InstanceAccess' {ipAddress} -> ipAddress) (\s@InstanceAccess' {} a -> s {ipAddress = a} :: InstanceAccess)

-- | Operating system that is running on the instance.
instanceAccess_operatingSystem :: Lens.Lens' InstanceAccess (Core.Maybe OperatingSystem)
instanceAccess_operatingSystem = Lens.lens (\InstanceAccess' {operatingSystem} -> operatingSystem) (\s@InstanceAccess' {} a -> s {operatingSystem = a} :: InstanceAccess)

-- | Credentials required to access the instance.
instanceAccess_credentials :: Lens.Lens' InstanceAccess (Core.Maybe InstanceCredentials)
instanceAccess_credentials = Lens.lens (\InstanceAccess' {credentials} -> credentials) (\s@InstanceAccess' {} a -> s {credentials = a} :: InstanceAccess) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON InstanceAccess where
  parseJSON =
    Core.withObject
      "InstanceAccess"
      ( \x ->
          InstanceAccess'
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "FleetId")
            Core.<*> (x Core..:? "IpAddress")
            Core.<*> (x Core..:? "OperatingSystem")
            Core.<*> (x Core..:? "Credentials")
      )

instance Core.Hashable InstanceAccess

instance Core.NFData InstanceAccess
