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
-- Module      : Amazonka.SageMaker.Types.DeviceStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeviceStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status of devices.
--
-- /See:/ 'newDeviceStats' smart constructor.
data DeviceStats = DeviceStats'
  { -- | The number of devices connected with a heartbeat.
    connectedDeviceCount :: Prelude.Integer,
    -- | The number of registered devices.
    registeredDeviceCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectedDeviceCount', 'deviceStats_connectedDeviceCount' - The number of devices connected with a heartbeat.
--
-- 'registeredDeviceCount', 'deviceStats_registeredDeviceCount' - The number of registered devices.
newDeviceStats ::
  -- | 'connectedDeviceCount'
  Prelude.Integer ->
  -- | 'registeredDeviceCount'
  Prelude.Integer ->
  DeviceStats
newDeviceStats
  pConnectedDeviceCount_
  pRegisteredDeviceCount_ =
    DeviceStats'
      { connectedDeviceCount =
          pConnectedDeviceCount_,
        registeredDeviceCount = pRegisteredDeviceCount_
      }

-- | The number of devices connected with a heartbeat.
deviceStats_connectedDeviceCount :: Lens.Lens' DeviceStats Prelude.Integer
deviceStats_connectedDeviceCount = Lens.lens (\DeviceStats' {connectedDeviceCount} -> connectedDeviceCount) (\s@DeviceStats' {} a -> s {connectedDeviceCount = a} :: DeviceStats)

-- | The number of registered devices.
deviceStats_registeredDeviceCount :: Lens.Lens' DeviceStats Prelude.Integer
deviceStats_registeredDeviceCount = Lens.lens (\DeviceStats' {registeredDeviceCount} -> registeredDeviceCount) (\s@DeviceStats' {} a -> s {registeredDeviceCount = a} :: DeviceStats)

instance Data.FromJSON DeviceStats where
  parseJSON =
    Data.withObject
      "DeviceStats"
      ( \x ->
          DeviceStats'
            Prelude.<$> (x Data..: "ConnectedDeviceCount")
            Prelude.<*> (x Data..: "RegisteredDeviceCount")
      )

instance Prelude.Hashable DeviceStats where
  hashWithSalt _salt DeviceStats' {..} =
    _salt
      `Prelude.hashWithSalt` connectedDeviceCount
      `Prelude.hashWithSalt` registeredDeviceCount

instance Prelude.NFData DeviceStats where
  rnf DeviceStats' {..} =
    Prelude.rnf connectedDeviceCount
      `Prelude.seq` Prelude.rnf registeredDeviceCount
