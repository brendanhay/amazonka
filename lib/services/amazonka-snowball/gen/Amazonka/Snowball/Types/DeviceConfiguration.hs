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
-- Module      : Amazonka.Snowball.Types.DeviceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.DeviceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.SnowconeDeviceConfiguration

-- | The container for @SnowconeDeviceConfiguration@.
--
-- /See:/ 'newDeviceConfiguration' smart constructor.
data DeviceConfiguration = DeviceConfiguration'
  { -- | Returns information about the device configuration for an Snowcone job.
    snowconeDeviceConfiguration :: Prelude.Maybe SnowconeDeviceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowconeDeviceConfiguration', 'deviceConfiguration_snowconeDeviceConfiguration' - Returns information about the device configuration for an Snowcone job.
newDeviceConfiguration ::
  DeviceConfiguration
newDeviceConfiguration =
  DeviceConfiguration'
    { snowconeDeviceConfiguration =
        Prelude.Nothing
    }

-- | Returns information about the device configuration for an Snowcone job.
deviceConfiguration_snowconeDeviceConfiguration :: Lens.Lens' DeviceConfiguration (Prelude.Maybe SnowconeDeviceConfiguration)
deviceConfiguration_snowconeDeviceConfiguration = Lens.lens (\DeviceConfiguration' {snowconeDeviceConfiguration} -> snowconeDeviceConfiguration) (\s@DeviceConfiguration' {} a -> s {snowconeDeviceConfiguration = a} :: DeviceConfiguration)

instance Data.FromJSON DeviceConfiguration where
  parseJSON =
    Data.withObject
      "DeviceConfiguration"
      ( \x ->
          DeviceConfiguration'
            Prelude.<$> (x Data..:? "SnowconeDeviceConfiguration")
      )

instance Prelude.Hashable DeviceConfiguration where
  hashWithSalt _salt DeviceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` snowconeDeviceConfiguration

instance Prelude.NFData DeviceConfiguration where
  rnf DeviceConfiguration' {..} =
    Prelude.rnf snowconeDeviceConfiguration

instance Data.ToJSON DeviceConfiguration where
  toJSON DeviceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SnowconeDeviceConfiguration" Data..=)
              Prelude.<$> snowconeDeviceConfiguration
          ]
      )
