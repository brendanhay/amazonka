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
-- Module      : Network.AWS.Snowball.Types.DeviceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DeviceConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration

-- | The container for @SnowconeDeviceConfiguration@.
--
-- /See:/ 'newDeviceConfiguration' smart constructor.
data DeviceConfiguration = DeviceConfiguration'
  { -- | Returns information about the device configuration for an AWS Snowcone
    -- job.
    snowconeDeviceConfiguration :: Core.Maybe SnowconeDeviceConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowconeDeviceConfiguration', 'deviceConfiguration_snowconeDeviceConfiguration' - Returns information about the device configuration for an AWS Snowcone
-- job.
newDeviceConfiguration ::
  DeviceConfiguration
newDeviceConfiguration =
  DeviceConfiguration'
    { snowconeDeviceConfiguration =
        Core.Nothing
    }

-- | Returns information about the device configuration for an AWS Snowcone
-- job.
deviceConfiguration_snowconeDeviceConfiguration :: Lens.Lens' DeviceConfiguration (Core.Maybe SnowconeDeviceConfiguration)
deviceConfiguration_snowconeDeviceConfiguration = Lens.lens (\DeviceConfiguration' {snowconeDeviceConfiguration} -> snowconeDeviceConfiguration) (\s@DeviceConfiguration' {} a -> s {snowconeDeviceConfiguration = a} :: DeviceConfiguration)

instance Core.FromJSON DeviceConfiguration where
  parseJSON =
    Core.withObject
      "DeviceConfiguration"
      ( \x ->
          DeviceConfiguration'
            Core.<$> (x Core..:? "SnowconeDeviceConfiguration")
      )

instance Core.Hashable DeviceConfiguration

instance Core.NFData DeviceConfiguration

instance Core.ToJSON DeviceConfiguration where
  toJSON DeviceConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SnowconeDeviceConfiguration" Core..=)
              Core.<$> snowconeDeviceConfiguration
          ]
      )
