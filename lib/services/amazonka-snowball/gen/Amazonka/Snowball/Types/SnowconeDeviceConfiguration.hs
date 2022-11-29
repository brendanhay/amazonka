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
-- Module      : Amazonka.Snowball.Types.SnowconeDeviceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.SnowconeDeviceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.WirelessConnection

-- | Specifies the device configuration for an Snowcone job.
--
-- /See:/ 'newSnowconeDeviceConfiguration' smart constructor.
data SnowconeDeviceConfiguration = SnowconeDeviceConfiguration'
  { -- | Configures the wireless connection for the Snowcone device.
    wirelessConnection :: Prelude.Maybe WirelessConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnowconeDeviceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessConnection', 'snowconeDeviceConfiguration_wirelessConnection' - Configures the wireless connection for the Snowcone device.
newSnowconeDeviceConfiguration ::
  SnowconeDeviceConfiguration
newSnowconeDeviceConfiguration =
  SnowconeDeviceConfiguration'
    { wirelessConnection =
        Prelude.Nothing
    }

-- | Configures the wireless connection for the Snowcone device.
snowconeDeviceConfiguration_wirelessConnection :: Lens.Lens' SnowconeDeviceConfiguration (Prelude.Maybe WirelessConnection)
snowconeDeviceConfiguration_wirelessConnection = Lens.lens (\SnowconeDeviceConfiguration' {wirelessConnection} -> wirelessConnection) (\s@SnowconeDeviceConfiguration' {} a -> s {wirelessConnection = a} :: SnowconeDeviceConfiguration)

instance Core.FromJSON SnowconeDeviceConfiguration where
  parseJSON =
    Core.withObject
      "SnowconeDeviceConfiguration"
      ( \x ->
          SnowconeDeviceConfiguration'
            Prelude.<$> (x Core..:? "WirelessConnection")
      )

instance Prelude.Hashable SnowconeDeviceConfiguration where
  hashWithSalt _salt SnowconeDeviceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` wirelessConnection

instance Prelude.NFData SnowconeDeviceConfiguration where
  rnf SnowconeDeviceConfiguration' {..} =
    Prelude.rnf wirelessConnection

instance Core.ToJSON SnowconeDeviceConfiguration where
  toJSON SnowconeDeviceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WirelessConnection" Core..=)
              Prelude.<$> wirelessConnection
          ]
      )
