{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowconeDeviceConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.WirelessConnection

-- | Specifies the device configuration for an AWS Snowcone job.
--
-- /See:/ 'newSnowconeDeviceConfiguration' smart constructor.
data SnowconeDeviceConfiguration = SnowconeDeviceConfiguration'
  { -- | Configures the wireless connection for the AWS Snowcone device.
    wirelessConnection :: Prelude.Maybe WirelessConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SnowconeDeviceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessConnection', 'snowconeDeviceConfiguration_wirelessConnection' - Configures the wireless connection for the AWS Snowcone device.
newSnowconeDeviceConfiguration ::
  SnowconeDeviceConfiguration
newSnowconeDeviceConfiguration =
  SnowconeDeviceConfiguration'
    { wirelessConnection =
        Prelude.Nothing
    }

-- | Configures the wireless connection for the AWS Snowcone device.
snowconeDeviceConfiguration_wirelessConnection :: Lens.Lens' SnowconeDeviceConfiguration (Prelude.Maybe WirelessConnection)
snowconeDeviceConfiguration_wirelessConnection = Lens.lens (\SnowconeDeviceConfiguration' {wirelessConnection} -> wirelessConnection) (\s@SnowconeDeviceConfiguration' {} a -> s {wirelessConnection = a} :: SnowconeDeviceConfiguration)

instance Prelude.FromJSON SnowconeDeviceConfiguration where
  parseJSON =
    Prelude.withObject
      "SnowconeDeviceConfiguration"
      ( \x ->
          SnowconeDeviceConfiguration'
            Prelude.<$> (x Prelude..:? "WirelessConnection")
      )

instance Prelude.Hashable SnowconeDeviceConfiguration

instance Prelude.NFData SnowconeDeviceConfiguration

instance Prelude.ToJSON SnowconeDeviceConfiguration where
  toJSON SnowconeDeviceConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WirelessConnection" Prelude..=)
              Prelude.<$> wirelessConnection
          ]
      )
