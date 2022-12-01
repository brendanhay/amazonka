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
-- Module      : Amazonka.IoTWireless.Types.DeviceRegistrationStateResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DeviceRegistrationStateResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.SidewalkResourceTypeEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Device registration state resource type event configuration object for
-- enabling or disabling topic.
--
-- /See:/ 'newDeviceRegistrationStateResourceTypeEventConfiguration' smart constructor.
data DeviceRegistrationStateResourceTypeEventConfiguration = DeviceRegistrationStateResourceTypeEventConfiguration'
  { -- | Device registration resource type state event configuration object for
    -- enabling or disabling Sidewalk related event topics.
    sidewalk :: Prelude.Maybe SidewalkResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceRegistrationStateResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'deviceRegistrationStateResourceTypeEventConfiguration_sidewalk' - Device registration resource type state event configuration object for
-- enabling or disabling Sidewalk related event topics.
newDeviceRegistrationStateResourceTypeEventConfiguration ::
  DeviceRegistrationStateResourceTypeEventConfiguration
newDeviceRegistrationStateResourceTypeEventConfiguration =
  DeviceRegistrationStateResourceTypeEventConfiguration'
    { sidewalk =
        Prelude.Nothing
    }

-- | Device registration resource type state event configuration object for
-- enabling or disabling Sidewalk related event topics.
deviceRegistrationStateResourceTypeEventConfiguration_sidewalk :: Lens.Lens' DeviceRegistrationStateResourceTypeEventConfiguration (Prelude.Maybe SidewalkResourceTypeEventConfiguration)
deviceRegistrationStateResourceTypeEventConfiguration_sidewalk = Lens.lens (\DeviceRegistrationStateResourceTypeEventConfiguration' {sidewalk} -> sidewalk) (\s@DeviceRegistrationStateResourceTypeEventConfiguration' {} a -> s {sidewalk = a} :: DeviceRegistrationStateResourceTypeEventConfiguration)

instance
  Core.FromJSON
    DeviceRegistrationStateResourceTypeEventConfiguration
  where
  parseJSON =
    Core.withObject
      "DeviceRegistrationStateResourceTypeEventConfiguration"
      ( \x ->
          DeviceRegistrationStateResourceTypeEventConfiguration'
            Prelude.<$> (x Core..:? "Sidewalk")
      )

instance
  Prelude.Hashable
    DeviceRegistrationStateResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    DeviceRegistrationStateResourceTypeEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    DeviceRegistrationStateResourceTypeEventConfiguration
  where
  rnf
    DeviceRegistrationStateResourceTypeEventConfiguration' {..} =
      Prelude.rnf sidewalk

instance
  Core.ToJSON
    DeviceRegistrationStateResourceTypeEventConfiguration
  where
  toJSON
    DeviceRegistrationStateResourceTypeEventConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Sidewalk" Core..=) Prelude.<$> sidewalk]
        )
