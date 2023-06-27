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
-- Module      : Amazonka.IoTWireless.Types.SidewalkCreateWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkCreateWirelessDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk object for creating a wireless device.
--
-- /See:/ 'newSidewalkCreateWirelessDevice' smart constructor.
data SidewalkCreateWirelessDevice = SidewalkCreateWirelessDevice'
  { -- | The ID of the Sidewalk device profile.
    deviceProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkCreateWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceProfileId', 'sidewalkCreateWirelessDevice_deviceProfileId' - The ID of the Sidewalk device profile.
newSidewalkCreateWirelessDevice ::
  SidewalkCreateWirelessDevice
newSidewalkCreateWirelessDevice =
  SidewalkCreateWirelessDevice'
    { deviceProfileId =
        Prelude.Nothing
    }

-- | The ID of the Sidewalk device profile.
sidewalkCreateWirelessDevice_deviceProfileId :: Lens.Lens' SidewalkCreateWirelessDevice (Prelude.Maybe Prelude.Text)
sidewalkCreateWirelessDevice_deviceProfileId = Lens.lens (\SidewalkCreateWirelessDevice' {deviceProfileId} -> deviceProfileId) (\s@SidewalkCreateWirelessDevice' {} a -> s {deviceProfileId = a} :: SidewalkCreateWirelessDevice)

instance
  Prelude.Hashable
    SidewalkCreateWirelessDevice
  where
  hashWithSalt _salt SidewalkCreateWirelessDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceProfileId

instance Prelude.NFData SidewalkCreateWirelessDevice where
  rnf SidewalkCreateWirelessDevice' {..} =
    Prelude.rnf deviceProfileId

instance Data.ToJSON SidewalkCreateWirelessDevice where
  toJSON SidewalkCreateWirelessDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceProfileId" Data..=)
              Prelude.<$> deviceProfileId
          ]
      )
