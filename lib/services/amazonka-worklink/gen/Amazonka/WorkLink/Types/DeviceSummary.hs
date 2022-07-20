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
-- Module      : Amazonka.WorkLink.Types.DeviceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.DeviceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkLink.Types.DeviceStatus

-- | The summary of devices.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The status of the device.
    deviceStatus :: Prelude.Maybe DeviceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'deviceSummary_deviceId' - The ID of the device.
--
-- 'deviceStatus', 'deviceSummary_deviceStatus' - The status of the device.
newDeviceSummary ::
  DeviceSummary
newDeviceSummary =
  DeviceSummary'
    { deviceId = Prelude.Nothing,
      deviceStatus = Prelude.Nothing
    }

-- | The ID of the device.
deviceSummary_deviceId :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_deviceId = Lens.lens (\DeviceSummary' {deviceId} -> deviceId) (\s@DeviceSummary' {} a -> s {deviceId = a} :: DeviceSummary)

-- | The status of the device.
deviceSummary_deviceStatus :: Lens.Lens' DeviceSummary (Prelude.Maybe DeviceStatus)
deviceSummary_deviceStatus = Lens.lens (\DeviceSummary' {deviceStatus} -> deviceStatus) (\s@DeviceSummary' {} a -> s {deviceStatus = a} :: DeviceSummary)

instance Core.FromJSON DeviceSummary where
  parseJSON =
    Core.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Prelude.<$> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "DeviceStatus")
      )

instance Prelude.Hashable DeviceSummary where
  hashWithSalt _salt DeviceSummary' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` deviceStatus

instance Prelude.NFData DeviceSummary where
  rnf DeviceSummary' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf deviceStatus
