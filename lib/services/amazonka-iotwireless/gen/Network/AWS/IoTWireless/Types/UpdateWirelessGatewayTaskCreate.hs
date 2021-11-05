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
-- Module      : Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | UpdateWirelessGatewayTaskCreate object.
--
-- /See:/ 'newUpdateWirelessGatewayTaskCreate' smart constructor.
data UpdateWirelessGatewayTaskCreate = UpdateWirelessGatewayTaskCreate'
  { -- | The link to the S3 bucket.
    updateDataSource :: Prelude.Maybe Prelude.Text,
    -- | The IAM role used to read data from the S3 bucket.
    updateDataRole :: Prelude.Maybe Prelude.Text,
    -- | The properties that relate to the LoRaWAN wireless gateway.
    loRaWAN :: Prelude.Maybe LoRaWANUpdateGatewayTaskCreate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessGatewayTaskCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateDataSource', 'updateWirelessGatewayTaskCreate_updateDataSource' - The link to the S3 bucket.
--
-- 'updateDataRole', 'updateWirelessGatewayTaskCreate_updateDataRole' - The IAM role used to read data from the S3 bucket.
--
-- 'loRaWAN', 'updateWirelessGatewayTaskCreate_loRaWAN' - The properties that relate to the LoRaWAN wireless gateway.
newUpdateWirelessGatewayTaskCreate ::
  UpdateWirelessGatewayTaskCreate
newUpdateWirelessGatewayTaskCreate =
  UpdateWirelessGatewayTaskCreate'
    { updateDataSource =
        Prelude.Nothing,
      updateDataRole = Prelude.Nothing,
      loRaWAN = Prelude.Nothing
    }

-- | The link to the S3 bucket.
updateWirelessGatewayTaskCreate_updateDataSource :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskCreate_updateDataSource = Lens.lens (\UpdateWirelessGatewayTaskCreate' {updateDataSource} -> updateDataSource) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {updateDataSource = a} :: UpdateWirelessGatewayTaskCreate)

-- | The IAM role used to read data from the S3 bucket.
updateWirelessGatewayTaskCreate_updateDataRole :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskCreate_updateDataRole = Lens.lens (\UpdateWirelessGatewayTaskCreate' {updateDataRole} -> updateDataRole) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {updateDataRole = a} :: UpdateWirelessGatewayTaskCreate)

-- | The properties that relate to the LoRaWAN wireless gateway.
updateWirelessGatewayTaskCreate_loRaWAN :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe LoRaWANUpdateGatewayTaskCreate)
updateWirelessGatewayTaskCreate_loRaWAN = Lens.lens (\UpdateWirelessGatewayTaskCreate' {loRaWAN} -> loRaWAN) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {loRaWAN = a} :: UpdateWirelessGatewayTaskCreate)

instance
  Core.FromJSON
    UpdateWirelessGatewayTaskCreate
  where
  parseJSON =
    Core.withObject
      "UpdateWirelessGatewayTaskCreate"
      ( \x ->
          UpdateWirelessGatewayTaskCreate'
            Prelude.<$> (x Core..:? "UpdateDataSource")
            Prelude.<*> (x Core..:? "UpdateDataRole")
            Prelude.<*> (x Core..:? "LoRaWAN")
      )

instance
  Prelude.Hashable
    UpdateWirelessGatewayTaskCreate

instance
  Prelude.NFData
    UpdateWirelessGatewayTaskCreate

instance Core.ToJSON UpdateWirelessGatewayTaskCreate where
  toJSON UpdateWirelessGatewayTaskCreate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UpdateDataSource" Core..=)
              Prelude.<$> updateDataSource,
            ("UpdateDataRole" Core..=)
              Prelude.<$> updateDataRole,
            ("LoRaWAN" Core..=) Prelude.<$> loRaWAN
          ]
      )
