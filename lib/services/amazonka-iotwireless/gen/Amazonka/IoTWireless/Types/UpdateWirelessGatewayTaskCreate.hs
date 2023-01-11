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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
import qualified Amazonka.Prelude as Prelude

-- | UpdateWirelessGatewayTaskCreate object.
--
-- /See:/ 'newUpdateWirelessGatewayTaskCreate' smart constructor.
data UpdateWirelessGatewayTaskCreate = UpdateWirelessGatewayTaskCreate'
  { -- | The properties that relate to the LoRaWAN wireless gateway.
    loRaWAN :: Prelude.Maybe LoRaWANUpdateGatewayTaskCreate,
    -- | The IAM role used to read data from the S3 bucket.
    updateDataRole :: Prelude.Maybe Prelude.Text,
    -- | The link to the S3 bucket.
    updateDataSource :: Prelude.Maybe Prelude.Text
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
-- 'loRaWAN', 'updateWirelessGatewayTaskCreate_loRaWAN' - The properties that relate to the LoRaWAN wireless gateway.
--
-- 'updateDataRole', 'updateWirelessGatewayTaskCreate_updateDataRole' - The IAM role used to read data from the S3 bucket.
--
-- 'updateDataSource', 'updateWirelessGatewayTaskCreate_updateDataSource' - The link to the S3 bucket.
newUpdateWirelessGatewayTaskCreate ::
  UpdateWirelessGatewayTaskCreate
newUpdateWirelessGatewayTaskCreate =
  UpdateWirelessGatewayTaskCreate'
    { loRaWAN =
        Prelude.Nothing,
      updateDataRole = Prelude.Nothing,
      updateDataSource = Prelude.Nothing
    }

-- | The properties that relate to the LoRaWAN wireless gateway.
updateWirelessGatewayTaskCreate_loRaWAN :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe LoRaWANUpdateGatewayTaskCreate)
updateWirelessGatewayTaskCreate_loRaWAN = Lens.lens (\UpdateWirelessGatewayTaskCreate' {loRaWAN} -> loRaWAN) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {loRaWAN = a} :: UpdateWirelessGatewayTaskCreate)

-- | The IAM role used to read data from the S3 bucket.
updateWirelessGatewayTaskCreate_updateDataRole :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskCreate_updateDataRole = Lens.lens (\UpdateWirelessGatewayTaskCreate' {updateDataRole} -> updateDataRole) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {updateDataRole = a} :: UpdateWirelessGatewayTaskCreate)

-- | The link to the S3 bucket.
updateWirelessGatewayTaskCreate_updateDataSource :: Lens.Lens' UpdateWirelessGatewayTaskCreate (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskCreate_updateDataSource = Lens.lens (\UpdateWirelessGatewayTaskCreate' {updateDataSource} -> updateDataSource) (\s@UpdateWirelessGatewayTaskCreate' {} a -> s {updateDataSource = a} :: UpdateWirelessGatewayTaskCreate)

instance
  Data.FromJSON
    UpdateWirelessGatewayTaskCreate
  where
  parseJSON =
    Data.withObject
      "UpdateWirelessGatewayTaskCreate"
      ( \x ->
          UpdateWirelessGatewayTaskCreate'
            Prelude.<$> (x Data..:? "LoRaWAN")
            Prelude.<*> (x Data..:? "UpdateDataRole")
            Prelude.<*> (x Data..:? "UpdateDataSource")
      )

instance
  Prelude.Hashable
    UpdateWirelessGatewayTaskCreate
  where
  hashWithSalt
    _salt
    UpdateWirelessGatewayTaskCreate' {..} =
      _salt `Prelude.hashWithSalt` loRaWAN
        `Prelude.hashWithSalt` updateDataRole
        `Prelude.hashWithSalt` updateDataSource

instance
  Prelude.NFData
    UpdateWirelessGatewayTaskCreate
  where
  rnf UpdateWirelessGatewayTaskCreate' {..} =
    Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf updateDataRole
      `Prelude.seq` Prelude.rnf updateDataSource

instance Data.ToJSON UpdateWirelessGatewayTaskCreate where
  toJSON UpdateWirelessGatewayTaskCreate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("UpdateDataRole" Data..=)
              Prelude.<$> updateDataRole,
            ("UpdateDataSource" Data..=)
              Prelude.<$> updateDataSource
          ]
      )
