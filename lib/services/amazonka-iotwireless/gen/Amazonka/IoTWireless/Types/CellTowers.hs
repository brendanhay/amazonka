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
-- Module      : Amazonka.IoTWireless.Types.CellTowers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.CellTowers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.CdmaObj
import Amazonka.IoTWireless.Types.GsmObj
import Amazonka.IoTWireless.Types.LteObj
import Amazonka.IoTWireless.Types.TdscdmaObj
import Amazonka.IoTWireless.Types.WcdmaObj
import qualified Amazonka.Prelude as Prelude

-- | The cell towers that were used to perform the measurements.
--
-- /See:/ 'newCellTowers' smart constructor.
data CellTowers = CellTowers'
  { -- | CDMA object information.
    cdma :: Prelude.Maybe (Prelude.NonEmpty CdmaObj),
    -- | GSM object information.
    gsm :: Prelude.Maybe (Prelude.NonEmpty GsmObj),
    -- | LTE object information.
    lte :: Prelude.Maybe (Prelude.NonEmpty LteObj),
    -- | TD-SCDMA object information.
    tdscdma :: Prelude.Maybe (Prelude.NonEmpty TdscdmaObj),
    -- | WCDMA object information.
    wcdma :: Prelude.Maybe (Prelude.NonEmpty WcdmaObj)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CellTowers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cdma', 'cellTowers_cdma' - CDMA object information.
--
-- 'gsm', 'cellTowers_gsm' - GSM object information.
--
-- 'lte', 'cellTowers_lte' - LTE object information.
--
-- 'tdscdma', 'cellTowers_tdscdma' - TD-SCDMA object information.
--
-- 'wcdma', 'cellTowers_wcdma' - WCDMA object information.
newCellTowers ::
  CellTowers
newCellTowers =
  CellTowers'
    { cdma = Prelude.Nothing,
      gsm = Prelude.Nothing,
      lte = Prelude.Nothing,
      tdscdma = Prelude.Nothing,
      wcdma = Prelude.Nothing
    }

-- | CDMA object information.
cellTowers_cdma :: Lens.Lens' CellTowers (Prelude.Maybe (Prelude.NonEmpty CdmaObj))
cellTowers_cdma = Lens.lens (\CellTowers' {cdma} -> cdma) (\s@CellTowers' {} a -> s {cdma = a} :: CellTowers) Prelude.. Lens.mapping Lens.coerced

-- | GSM object information.
cellTowers_gsm :: Lens.Lens' CellTowers (Prelude.Maybe (Prelude.NonEmpty GsmObj))
cellTowers_gsm = Lens.lens (\CellTowers' {gsm} -> gsm) (\s@CellTowers' {} a -> s {gsm = a} :: CellTowers) Prelude.. Lens.mapping Lens.coerced

-- | LTE object information.
cellTowers_lte :: Lens.Lens' CellTowers (Prelude.Maybe (Prelude.NonEmpty LteObj))
cellTowers_lte = Lens.lens (\CellTowers' {lte} -> lte) (\s@CellTowers' {} a -> s {lte = a} :: CellTowers) Prelude.. Lens.mapping Lens.coerced

-- | TD-SCDMA object information.
cellTowers_tdscdma :: Lens.Lens' CellTowers (Prelude.Maybe (Prelude.NonEmpty TdscdmaObj))
cellTowers_tdscdma = Lens.lens (\CellTowers' {tdscdma} -> tdscdma) (\s@CellTowers' {} a -> s {tdscdma = a} :: CellTowers) Prelude.. Lens.mapping Lens.coerced

-- | WCDMA object information.
cellTowers_wcdma :: Lens.Lens' CellTowers (Prelude.Maybe (Prelude.NonEmpty WcdmaObj))
cellTowers_wcdma = Lens.lens (\CellTowers' {wcdma} -> wcdma) (\s@CellTowers' {} a -> s {wcdma = a} :: CellTowers) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable CellTowers where
  hashWithSalt _salt CellTowers' {..} =
    _salt `Prelude.hashWithSalt` cdma
      `Prelude.hashWithSalt` gsm
      `Prelude.hashWithSalt` lte
      `Prelude.hashWithSalt` tdscdma
      `Prelude.hashWithSalt` wcdma

instance Prelude.NFData CellTowers where
  rnf CellTowers' {..} =
    Prelude.rnf cdma
      `Prelude.seq` Prelude.rnf gsm
      `Prelude.seq` Prelude.rnf lte
      `Prelude.seq` Prelude.rnf tdscdma
      `Prelude.seq` Prelude.rnf wcdma

instance Data.ToJSON CellTowers where
  toJSON CellTowers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Cdma" Data..=) Prelude.<$> cdma,
            ("Gsm" Data..=) Prelude.<$> gsm,
            ("Lte" Data..=) Prelude.<$> lte,
            ("Tdscdma" Data..=) Prelude.<$> tdscdma,
            ("Wcdma" Data..=) Prelude.<$> wcdma
          ]
      )
