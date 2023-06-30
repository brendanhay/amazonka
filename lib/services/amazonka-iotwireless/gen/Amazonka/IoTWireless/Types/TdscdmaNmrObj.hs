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
-- Module      : Amazonka.IoTWireless.Types.TdscdmaNmrObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.TdscdmaNmrObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | TD-SCDMA object for network measurement reports.
--
-- /See:/ 'newTdscdmaNmrObj' smart constructor.
data TdscdmaNmrObj = TdscdmaNmrObj'
  { -- | Path loss, or path attenuation, is the reduction in power density of an
    -- electromagnetic wave as it propagates through space.
    pathLoss :: Prelude.Maybe Prelude.Natural,
    -- | Code power of the received signal, measured in decibel-milliwatts (dBm).
    rscp :: Prelude.Maybe Prelude.Int,
    -- | UTRAN (UMTS Terrestrial Radio Access Network) cell global identifier.
    utranCid :: Prelude.Maybe Prelude.Natural,
    -- | TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
    -- channel number.
    uarfcn :: Prelude.Natural,
    -- | Cell parameters for TD-SCDMA network measurement reports object.
    cellParams :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TdscdmaNmrObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathLoss', 'tdscdmaNmrObj_pathLoss' - Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
--
-- 'rscp', 'tdscdmaNmrObj_rscp' - Code power of the received signal, measured in decibel-milliwatts (dBm).
--
-- 'utranCid', 'tdscdmaNmrObj_utranCid' - UTRAN (UMTS Terrestrial Radio Access Network) cell global identifier.
--
-- 'uarfcn', 'tdscdmaNmrObj_uarfcn' - TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
-- channel number.
--
-- 'cellParams', 'tdscdmaNmrObj_cellParams' - Cell parameters for TD-SCDMA network measurement reports object.
newTdscdmaNmrObj ::
  -- | 'uarfcn'
  Prelude.Natural ->
  -- | 'cellParams'
  Prelude.Natural ->
  TdscdmaNmrObj
newTdscdmaNmrObj pUarfcn_ pCellParams_ =
  TdscdmaNmrObj'
    { pathLoss = Prelude.Nothing,
      rscp = Prelude.Nothing,
      utranCid = Prelude.Nothing,
      uarfcn = pUarfcn_,
      cellParams = pCellParams_
    }

-- | Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
tdscdmaNmrObj_pathLoss :: Lens.Lens' TdscdmaNmrObj (Prelude.Maybe Prelude.Natural)
tdscdmaNmrObj_pathLoss = Lens.lens (\TdscdmaNmrObj' {pathLoss} -> pathLoss) (\s@TdscdmaNmrObj' {} a -> s {pathLoss = a} :: TdscdmaNmrObj)

-- | Code power of the received signal, measured in decibel-milliwatts (dBm).
tdscdmaNmrObj_rscp :: Lens.Lens' TdscdmaNmrObj (Prelude.Maybe Prelude.Int)
tdscdmaNmrObj_rscp = Lens.lens (\TdscdmaNmrObj' {rscp} -> rscp) (\s@TdscdmaNmrObj' {} a -> s {rscp = a} :: TdscdmaNmrObj)

-- | UTRAN (UMTS Terrestrial Radio Access Network) cell global identifier.
tdscdmaNmrObj_utranCid :: Lens.Lens' TdscdmaNmrObj (Prelude.Maybe Prelude.Natural)
tdscdmaNmrObj_utranCid = Lens.lens (\TdscdmaNmrObj' {utranCid} -> utranCid) (\s@TdscdmaNmrObj' {} a -> s {utranCid = a} :: TdscdmaNmrObj)

-- | TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
-- channel number.
tdscdmaNmrObj_uarfcn :: Lens.Lens' TdscdmaNmrObj Prelude.Natural
tdscdmaNmrObj_uarfcn = Lens.lens (\TdscdmaNmrObj' {uarfcn} -> uarfcn) (\s@TdscdmaNmrObj' {} a -> s {uarfcn = a} :: TdscdmaNmrObj)

-- | Cell parameters for TD-SCDMA network measurement reports object.
tdscdmaNmrObj_cellParams :: Lens.Lens' TdscdmaNmrObj Prelude.Natural
tdscdmaNmrObj_cellParams = Lens.lens (\TdscdmaNmrObj' {cellParams} -> cellParams) (\s@TdscdmaNmrObj' {} a -> s {cellParams = a} :: TdscdmaNmrObj)

instance Prelude.Hashable TdscdmaNmrObj where
  hashWithSalt _salt TdscdmaNmrObj' {..} =
    _salt
      `Prelude.hashWithSalt` pathLoss
      `Prelude.hashWithSalt` rscp
      `Prelude.hashWithSalt` utranCid
      `Prelude.hashWithSalt` uarfcn
      `Prelude.hashWithSalt` cellParams

instance Prelude.NFData TdscdmaNmrObj where
  rnf TdscdmaNmrObj' {..} =
    Prelude.rnf pathLoss
      `Prelude.seq` Prelude.rnf rscp
      `Prelude.seq` Prelude.rnf utranCid
      `Prelude.seq` Prelude.rnf uarfcn
      `Prelude.seq` Prelude.rnf cellParams

instance Data.ToJSON TdscdmaNmrObj where
  toJSON TdscdmaNmrObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PathLoss" Data..=) Prelude.<$> pathLoss,
            ("Rscp" Data..=) Prelude.<$> rscp,
            ("UtranCid" Data..=) Prelude.<$> utranCid,
            Prelude.Just ("Uarfcn" Data..= uarfcn),
            Prelude.Just ("CellParams" Data..= cellParams)
          ]
      )
