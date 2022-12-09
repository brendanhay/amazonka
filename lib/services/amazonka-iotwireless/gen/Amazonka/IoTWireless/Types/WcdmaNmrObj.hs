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
-- Module      : Amazonka.IoTWireless.Types.WcdmaNmrObj
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WcdmaNmrObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network Measurement Reports.
--
-- /See:/ 'newWcdmaNmrObj' smart constructor.
data WcdmaNmrObj = WcdmaNmrObj'
  { -- | Path loss, or path attenuation, is the reduction in power density of an
    -- electromagnetic wave as it propagates through space.
    pathLoss :: Prelude.Maybe Prelude.Natural,
    -- | Received Signal Code Power (signal power) (dBm)
    rscp :: Prelude.Maybe Prelude.Int,
    -- | WCDMA UTRA Absolute RF Channel Number downlink.
    uarfcndl :: Prelude.Natural,
    -- | Primary Scrambling Code.
    psc :: Prelude.Natural,
    -- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
    utranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WcdmaNmrObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathLoss', 'wcdmaNmrObj_pathLoss' - Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
--
-- 'rscp', 'wcdmaNmrObj_rscp' - Received Signal Code Power (signal power) (dBm)
--
-- 'uarfcndl', 'wcdmaNmrObj_uarfcndl' - WCDMA UTRA Absolute RF Channel Number downlink.
--
-- 'psc', 'wcdmaNmrObj_psc' - Primary Scrambling Code.
--
-- 'utranCid', 'wcdmaNmrObj_utranCid' - UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
newWcdmaNmrObj ::
  -- | 'uarfcndl'
  Prelude.Natural ->
  -- | 'psc'
  Prelude.Natural ->
  -- | 'utranCid'
  Prelude.Natural ->
  WcdmaNmrObj
newWcdmaNmrObj pUarfcndl_ pPsc_ pUtranCid_ =
  WcdmaNmrObj'
    { pathLoss = Prelude.Nothing,
      rscp = Prelude.Nothing,
      uarfcndl = pUarfcndl_,
      psc = pPsc_,
      utranCid = pUtranCid_
    }

-- | Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
wcdmaNmrObj_pathLoss :: Lens.Lens' WcdmaNmrObj (Prelude.Maybe Prelude.Natural)
wcdmaNmrObj_pathLoss = Lens.lens (\WcdmaNmrObj' {pathLoss} -> pathLoss) (\s@WcdmaNmrObj' {} a -> s {pathLoss = a} :: WcdmaNmrObj)

-- | Received Signal Code Power (signal power) (dBm)
wcdmaNmrObj_rscp :: Lens.Lens' WcdmaNmrObj (Prelude.Maybe Prelude.Int)
wcdmaNmrObj_rscp = Lens.lens (\WcdmaNmrObj' {rscp} -> rscp) (\s@WcdmaNmrObj' {} a -> s {rscp = a} :: WcdmaNmrObj)

-- | WCDMA UTRA Absolute RF Channel Number downlink.
wcdmaNmrObj_uarfcndl :: Lens.Lens' WcdmaNmrObj Prelude.Natural
wcdmaNmrObj_uarfcndl = Lens.lens (\WcdmaNmrObj' {uarfcndl} -> uarfcndl) (\s@WcdmaNmrObj' {} a -> s {uarfcndl = a} :: WcdmaNmrObj)

-- | Primary Scrambling Code.
wcdmaNmrObj_psc :: Lens.Lens' WcdmaNmrObj Prelude.Natural
wcdmaNmrObj_psc = Lens.lens (\WcdmaNmrObj' {psc} -> psc) (\s@WcdmaNmrObj' {} a -> s {psc = a} :: WcdmaNmrObj)

-- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
wcdmaNmrObj_utranCid :: Lens.Lens' WcdmaNmrObj Prelude.Natural
wcdmaNmrObj_utranCid = Lens.lens (\WcdmaNmrObj' {utranCid} -> utranCid) (\s@WcdmaNmrObj' {} a -> s {utranCid = a} :: WcdmaNmrObj)

instance Prelude.Hashable WcdmaNmrObj where
  hashWithSalt _salt WcdmaNmrObj' {..} =
    _salt `Prelude.hashWithSalt` pathLoss
      `Prelude.hashWithSalt` rscp
      `Prelude.hashWithSalt` uarfcndl
      `Prelude.hashWithSalt` psc
      `Prelude.hashWithSalt` utranCid

instance Prelude.NFData WcdmaNmrObj where
  rnf WcdmaNmrObj' {..} =
    Prelude.rnf pathLoss
      `Prelude.seq` Prelude.rnf rscp
      `Prelude.seq` Prelude.rnf uarfcndl
      `Prelude.seq` Prelude.rnf psc
      `Prelude.seq` Prelude.rnf utranCid

instance Data.ToJSON WcdmaNmrObj where
  toJSON WcdmaNmrObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PathLoss" Data..=) Prelude.<$> pathLoss,
            ("Rscp" Data..=) Prelude.<$> rscp,
            Prelude.Just ("Uarfcndl" Data..= uarfcndl),
            Prelude.Just ("Psc" Data..= psc),
            Prelude.Just ("UtranCid" Data..= utranCid)
          ]
      )
