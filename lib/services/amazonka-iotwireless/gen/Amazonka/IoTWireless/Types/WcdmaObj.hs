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
-- Module      : Amazonka.IoTWireless.Types.WcdmaObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WcdmaObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.WcdmaLocalId
import Amazonka.IoTWireless.Types.WcdmaNmrObj
import qualified Amazonka.Prelude as Prelude

-- | WCDMA.
--
-- /See:/ 'newWcdmaObj' smart constructor.
data WcdmaObj = WcdmaObj'
  { -- | Location Area Code.
    lac :: Prelude.Maybe Prelude.Natural,
    -- | Path loss, or path attenuation, is the reduction in power density of an
    -- electromagnetic wave as it propagates through space.
    pathLoss :: Prelude.Maybe Prelude.Natural,
    -- | Received Signal Code Power (signal power) (dBm).
    rscp :: Prelude.Maybe Prelude.Int,
    -- | WCDMA local ID information.
    wcdmaLocalId :: Prelude.Maybe WcdmaLocalId,
    -- | WCDMA object for network measurement reports.
    wcdmaNmr :: Prelude.Maybe (Prelude.NonEmpty WcdmaNmrObj),
    -- | Mobile Country Code.
    mcc :: Prelude.Natural,
    -- | Mobile Network Code.
    mnc :: Prelude.Natural,
    -- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
    utranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WcdmaObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lac', 'wcdmaObj_lac' - Location Area Code.
--
-- 'pathLoss', 'wcdmaObj_pathLoss' - Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
--
-- 'rscp', 'wcdmaObj_rscp' - Received Signal Code Power (signal power) (dBm).
--
-- 'wcdmaLocalId', 'wcdmaObj_wcdmaLocalId' - WCDMA local ID information.
--
-- 'wcdmaNmr', 'wcdmaObj_wcdmaNmr' - WCDMA object for network measurement reports.
--
-- 'mcc', 'wcdmaObj_mcc' - Mobile Country Code.
--
-- 'mnc', 'wcdmaObj_mnc' - Mobile Network Code.
--
-- 'utranCid', 'wcdmaObj_utranCid' - UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
newWcdmaObj ::
  -- | 'mcc'
  Prelude.Natural ->
  -- | 'mnc'
  Prelude.Natural ->
  -- | 'utranCid'
  Prelude.Natural ->
  WcdmaObj
newWcdmaObj pMcc_ pMnc_ pUtranCid_ =
  WcdmaObj'
    { lac = Prelude.Nothing,
      pathLoss = Prelude.Nothing,
      rscp = Prelude.Nothing,
      wcdmaLocalId = Prelude.Nothing,
      wcdmaNmr = Prelude.Nothing,
      mcc = pMcc_,
      mnc = pMnc_,
      utranCid = pUtranCid_
    }

-- | Location Area Code.
wcdmaObj_lac :: Lens.Lens' WcdmaObj (Prelude.Maybe Prelude.Natural)
wcdmaObj_lac = Lens.lens (\WcdmaObj' {lac} -> lac) (\s@WcdmaObj' {} a -> s {lac = a} :: WcdmaObj)

-- | Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
wcdmaObj_pathLoss :: Lens.Lens' WcdmaObj (Prelude.Maybe Prelude.Natural)
wcdmaObj_pathLoss = Lens.lens (\WcdmaObj' {pathLoss} -> pathLoss) (\s@WcdmaObj' {} a -> s {pathLoss = a} :: WcdmaObj)

-- | Received Signal Code Power (signal power) (dBm).
wcdmaObj_rscp :: Lens.Lens' WcdmaObj (Prelude.Maybe Prelude.Int)
wcdmaObj_rscp = Lens.lens (\WcdmaObj' {rscp} -> rscp) (\s@WcdmaObj' {} a -> s {rscp = a} :: WcdmaObj)

-- | WCDMA local ID information.
wcdmaObj_wcdmaLocalId :: Lens.Lens' WcdmaObj (Prelude.Maybe WcdmaLocalId)
wcdmaObj_wcdmaLocalId = Lens.lens (\WcdmaObj' {wcdmaLocalId} -> wcdmaLocalId) (\s@WcdmaObj' {} a -> s {wcdmaLocalId = a} :: WcdmaObj)

-- | WCDMA object for network measurement reports.
wcdmaObj_wcdmaNmr :: Lens.Lens' WcdmaObj (Prelude.Maybe (Prelude.NonEmpty WcdmaNmrObj))
wcdmaObj_wcdmaNmr = Lens.lens (\WcdmaObj' {wcdmaNmr} -> wcdmaNmr) (\s@WcdmaObj' {} a -> s {wcdmaNmr = a} :: WcdmaObj) Prelude.. Lens.mapping Lens.coerced

-- | Mobile Country Code.
wcdmaObj_mcc :: Lens.Lens' WcdmaObj Prelude.Natural
wcdmaObj_mcc = Lens.lens (\WcdmaObj' {mcc} -> mcc) (\s@WcdmaObj' {} a -> s {mcc = a} :: WcdmaObj)

-- | Mobile Network Code.
wcdmaObj_mnc :: Lens.Lens' WcdmaObj Prelude.Natural
wcdmaObj_mnc = Lens.lens (\WcdmaObj' {mnc} -> mnc) (\s@WcdmaObj' {} a -> s {mnc = a} :: WcdmaObj)

-- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
wcdmaObj_utranCid :: Lens.Lens' WcdmaObj Prelude.Natural
wcdmaObj_utranCid = Lens.lens (\WcdmaObj' {utranCid} -> utranCid) (\s@WcdmaObj' {} a -> s {utranCid = a} :: WcdmaObj)

instance Prelude.Hashable WcdmaObj where
  hashWithSalt _salt WcdmaObj' {..} =
    _salt
      `Prelude.hashWithSalt` lac
      `Prelude.hashWithSalt` pathLoss
      `Prelude.hashWithSalt` rscp
      `Prelude.hashWithSalt` wcdmaLocalId
      `Prelude.hashWithSalt` wcdmaNmr
      `Prelude.hashWithSalt` mcc
      `Prelude.hashWithSalt` mnc
      `Prelude.hashWithSalt` utranCid

instance Prelude.NFData WcdmaObj where
  rnf WcdmaObj' {..} =
    Prelude.rnf lac `Prelude.seq`
      Prelude.rnf pathLoss `Prelude.seq`
        Prelude.rnf rscp `Prelude.seq`
          Prelude.rnf wcdmaLocalId `Prelude.seq`
            Prelude.rnf wcdmaNmr `Prelude.seq`
              Prelude.rnf mcc `Prelude.seq`
                Prelude.rnf mnc `Prelude.seq`
                  Prelude.rnf utranCid

instance Data.ToJSON WcdmaObj where
  toJSON WcdmaObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Lac" Data..=) Prelude.<$> lac,
            ("PathLoss" Data..=) Prelude.<$> pathLoss,
            ("Rscp" Data..=) Prelude.<$> rscp,
            ("WcdmaLocalId" Data..=) Prelude.<$> wcdmaLocalId,
            ("WcdmaNmr" Data..=) Prelude.<$> wcdmaNmr,
            Prelude.Just ("Mcc" Data..= mcc),
            Prelude.Just ("Mnc" Data..= mnc),
            Prelude.Just ("UtranCid" Data..= utranCid)
          ]
      )
