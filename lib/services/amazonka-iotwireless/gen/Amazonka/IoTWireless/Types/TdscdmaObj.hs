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
-- Module      : Amazonka.IoTWireless.Types.TdscdmaObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.TdscdmaObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.TdscdmaLocalId
import Amazonka.IoTWireless.Types.TdscdmaNmrObj
import qualified Amazonka.Prelude as Prelude

-- | TD-SCDMA object.
--
-- /See:/ 'newTdscdmaObj' smart constructor.
data TdscdmaObj = TdscdmaObj'
  { -- | Location Area Code.
    lac :: Prelude.Maybe Prelude.Natural,
    -- | Path loss, or path attenuation, is the reduction in power density of an
    -- electromagnetic wave as it propagates through space.
    pathLoss :: Prelude.Maybe Prelude.Natural,
    -- | Signal power of the received signal (Received Signal Code Power),
    -- measured in decibel-milliwatts (dBm).
    rscp :: Prelude.Maybe Prelude.Int,
    -- | TD-SCDMA local identification (local ID) information.
    tdscdmaLocalId :: Prelude.Maybe TdscdmaLocalId,
    -- | TD-SCDMA object for network measurement reports.
    tdscdmaNmr :: Prelude.Maybe (Prelude.NonEmpty TdscdmaNmrObj),
    -- | TD-SCDMA Timing advance.
    tdscdmaTimingAdvance :: Prelude.Maybe Prelude.Natural,
    -- | Mobile Country Code.
    mcc :: Prelude.Natural,
    -- | Mobile Network Code.
    mnc :: Prelude.Natural,
    -- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
    utranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TdscdmaObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lac', 'tdscdmaObj_lac' - Location Area Code.
--
-- 'pathLoss', 'tdscdmaObj_pathLoss' - Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
--
-- 'rscp', 'tdscdmaObj_rscp' - Signal power of the received signal (Received Signal Code Power),
-- measured in decibel-milliwatts (dBm).
--
-- 'tdscdmaLocalId', 'tdscdmaObj_tdscdmaLocalId' - TD-SCDMA local identification (local ID) information.
--
-- 'tdscdmaNmr', 'tdscdmaObj_tdscdmaNmr' - TD-SCDMA object for network measurement reports.
--
-- 'tdscdmaTimingAdvance', 'tdscdmaObj_tdscdmaTimingAdvance' - TD-SCDMA Timing advance.
--
-- 'mcc', 'tdscdmaObj_mcc' - Mobile Country Code.
--
-- 'mnc', 'tdscdmaObj_mnc' - Mobile Network Code.
--
-- 'utranCid', 'tdscdmaObj_utranCid' - UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
newTdscdmaObj ::
  -- | 'mcc'
  Prelude.Natural ->
  -- | 'mnc'
  Prelude.Natural ->
  -- | 'utranCid'
  Prelude.Natural ->
  TdscdmaObj
newTdscdmaObj pMcc_ pMnc_ pUtranCid_ =
  TdscdmaObj'
    { lac = Prelude.Nothing,
      pathLoss = Prelude.Nothing,
      rscp = Prelude.Nothing,
      tdscdmaLocalId = Prelude.Nothing,
      tdscdmaNmr = Prelude.Nothing,
      tdscdmaTimingAdvance = Prelude.Nothing,
      mcc = pMcc_,
      mnc = pMnc_,
      utranCid = pUtranCid_
    }

-- | Location Area Code.
tdscdmaObj_lac :: Lens.Lens' TdscdmaObj (Prelude.Maybe Prelude.Natural)
tdscdmaObj_lac = Lens.lens (\TdscdmaObj' {lac} -> lac) (\s@TdscdmaObj' {} a -> s {lac = a} :: TdscdmaObj)

-- | Path loss, or path attenuation, is the reduction in power density of an
-- electromagnetic wave as it propagates through space.
tdscdmaObj_pathLoss :: Lens.Lens' TdscdmaObj (Prelude.Maybe Prelude.Natural)
tdscdmaObj_pathLoss = Lens.lens (\TdscdmaObj' {pathLoss} -> pathLoss) (\s@TdscdmaObj' {} a -> s {pathLoss = a} :: TdscdmaObj)

-- | Signal power of the received signal (Received Signal Code Power),
-- measured in decibel-milliwatts (dBm).
tdscdmaObj_rscp :: Lens.Lens' TdscdmaObj (Prelude.Maybe Prelude.Int)
tdscdmaObj_rscp = Lens.lens (\TdscdmaObj' {rscp} -> rscp) (\s@TdscdmaObj' {} a -> s {rscp = a} :: TdscdmaObj)

-- | TD-SCDMA local identification (local ID) information.
tdscdmaObj_tdscdmaLocalId :: Lens.Lens' TdscdmaObj (Prelude.Maybe TdscdmaLocalId)
tdscdmaObj_tdscdmaLocalId = Lens.lens (\TdscdmaObj' {tdscdmaLocalId} -> tdscdmaLocalId) (\s@TdscdmaObj' {} a -> s {tdscdmaLocalId = a} :: TdscdmaObj)

-- | TD-SCDMA object for network measurement reports.
tdscdmaObj_tdscdmaNmr :: Lens.Lens' TdscdmaObj (Prelude.Maybe (Prelude.NonEmpty TdscdmaNmrObj))
tdscdmaObj_tdscdmaNmr = Lens.lens (\TdscdmaObj' {tdscdmaNmr} -> tdscdmaNmr) (\s@TdscdmaObj' {} a -> s {tdscdmaNmr = a} :: TdscdmaObj) Prelude.. Lens.mapping Lens.coerced

-- | TD-SCDMA Timing advance.
tdscdmaObj_tdscdmaTimingAdvance :: Lens.Lens' TdscdmaObj (Prelude.Maybe Prelude.Natural)
tdscdmaObj_tdscdmaTimingAdvance = Lens.lens (\TdscdmaObj' {tdscdmaTimingAdvance} -> tdscdmaTimingAdvance) (\s@TdscdmaObj' {} a -> s {tdscdmaTimingAdvance = a} :: TdscdmaObj)

-- | Mobile Country Code.
tdscdmaObj_mcc :: Lens.Lens' TdscdmaObj Prelude.Natural
tdscdmaObj_mcc = Lens.lens (\TdscdmaObj' {mcc} -> mcc) (\s@TdscdmaObj' {} a -> s {mcc = a} :: TdscdmaObj)

-- | Mobile Network Code.
tdscdmaObj_mnc :: Lens.Lens' TdscdmaObj Prelude.Natural
tdscdmaObj_mnc = Lens.lens (\TdscdmaObj' {mnc} -> mnc) (\s@TdscdmaObj' {} a -> s {mnc = a} :: TdscdmaObj)

-- | UTRAN (UMTS Terrestrial Radio Access Network) Cell Global Identifier.
tdscdmaObj_utranCid :: Lens.Lens' TdscdmaObj Prelude.Natural
tdscdmaObj_utranCid = Lens.lens (\TdscdmaObj' {utranCid} -> utranCid) (\s@TdscdmaObj' {} a -> s {utranCid = a} :: TdscdmaObj)

instance Prelude.Hashable TdscdmaObj where
  hashWithSalt _salt TdscdmaObj' {..} =
    _salt
      `Prelude.hashWithSalt` lac
      `Prelude.hashWithSalt` pathLoss
      `Prelude.hashWithSalt` rscp
      `Prelude.hashWithSalt` tdscdmaLocalId
      `Prelude.hashWithSalt` tdscdmaNmr
      `Prelude.hashWithSalt` tdscdmaTimingAdvance
      `Prelude.hashWithSalt` mcc
      `Prelude.hashWithSalt` mnc
      `Prelude.hashWithSalt` utranCid

instance Prelude.NFData TdscdmaObj where
  rnf TdscdmaObj' {..} =
    Prelude.rnf lac
      `Prelude.seq` Prelude.rnf pathLoss
      `Prelude.seq` Prelude.rnf rscp
      `Prelude.seq` Prelude.rnf tdscdmaLocalId
      `Prelude.seq` Prelude.rnf tdscdmaNmr
      `Prelude.seq` Prelude.rnf tdscdmaTimingAdvance
      `Prelude.seq` Prelude.rnf mcc
      `Prelude.seq` Prelude.rnf mnc
      `Prelude.seq` Prelude.rnf utranCid

instance Data.ToJSON TdscdmaObj where
  toJSON TdscdmaObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Lac" Data..=) Prelude.<$> lac,
            ("PathLoss" Data..=) Prelude.<$> pathLoss,
            ("Rscp" Data..=) Prelude.<$> rscp,
            ("TdscdmaLocalId" Data..=)
              Prelude.<$> tdscdmaLocalId,
            ("TdscdmaNmr" Data..=) Prelude.<$> tdscdmaNmr,
            ("TdscdmaTimingAdvance" Data..=)
              Prelude.<$> tdscdmaTimingAdvance,
            Prelude.Just ("Mcc" Data..= mcc),
            Prelude.Just ("Mnc" Data..= mnc),
            Prelude.Just ("UtranCid" Data..= utranCid)
          ]
      )
