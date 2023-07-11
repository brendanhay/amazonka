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
-- Module      : Amazonka.IoTWireless.Types.GsmObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.GsmObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.GsmLocalId
import Amazonka.IoTWireless.Types.GsmNmrObj
import qualified Amazonka.Prelude as Prelude

-- | GSM object.
--
-- /See:/ 'newGsmObj' smart constructor.
data GsmObj = GsmObj'
  { -- | GSM local identification (local ID) information.
    gsmLocalId :: Prelude.Maybe GsmLocalId,
    -- | GSM object for network measurement reports.
    gsmNmr :: Prelude.Maybe (Prelude.NonEmpty GsmNmrObj),
    -- | Timing advance value, which corresponds to the length of time a signal
    -- takes to reach the base station from a mobile phone.
    gsmTimingAdvance :: Prelude.Maybe Prelude.Natural,
    -- | Rx level, which is the received signal power, measured in dBm
    -- (decibel-milliwatts).
    rxLevel :: Prelude.Maybe Prelude.Int,
    -- | Mobile Country Code.
    mcc :: Prelude.Natural,
    -- | Mobile Network Code.
    mnc :: Prelude.Natural,
    -- | Location area code.
    lac :: Prelude.Natural,
    -- | GERAN (GSM EDGE Radio Access Network) Cell Global Identifier.
    geranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GsmObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gsmLocalId', 'gsmObj_gsmLocalId' - GSM local identification (local ID) information.
--
-- 'gsmNmr', 'gsmObj_gsmNmr' - GSM object for network measurement reports.
--
-- 'gsmTimingAdvance', 'gsmObj_gsmTimingAdvance' - Timing advance value, which corresponds to the length of time a signal
-- takes to reach the base station from a mobile phone.
--
-- 'rxLevel', 'gsmObj_rxLevel' - Rx level, which is the received signal power, measured in dBm
-- (decibel-milliwatts).
--
-- 'mcc', 'gsmObj_mcc' - Mobile Country Code.
--
-- 'mnc', 'gsmObj_mnc' - Mobile Network Code.
--
-- 'lac', 'gsmObj_lac' - Location area code.
--
-- 'geranCid', 'gsmObj_geranCid' - GERAN (GSM EDGE Radio Access Network) Cell Global Identifier.
newGsmObj ::
  -- | 'mcc'
  Prelude.Natural ->
  -- | 'mnc'
  Prelude.Natural ->
  -- | 'lac'
  Prelude.Natural ->
  -- | 'geranCid'
  Prelude.Natural ->
  GsmObj
newGsmObj pMcc_ pMnc_ pLac_ pGeranCid_ =
  GsmObj'
    { gsmLocalId = Prelude.Nothing,
      gsmNmr = Prelude.Nothing,
      gsmTimingAdvance = Prelude.Nothing,
      rxLevel = Prelude.Nothing,
      mcc = pMcc_,
      mnc = pMnc_,
      lac = pLac_,
      geranCid = pGeranCid_
    }

-- | GSM local identification (local ID) information.
gsmObj_gsmLocalId :: Lens.Lens' GsmObj (Prelude.Maybe GsmLocalId)
gsmObj_gsmLocalId = Lens.lens (\GsmObj' {gsmLocalId} -> gsmLocalId) (\s@GsmObj' {} a -> s {gsmLocalId = a} :: GsmObj)

-- | GSM object for network measurement reports.
gsmObj_gsmNmr :: Lens.Lens' GsmObj (Prelude.Maybe (Prelude.NonEmpty GsmNmrObj))
gsmObj_gsmNmr = Lens.lens (\GsmObj' {gsmNmr} -> gsmNmr) (\s@GsmObj' {} a -> s {gsmNmr = a} :: GsmObj) Prelude.. Lens.mapping Lens.coerced

-- | Timing advance value, which corresponds to the length of time a signal
-- takes to reach the base station from a mobile phone.
gsmObj_gsmTimingAdvance :: Lens.Lens' GsmObj (Prelude.Maybe Prelude.Natural)
gsmObj_gsmTimingAdvance = Lens.lens (\GsmObj' {gsmTimingAdvance} -> gsmTimingAdvance) (\s@GsmObj' {} a -> s {gsmTimingAdvance = a} :: GsmObj)

-- | Rx level, which is the received signal power, measured in dBm
-- (decibel-milliwatts).
gsmObj_rxLevel :: Lens.Lens' GsmObj (Prelude.Maybe Prelude.Int)
gsmObj_rxLevel = Lens.lens (\GsmObj' {rxLevel} -> rxLevel) (\s@GsmObj' {} a -> s {rxLevel = a} :: GsmObj)

-- | Mobile Country Code.
gsmObj_mcc :: Lens.Lens' GsmObj Prelude.Natural
gsmObj_mcc = Lens.lens (\GsmObj' {mcc} -> mcc) (\s@GsmObj' {} a -> s {mcc = a} :: GsmObj)

-- | Mobile Network Code.
gsmObj_mnc :: Lens.Lens' GsmObj Prelude.Natural
gsmObj_mnc = Lens.lens (\GsmObj' {mnc} -> mnc) (\s@GsmObj' {} a -> s {mnc = a} :: GsmObj)

-- | Location area code.
gsmObj_lac :: Lens.Lens' GsmObj Prelude.Natural
gsmObj_lac = Lens.lens (\GsmObj' {lac} -> lac) (\s@GsmObj' {} a -> s {lac = a} :: GsmObj)

-- | GERAN (GSM EDGE Radio Access Network) Cell Global Identifier.
gsmObj_geranCid :: Lens.Lens' GsmObj Prelude.Natural
gsmObj_geranCid = Lens.lens (\GsmObj' {geranCid} -> geranCid) (\s@GsmObj' {} a -> s {geranCid = a} :: GsmObj)

instance Prelude.Hashable GsmObj where
  hashWithSalt _salt GsmObj' {..} =
    _salt
      `Prelude.hashWithSalt` gsmLocalId
      `Prelude.hashWithSalt` gsmNmr
      `Prelude.hashWithSalt` gsmTimingAdvance
      `Prelude.hashWithSalt` rxLevel
      `Prelude.hashWithSalt` mcc
      `Prelude.hashWithSalt` mnc
      `Prelude.hashWithSalt` lac
      `Prelude.hashWithSalt` geranCid

instance Prelude.NFData GsmObj where
  rnf GsmObj' {..} =
    Prelude.rnf gsmLocalId
      `Prelude.seq` Prelude.rnf gsmNmr
      `Prelude.seq` Prelude.rnf gsmTimingAdvance
      `Prelude.seq` Prelude.rnf rxLevel
      `Prelude.seq` Prelude.rnf mcc
      `Prelude.seq` Prelude.rnf mnc
      `Prelude.seq` Prelude.rnf lac
      `Prelude.seq` Prelude.rnf geranCid

instance Data.ToJSON GsmObj where
  toJSON GsmObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GsmLocalId" Data..=) Prelude.<$> gsmLocalId,
            ("GsmNmr" Data..=) Prelude.<$> gsmNmr,
            ("GsmTimingAdvance" Data..=)
              Prelude.<$> gsmTimingAdvance,
            ("RxLevel" Data..=) Prelude.<$> rxLevel,
            Prelude.Just ("Mcc" Data..= mcc),
            Prelude.Just ("Mnc" Data..= mnc),
            Prelude.Just ("Lac" Data..= lac),
            Prelude.Just ("GeranCid" Data..= geranCid)
          ]
      )
