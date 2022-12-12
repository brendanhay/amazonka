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
-- Module      : Amazonka.IoTWireless.Types.LteObj
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LteObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LteLocalId
import Amazonka.IoTWireless.Types.LteNmrObj
import qualified Amazonka.Prelude as Prelude

-- | LTE object.
--
-- /See:/ 'newLteObj' smart constructor.
data LteObj = LteObj'
  { -- | LTE local identification (local ID) information.
    lteLocalId :: Prelude.Maybe LteLocalId,
    -- | LTE object for network measurement reports.
    lteNmr :: Prelude.Maybe (Prelude.NonEmpty LteNmrObj),
    -- | LTE timing advance.
    lteTimingAdvance :: Prelude.Maybe Prelude.Natural,
    -- | Parameter that determines whether the LTE object is capable of
    -- supporting NR (new radio).
    nrCapable :: Prelude.Maybe Prelude.Bool,
    -- | Signal power of the reference signal received, measured in dBm
    -- (decibel-milliwatts).
    rsrp :: Prelude.Maybe Prelude.Int,
    -- | Signal quality of the reference Signal received, measured in decibels
    -- (dB).
    rsrq :: Prelude.Maybe Prelude.Double,
    -- | LTE tracking area code.
    tac :: Prelude.Maybe Prelude.Natural,
    -- | Mobile Country Code.
    mcc :: Prelude.Natural,
    -- | Mobile Network Code.
    mnc :: Prelude.Natural,
    -- | E-UTRAN (Evolved Universal Terrestrial Radio Access Network) Cell Global
    -- Identifier.
    eutranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LteObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lteLocalId', 'lteObj_lteLocalId' - LTE local identification (local ID) information.
--
-- 'lteNmr', 'lteObj_lteNmr' - LTE object for network measurement reports.
--
-- 'lteTimingAdvance', 'lteObj_lteTimingAdvance' - LTE timing advance.
--
-- 'nrCapable', 'lteObj_nrCapable' - Parameter that determines whether the LTE object is capable of
-- supporting NR (new radio).
--
-- 'rsrp', 'lteObj_rsrp' - Signal power of the reference signal received, measured in dBm
-- (decibel-milliwatts).
--
-- 'rsrq', 'lteObj_rsrq' - Signal quality of the reference Signal received, measured in decibels
-- (dB).
--
-- 'tac', 'lteObj_tac' - LTE tracking area code.
--
-- 'mcc', 'lteObj_mcc' - Mobile Country Code.
--
-- 'mnc', 'lteObj_mnc' - Mobile Network Code.
--
-- 'eutranCid', 'lteObj_eutranCid' - E-UTRAN (Evolved Universal Terrestrial Radio Access Network) Cell Global
-- Identifier.
newLteObj ::
  -- | 'mcc'
  Prelude.Natural ->
  -- | 'mnc'
  Prelude.Natural ->
  -- | 'eutranCid'
  Prelude.Natural ->
  LteObj
newLteObj pMcc_ pMnc_ pEutranCid_ =
  LteObj'
    { lteLocalId = Prelude.Nothing,
      lteNmr = Prelude.Nothing,
      lteTimingAdvance = Prelude.Nothing,
      nrCapable = Prelude.Nothing,
      rsrp = Prelude.Nothing,
      rsrq = Prelude.Nothing,
      tac = Prelude.Nothing,
      mcc = pMcc_,
      mnc = pMnc_,
      eutranCid = pEutranCid_
    }

-- | LTE local identification (local ID) information.
lteObj_lteLocalId :: Lens.Lens' LteObj (Prelude.Maybe LteLocalId)
lteObj_lteLocalId = Lens.lens (\LteObj' {lteLocalId} -> lteLocalId) (\s@LteObj' {} a -> s {lteLocalId = a} :: LteObj)

-- | LTE object for network measurement reports.
lteObj_lteNmr :: Lens.Lens' LteObj (Prelude.Maybe (Prelude.NonEmpty LteNmrObj))
lteObj_lteNmr = Lens.lens (\LteObj' {lteNmr} -> lteNmr) (\s@LteObj' {} a -> s {lteNmr = a} :: LteObj) Prelude.. Lens.mapping Lens.coerced

-- | LTE timing advance.
lteObj_lteTimingAdvance :: Lens.Lens' LteObj (Prelude.Maybe Prelude.Natural)
lteObj_lteTimingAdvance = Lens.lens (\LteObj' {lteTimingAdvance} -> lteTimingAdvance) (\s@LteObj' {} a -> s {lteTimingAdvance = a} :: LteObj)

-- | Parameter that determines whether the LTE object is capable of
-- supporting NR (new radio).
lteObj_nrCapable :: Lens.Lens' LteObj (Prelude.Maybe Prelude.Bool)
lteObj_nrCapable = Lens.lens (\LteObj' {nrCapable} -> nrCapable) (\s@LteObj' {} a -> s {nrCapable = a} :: LteObj)

-- | Signal power of the reference signal received, measured in dBm
-- (decibel-milliwatts).
lteObj_rsrp :: Lens.Lens' LteObj (Prelude.Maybe Prelude.Int)
lteObj_rsrp = Lens.lens (\LteObj' {rsrp} -> rsrp) (\s@LteObj' {} a -> s {rsrp = a} :: LteObj)

-- | Signal quality of the reference Signal received, measured in decibels
-- (dB).
lteObj_rsrq :: Lens.Lens' LteObj (Prelude.Maybe Prelude.Double)
lteObj_rsrq = Lens.lens (\LteObj' {rsrq} -> rsrq) (\s@LteObj' {} a -> s {rsrq = a} :: LteObj)

-- | LTE tracking area code.
lteObj_tac :: Lens.Lens' LteObj (Prelude.Maybe Prelude.Natural)
lteObj_tac = Lens.lens (\LteObj' {tac} -> tac) (\s@LteObj' {} a -> s {tac = a} :: LteObj)

-- | Mobile Country Code.
lteObj_mcc :: Lens.Lens' LteObj Prelude.Natural
lteObj_mcc = Lens.lens (\LteObj' {mcc} -> mcc) (\s@LteObj' {} a -> s {mcc = a} :: LteObj)

-- | Mobile Network Code.
lteObj_mnc :: Lens.Lens' LteObj Prelude.Natural
lteObj_mnc = Lens.lens (\LteObj' {mnc} -> mnc) (\s@LteObj' {} a -> s {mnc = a} :: LteObj)

-- | E-UTRAN (Evolved Universal Terrestrial Radio Access Network) Cell Global
-- Identifier.
lteObj_eutranCid :: Lens.Lens' LteObj Prelude.Natural
lteObj_eutranCid = Lens.lens (\LteObj' {eutranCid} -> eutranCid) (\s@LteObj' {} a -> s {eutranCid = a} :: LteObj)

instance Prelude.Hashable LteObj where
  hashWithSalt _salt LteObj' {..} =
    _salt `Prelude.hashWithSalt` lteLocalId
      `Prelude.hashWithSalt` lteNmr
      `Prelude.hashWithSalt` lteTimingAdvance
      `Prelude.hashWithSalt` nrCapable
      `Prelude.hashWithSalt` rsrp
      `Prelude.hashWithSalt` rsrq
      `Prelude.hashWithSalt` tac
      `Prelude.hashWithSalt` mcc
      `Prelude.hashWithSalt` mnc
      `Prelude.hashWithSalt` eutranCid

instance Prelude.NFData LteObj where
  rnf LteObj' {..} =
    Prelude.rnf lteLocalId
      `Prelude.seq` Prelude.rnf lteNmr
      `Prelude.seq` Prelude.rnf lteTimingAdvance
      `Prelude.seq` Prelude.rnf nrCapable
      `Prelude.seq` Prelude.rnf rsrp
      `Prelude.seq` Prelude.rnf rsrq
      `Prelude.seq` Prelude.rnf tac
      `Prelude.seq` Prelude.rnf mcc
      `Prelude.seq` Prelude.rnf mnc
      `Prelude.seq` Prelude.rnf eutranCid

instance Data.ToJSON LteObj where
  toJSON LteObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LteLocalId" Data..=) Prelude.<$> lteLocalId,
            ("LteNmr" Data..=) Prelude.<$> lteNmr,
            ("LteTimingAdvance" Data..=)
              Prelude.<$> lteTimingAdvance,
            ("NrCapable" Data..=) Prelude.<$> nrCapable,
            ("Rsrp" Data..=) Prelude.<$> rsrp,
            ("Rsrq" Data..=) Prelude.<$> rsrq,
            ("Tac" Data..=) Prelude.<$> tac,
            Prelude.Just ("Mcc" Data..= mcc),
            Prelude.Just ("Mnc" Data..= mnc),
            Prelude.Just ("EutranCid" Data..= eutranCid)
          ]
      )
