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
-- Module      : Amazonka.IoTWireless.Types.LteNmrObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LteNmrObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | LTE object for network measurement reports.
--
-- /See:/ 'newLteNmrObj' smart constructor.
data LteNmrObj = LteNmrObj'
  { -- | Signal power of the reference signal received, measured in dBm
    -- (decibel-milliwatts).
    rsrp :: Prelude.Maybe Prelude.Int,
    -- | Signal quality of the reference Signal received, measured in decibels
    -- (dB).
    rsrq :: Prelude.Maybe Prelude.Double,
    -- | Physical cell ID.
    pci :: Prelude.Natural,
    -- | E-UTRA (Evolved universal terrestrial Radio Access) absolute radio
    -- frequency channel Number (EARFCN).
    earfcn :: Prelude.Natural,
    -- | E-UTRAN (Evolved Universal Terrestrial Radio Access Network) cell global
    -- identifier (EUTRANCID).
    eutranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LteNmrObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rsrp', 'lteNmrObj_rsrp' - Signal power of the reference signal received, measured in dBm
-- (decibel-milliwatts).
--
-- 'rsrq', 'lteNmrObj_rsrq' - Signal quality of the reference Signal received, measured in decibels
-- (dB).
--
-- 'pci', 'lteNmrObj_pci' - Physical cell ID.
--
-- 'earfcn', 'lteNmrObj_earfcn' - E-UTRA (Evolved universal terrestrial Radio Access) absolute radio
-- frequency channel Number (EARFCN).
--
-- 'eutranCid', 'lteNmrObj_eutranCid' - E-UTRAN (Evolved Universal Terrestrial Radio Access Network) cell global
-- identifier (EUTRANCID).
newLteNmrObj ::
  -- | 'pci'
  Prelude.Natural ->
  -- | 'earfcn'
  Prelude.Natural ->
  -- | 'eutranCid'
  Prelude.Natural ->
  LteNmrObj
newLteNmrObj pPci_ pEarfcn_ pEutranCid_ =
  LteNmrObj'
    { rsrp = Prelude.Nothing,
      rsrq = Prelude.Nothing,
      pci = pPci_,
      earfcn = pEarfcn_,
      eutranCid = pEutranCid_
    }

-- | Signal power of the reference signal received, measured in dBm
-- (decibel-milliwatts).
lteNmrObj_rsrp :: Lens.Lens' LteNmrObj (Prelude.Maybe Prelude.Int)
lteNmrObj_rsrp = Lens.lens (\LteNmrObj' {rsrp} -> rsrp) (\s@LteNmrObj' {} a -> s {rsrp = a} :: LteNmrObj)

-- | Signal quality of the reference Signal received, measured in decibels
-- (dB).
lteNmrObj_rsrq :: Lens.Lens' LteNmrObj (Prelude.Maybe Prelude.Double)
lteNmrObj_rsrq = Lens.lens (\LteNmrObj' {rsrq} -> rsrq) (\s@LteNmrObj' {} a -> s {rsrq = a} :: LteNmrObj)

-- | Physical cell ID.
lteNmrObj_pci :: Lens.Lens' LteNmrObj Prelude.Natural
lteNmrObj_pci = Lens.lens (\LteNmrObj' {pci} -> pci) (\s@LteNmrObj' {} a -> s {pci = a} :: LteNmrObj)

-- | E-UTRA (Evolved universal terrestrial Radio Access) absolute radio
-- frequency channel Number (EARFCN).
lteNmrObj_earfcn :: Lens.Lens' LteNmrObj Prelude.Natural
lteNmrObj_earfcn = Lens.lens (\LteNmrObj' {earfcn} -> earfcn) (\s@LteNmrObj' {} a -> s {earfcn = a} :: LteNmrObj)

-- | E-UTRAN (Evolved Universal Terrestrial Radio Access Network) cell global
-- identifier (EUTRANCID).
lteNmrObj_eutranCid :: Lens.Lens' LteNmrObj Prelude.Natural
lteNmrObj_eutranCid = Lens.lens (\LteNmrObj' {eutranCid} -> eutranCid) (\s@LteNmrObj' {} a -> s {eutranCid = a} :: LteNmrObj)

instance Prelude.Hashable LteNmrObj where
  hashWithSalt _salt LteNmrObj' {..} =
    _salt
      `Prelude.hashWithSalt` rsrp
      `Prelude.hashWithSalt` rsrq
      `Prelude.hashWithSalt` pci
      `Prelude.hashWithSalt` earfcn
      `Prelude.hashWithSalt` eutranCid

instance Prelude.NFData LteNmrObj where
  rnf LteNmrObj' {..} =
    Prelude.rnf rsrp
      `Prelude.seq` Prelude.rnf rsrq
      `Prelude.seq` Prelude.rnf pci
      `Prelude.seq` Prelude.rnf earfcn
      `Prelude.seq` Prelude.rnf eutranCid

instance Data.ToJSON LteNmrObj where
  toJSON LteNmrObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Rsrp" Data..=) Prelude.<$> rsrp,
            ("Rsrq" Data..=) Prelude.<$> rsrq,
            Prelude.Just ("Pci" Data..= pci),
            Prelude.Just ("Earfcn" Data..= earfcn),
            Prelude.Just ("EutranCid" Data..= eutranCid)
          ]
      )
