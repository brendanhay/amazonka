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
-- Module      : Amazonka.IoTWireless.Types.CdmaNmrObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.CdmaNmrObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CDMA object for network measurement reports.
--
-- /See:/ 'newCdmaNmrObj' smart constructor.
data CdmaNmrObj = CdmaNmrObj'
  { -- | CDMA base station ID (BSID).
    baseStationId :: Prelude.Maybe Prelude.Natural,
    -- | Transmit power level of the pilot signal, measured in dBm
    -- (decibel-milliwatts).
    pilotPower :: Prelude.Maybe Prelude.Int,
    -- | Pseudo-noise offset, which is a characteristic of the signal from a cell
    -- on a radio tower.
    pnOffset :: Prelude.Natural,
    -- | CDMA channel information.
    cdmaChannel :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CdmaNmrObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseStationId', 'cdmaNmrObj_baseStationId' - CDMA base station ID (BSID).
--
-- 'pilotPower', 'cdmaNmrObj_pilotPower' - Transmit power level of the pilot signal, measured in dBm
-- (decibel-milliwatts).
--
-- 'pnOffset', 'cdmaNmrObj_pnOffset' - Pseudo-noise offset, which is a characteristic of the signal from a cell
-- on a radio tower.
--
-- 'cdmaChannel', 'cdmaNmrObj_cdmaChannel' - CDMA channel information.
newCdmaNmrObj ::
  -- | 'pnOffset'
  Prelude.Natural ->
  -- | 'cdmaChannel'
  Prelude.Natural ->
  CdmaNmrObj
newCdmaNmrObj pPnOffset_ pCdmaChannel_ =
  CdmaNmrObj'
    { baseStationId = Prelude.Nothing,
      pilotPower = Prelude.Nothing,
      pnOffset = pPnOffset_,
      cdmaChannel = pCdmaChannel_
    }

-- | CDMA base station ID (BSID).
cdmaNmrObj_baseStationId :: Lens.Lens' CdmaNmrObj (Prelude.Maybe Prelude.Natural)
cdmaNmrObj_baseStationId = Lens.lens (\CdmaNmrObj' {baseStationId} -> baseStationId) (\s@CdmaNmrObj' {} a -> s {baseStationId = a} :: CdmaNmrObj)

-- | Transmit power level of the pilot signal, measured in dBm
-- (decibel-milliwatts).
cdmaNmrObj_pilotPower :: Lens.Lens' CdmaNmrObj (Prelude.Maybe Prelude.Int)
cdmaNmrObj_pilotPower = Lens.lens (\CdmaNmrObj' {pilotPower} -> pilotPower) (\s@CdmaNmrObj' {} a -> s {pilotPower = a} :: CdmaNmrObj)

-- | Pseudo-noise offset, which is a characteristic of the signal from a cell
-- on a radio tower.
cdmaNmrObj_pnOffset :: Lens.Lens' CdmaNmrObj Prelude.Natural
cdmaNmrObj_pnOffset = Lens.lens (\CdmaNmrObj' {pnOffset} -> pnOffset) (\s@CdmaNmrObj' {} a -> s {pnOffset = a} :: CdmaNmrObj)

-- | CDMA channel information.
cdmaNmrObj_cdmaChannel :: Lens.Lens' CdmaNmrObj Prelude.Natural
cdmaNmrObj_cdmaChannel = Lens.lens (\CdmaNmrObj' {cdmaChannel} -> cdmaChannel) (\s@CdmaNmrObj' {} a -> s {cdmaChannel = a} :: CdmaNmrObj)

instance Prelude.Hashable CdmaNmrObj where
  hashWithSalt _salt CdmaNmrObj' {..} =
    _salt
      `Prelude.hashWithSalt` baseStationId
      `Prelude.hashWithSalt` pilotPower
      `Prelude.hashWithSalt` pnOffset
      `Prelude.hashWithSalt` cdmaChannel

instance Prelude.NFData CdmaNmrObj where
  rnf CdmaNmrObj' {..} =
    Prelude.rnf baseStationId `Prelude.seq`
      Prelude.rnf pilotPower `Prelude.seq`
        Prelude.rnf pnOffset `Prelude.seq`
          Prelude.rnf cdmaChannel

instance Data.ToJSON CdmaNmrObj where
  toJSON CdmaNmrObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseStationId" Data..=) Prelude.<$> baseStationId,
            ("PilotPower" Data..=) Prelude.<$> pilotPower,
            Prelude.Just ("PnOffset" Data..= pnOffset),
            Prelude.Just ("CdmaChannel" Data..= cdmaChannel)
          ]
      )
