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
-- Module      : Amazonka.IoTWireless.Types.CdmaObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.CdmaObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.CdmaLocalId
import Amazonka.IoTWireless.Types.CdmaNmrObj
import qualified Amazonka.Prelude as Prelude

-- | CDMA (Code-division multiple access) object.
--
-- /See:/ 'newCdmaObj' smart constructor.
data CdmaObj = CdmaObj'
  { -- | CDMA base station latitude in degrees.
    baseLat :: Prelude.Maybe Prelude.Double,
    -- | CDMA base station longitude in degrees.
    baseLng :: Prelude.Maybe Prelude.Double,
    -- | CDMA local identification (local ID) parameters.
    cdmaLocalId :: Prelude.Maybe CdmaLocalId,
    -- | CDMA network measurement reports.
    cdmaNmr :: Prelude.Maybe (Prelude.NonEmpty CdmaNmrObj),
    -- | Transmit power level of the pilot signal, measured in dBm
    -- (decibel-milliwatts).
    pilotPower :: Prelude.Maybe Prelude.Int,
    -- | CDMA registration zone (RZ).
    registrationZone :: Prelude.Maybe Prelude.Natural,
    -- | CDMA system ID (SID).
    systemId :: Prelude.Natural,
    -- | CDMA network ID (NID).
    networkId :: Prelude.Natural,
    -- | CDMA base station ID (BSID).
    baseStationId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CdmaObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseLat', 'cdmaObj_baseLat' - CDMA base station latitude in degrees.
--
-- 'baseLng', 'cdmaObj_baseLng' - CDMA base station longitude in degrees.
--
-- 'cdmaLocalId', 'cdmaObj_cdmaLocalId' - CDMA local identification (local ID) parameters.
--
-- 'cdmaNmr', 'cdmaObj_cdmaNmr' - CDMA network measurement reports.
--
-- 'pilotPower', 'cdmaObj_pilotPower' - Transmit power level of the pilot signal, measured in dBm
-- (decibel-milliwatts).
--
-- 'registrationZone', 'cdmaObj_registrationZone' - CDMA registration zone (RZ).
--
-- 'systemId', 'cdmaObj_systemId' - CDMA system ID (SID).
--
-- 'networkId', 'cdmaObj_networkId' - CDMA network ID (NID).
--
-- 'baseStationId', 'cdmaObj_baseStationId' - CDMA base station ID (BSID).
newCdmaObj ::
  -- | 'systemId'
  Prelude.Natural ->
  -- | 'networkId'
  Prelude.Natural ->
  -- | 'baseStationId'
  Prelude.Natural ->
  CdmaObj
newCdmaObj pSystemId_ pNetworkId_ pBaseStationId_ =
  CdmaObj'
    { baseLat = Prelude.Nothing,
      baseLng = Prelude.Nothing,
      cdmaLocalId = Prelude.Nothing,
      cdmaNmr = Prelude.Nothing,
      pilotPower = Prelude.Nothing,
      registrationZone = Prelude.Nothing,
      systemId = pSystemId_,
      networkId = pNetworkId_,
      baseStationId = pBaseStationId_
    }

-- | CDMA base station latitude in degrees.
cdmaObj_baseLat :: Lens.Lens' CdmaObj (Prelude.Maybe Prelude.Double)
cdmaObj_baseLat = Lens.lens (\CdmaObj' {baseLat} -> baseLat) (\s@CdmaObj' {} a -> s {baseLat = a} :: CdmaObj)

-- | CDMA base station longitude in degrees.
cdmaObj_baseLng :: Lens.Lens' CdmaObj (Prelude.Maybe Prelude.Double)
cdmaObj_baseLng = Lens.lens (\CdmaObj' {baseLng} -> baseLng) (\s@CdmaObj' {} a -> s {baseLng = a} :: CdmaObj)

-- | CDMA local identification (local ID) parameters.
cdmaObj_cdmaLocalId :: Lens.Lens' CdmaObj (Prelude.Maybe CdmaLocalId)
cdmaObj_cdmaLocalId = Lens.lens (\CdmaObj' {cdmaLocalId} -> cdmaLocalId) (\s@CdmaObj' {} a -> s {cdmaLocalId = a} :: CdmaObj)

-- | CDMA network measurement reports.
cdmaObj_cdmaNmr :: Lens.Lens' CdmaObj (Prelude.Maybe (Prelude.NonEmpty CdmaNmrObj))
cdmaObj_cdmaNmr = Lens.lens (\CdmaObj' {cdmaNmr} -> cdmaNmr) (\s@CdmaObj' {} a -> s {cdmaNmr = a} :: CdmaObj) Prelude.. Lens.mapping Lens.coerced

-- | Transmit power level of the pilot signal, measured in dBm
-- (decibel-milliwatts).
cdmaObj_pilotPower :: Lens.Lens' CdmaObj (Prelude.Maybe Prelude.Int)
cdmaObj_pilotPower = Lens.lens (\CdmaObj' {pilotPower} -> pilotPower) (\s@CdmaObj' {} a -> s {pilotPower = a} :: CdmaObj)

-- | CDMA registration zone (RZ).
cdmaObj_registrationZone :: Lens.Lens' CdmaObj (Prelude.Maybe Prelude.Natural)
cdmaObj_registrationZone = Lens.lens (\CdmaObj' {registrationZone} -> registrationZone) (\s@CdmaObj' {} a -> s {registrationZone = a} :: CdmaObj)

-- | CDMA system ID (SID).
cdmaObj_systemId :: Lens.Lens' CdmaObj Prelude.Natural
cdmaObj_systemId = Lens.lens (\CdmaObj' {systemId} -> systemId) (\s@CdmaObj' {} a -> s {systemId = a} :: CdmaObj)

-- | CDMA network ID (NID).
cdmaObj_networkId :: Lens.Lens' CdmaObj Prelude.Natural
cdmaObj_networkId = Lens.lens (\CdmaObj' {networkId} -> networkId) (\s@CdmaObj' {} a -> s {networkId = a} :: CdmaObj)

-- | CDMA base station ID (BSID).
cdmaObj_baseStationId :: Lens.Lens' CdmaObj Prelude.Natural
cdmaObj_baseStationId = Lens.lens (\CdmaObj' {baseStationId} -> baseStationId) (\s@CdmaObj' {} a -> s {baseStationId = a} :: CdmaObj)

instance Prelude.Hashable CdmaObj where
  hashWithSalt _salt CdmaObj' {..} =
    _salt
      `Prelude.hashWithSalt` baseLat
      `Prelude.hashWithSalt` baseLng
      `Prelude.hashWithSalt` cdmaLocalId
      `Prelude.hashWithSalt` cdmaNmr
      `Prelude.hashWithSalt` pilotPower
      `Prelude.hashWithSalt` registrationZone
      `Prelude.hashWithSalt` systemId
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` baseStationId

instance Prelude.NFData CdmaObj where
  rnf CdmaObj' {..} =
    Prelude.rnf baseLat
      `Prelude.seq` Prelude.rnf baseLng
      `Prelude.seq` Prelude.rnf cdmaLocalId
      `Prelude.seq` Prelude.rnf cdmaNmr
      `Prelude.seq` Prelude.rnf pilotPower
      `Prelude.seq` Prelude.rnf registrationZone
      `Prelude.seq` Prelude.rnf systemId
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf baseStationId

instance Data.ToJSON CdmaObj where
  toJSON CdmaObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseLat" Data..=) Prelude.<$> baseLat,
            ("BaseLng" Data..=) Prelude.<$> baseLng,
            ("CdmaLocalId" Data..=) Prelude.<$> cdmaLocalId,
            ("CdmaNmr" Data..=) Prelude.<$> cdmaNmr,
            ("PilotPower" Data..=) Prelude.<$> pilotPower,
            ("RegistrationZone" Data..=)
              Prelude.<$> registrationZone,
            Prelude.Just ("SystemId" Data..= systemId),
            Prelude.Just ("NetworkId" Data..= networkId),
            Prelude.Just
              ("BaseStationId" Data..= baseStationId)
          ]
      )
