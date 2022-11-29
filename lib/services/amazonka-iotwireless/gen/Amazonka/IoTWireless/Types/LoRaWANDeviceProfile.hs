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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANDeviceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANDeviceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANDeviceProfile object.
--
-- /See:/ 'newLoRaWANDeviceProfile' smart constructor.
data LoRaWANDeviceProfile = LoRaWANDeviceProfile'
  { -- | The RXDataRate2 value.
    rxDataRate2 :: Prelude.Maybe Prelude.Natural,
    -- | The frequency band (RFRegion) value.
    rfRegion :: Prelude.Maybe Prelude.Text,
    -- | The SupportsJoin value.
    supportsJoin :: Prelude.Maybe Prelude.Bool,
    -- | The SupportsClassC value.
    supportsClassC :: Prelude.Maybe Prelude.Bool,
    -- | The MaxEIRP value.
    maxEirp :: Prelude.Maybe Prelude.Natural,
    -- | The list of values that make up the FactoryPresetFreqs value.
    factoryPresetFreqsList :: Prelude.Maybe [Prelude.Natural],
    -- | The MaxDutyCycle value.
    maxDutyCycle :: Prelude.Maybe Prelude.Natural,
    -- | The version of regional parameters.
    regParamsRevision :: Prelude.Maybe Prelude.Text,
    -- | The PingSlotDR value.
    pingSlotDr :: Prelude.Maybe Prelude.Natural,
    -- | The RXDelay1 value.
    rxDelay1 :: Prelude.Maybe Prelude.Natural,
    -- | The ClassBTimeout value.
    classBTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The PingSlotPeriod value.
    pingSlotPeriod :: Prelude.Maybe Prelude.Natural,
    -- | The PingSlotFreq value.
    pingSlotFreq :: Prelude.Maybe Prelude.Natural,
    -- | The ClassCTimeout value.
    classCTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The RXFreq2 value.
    rxFreq2 :: Prelude.Maybe Prelude.Natural,
    -- | The SupportsClassB value.
    supportsClassB :: Prelude.Maybe Prelude.Bool,
    -- | The RXDROffset1 value.
    rxDrOffset1 :: Prelude.Maybe Prelude.Natural,
    -- | The Supports32BitFCnt value.
    supports32BitFCnt :: Prelude.Maybe Prelude.Bool,
    -- | The MAC version (such as OTAA 1.1 or OTAA 1.0.3) to use with this device
    -- profile.
    macVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANDeviceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rxDataRate2', 'loRaWANDeviceProfile_rxDataRate2' - The RXDataRate2 value.
--
-- 'rfRegion', 'loRaWANDeviceProfile_rfRegion' - The frequency band (RFRegion) value.
--
-- 'supportsJoin', 'loRaWANDeviceProfile_supportsJoin' - The SupportsJoin value.
--
-- 'supportsClassC', 'loRaWANDeviceProfile_supportsClassC' - The SupportsClassC value.
--
-- 'maxEirp', 'loRaWANDeviceProfile_maxEirp' - The MaxEIRP value.
--
-- 'factoryPresetFreqsList', 'loRaWANDeviceProfile_factoryPresetFreqsList' - The list of values that make up the FactoryPresetFreqs value.
--
-- 'maxDutyCycle', 'loRaWANDeviceProfile_maxDutyCycle' - The MaxDutyCycle value.
--
-- 'regParamsRevision', 'loRaWANDeviceProfile_regParamsRevision' - The version of regional parameters.
--
-- 'pingSlotDr', 'loRaWANDeviceProfile_pingSlotDr' - The PingSlotDR value.
--
-- 'rxDelay1', 'loRaWANDeviceProfile_rxDelay1' - The RXDelay1 value.
--
-- 'classBTimeout', 'loRaWANDeviceProfile_classBTimeout' - The ClassBTimeout value.
--
-- 'pingSlotPeriod', 'loRaWANDeviceProfile_pingSlotPeriod' - The PingSlotPeriod value.
--
-- 'pingSlotFreq', 'loRaWANDeviceProfile_pingSlotFreq' - The PingSlotFreq value.
--
-- 'classCTimeout', 'loRaWANDeviceProfile_classCTimeout' - The ClassCTimeout value.
--
-- 'rxFreq2', 'loRaWANDeviceProfile_rxFreq2' - The RXFreq2 value.
--
-- 'supportsClassB', 'loRaWANDeviceProfile_supportsClassB' - The SupportsClassB value.
--
-- 'rxDrOffset1', 'loRaWANDeviceProfile_rxDrOffset1' - The RXDROffset1 value.
--
-- 'supports32BitFCnt', 'loRaWANDeviceProfile_supports32BitFCnt' - The Supports32BitFCnt value.
--
-- 'macVersion', 'loRaWANDeviceProfile_macVersion' - The MAC version (such as OTAA 1.1 or OTAA 1.0.3) to use with this device
-- profile.
newLoRaWANDeviceProfile ::
  LoRaWANDeviceProfile
newLoRaWANDeviceProfile =
  LoRaWANDeviceProfile'
    { rxDataRate2 =
        Prelude.Nothing,
      rfRegion = Prelude.Nothing,
      supportsJoin = Prelude.Nothing,
      supportsClassC = Prelude.Nothing,
      maxEirp = Prelude.Nothing,
      factoryPresetFreqsList = Prelude.Nothing,
      maxDutyCycle = Prelude.Nothing,
      regParamsRevision = Prelude.Nothing,
      pingSlotDr = Prelude.Nothing,
      rxDelay1 = Prelude.Nothing,
      classBTimeout = Prelude.Nothing,
      pingSlotPeriod = Prelude.Nothing,
      pingSlotFreq = Prelude.Nothing,
      classCTimeout = Prelude.Nothing,
      rxFreq2 = Prelude.Nothing,
      supportsClassB = Prelude.Nothing,
      rxDrOffset1 = Prelude.Nothing,
      supports32BitFCnt = Prelude.Nothing,
      macVersion = Prelude.Nothing
    }

-- | The RXDataRate2 value.
loRaWANDeviceProfile_rxDataRate2 :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_rxDataRate2 = Lens.lens (\LoRaWANDeviceProfile' {rxDataRate2} -> rxDataRate2) (\s@LoRaWANDeviceProfile' {} a -> s {rxDataRate2 = a} :: LoRaWANDeviceProfile)

-- | The frequency band (RFRegion) value.
loRaWANDeviceProfile_rfRegion :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Text)
loRaWANDeviceProfile_rfRegion = Lens.lens (\LoRaWANDeviceProfile' {rfRegion} -> rfRegion) (\s@LoRaWANDeviceProfile' {} a -> s {rfRegion = a} :: LoRaWANDeviceProfile)

-- | The SupportsJoin value.
loRaWANDeviceProfile_supportsJoin :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Bool)
loRaWANDeviceProfile_supportsJoin = Lens.lens (\LoRaWANDeviceProfile' {supportsJoin} -> supportsJoin) (\s@LoRaWANDeviceProfile' {} a -> s {supportsJoin = a} :: LoRaWANDeviceProfile)

-- | The SupportsClassC value.
loRaWANDeviceProfile_supportsClassC :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Bool)
loRaWANDeviceProfile_supportsClassC = Lens.lens (\LoRaWANDeviceProfile' {supportsClassC} -> supportsClassC) (\s@LoRaWANDeviceProfile' {} a -> s {supportsClassC = a} :: LoRaWANDeviceProfile)

-- | The MaxEIRP value.
loRaWANDeviceProfile_maxEirp :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_maxEirp = Lens.lens (\LoRaWANDeviceProfile' {maxEirp} -> maxEirp) (\s@LoRaWANDeviceProfile' {} a -> s {maxEirp = a} :: LoRaWANDeviceProfile)

-- | The list of values that make up the FactoryPresetFreqs value.
loRaWANDeviceProfile_factoryPresetFreqsList :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe [Prelude.Natural])
loRaWANDeviceProfile_factoryPresetFreqsList = Lens.lens (\LoRaWANDeviceProfile' {factoryPresetFreqsList} -> factoryPresetFreqsList) (\s@LoRaWANDeviceProfile' {} a -> s {factoryPresetFreqsList = a} :: LoRaWANDeviceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The MaxDutyCycle value.
loRaWANDeviceProfile_maxDutyCycle :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_maxDutyCycle = Lens.lens (\LoRaWANDeviceProfile' {maxDutyCycle} -> maxDutyCycle) (\s@LoRaWANDeviceProfile' {} a -> s {maxDutyCycle = a} :: LoRaWANDeviceProfile)

-- | The version of regional parameters.
loRaWANDeviceProfile_regParamsRevision :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Text)
loRaWANDeviceProfile_regParamsRevision = Lens.lens (\LoRaWANDeviceProfile' {regParamsRevision} -> regParamsRevision) (\s@LoRaWANDeviceProfile' {} a -> s {regParamsRevision = a} :: LoRaWANDeviceProfile)

-- | The PingSlotDR value.
loRaWANDeviceProfile_pingSlotDr :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_pingSlotDr = Lens.lens (\LoRaWANDeviceProfile' {pingSlotDr} -> pingSlotDr) (\s@LoRaWANDeviceProfile' {} a -> s {pingSlotDr = a} :: LoRaWANDeviceProfile)

-- | The RXDelay1 value.
loRaWANDeviceProfile_rxDelay1 :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_rxDelay1 = Lens.lens (\LoRaWANDeviceProfile' {rxDelay1} -> rxDelay1) (\s@LoRaWANDeviceProfile' {} a -> s {rxDelay1 = a} :: LoRaWANDeviceProfile)

-- | The ClassBTimeout value.
loRaWANDeviceProfile_classBTimeout :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_classBTimeout = Lens.lens (\LoRaWANDeviceProfile' {classBTimeout} -> classBTimeout) (\s@LoRaWANDeviceProfile' {} a -> s {classBTimeout = a} :: LoRaWANDeviceProfile)

-- | The PingSlotPeriod value.
loRaWANDeviceProfile_pingSlotPeriod :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_pingSlotPeriod = Lens.lens (\LoRaWANDeviceProfile' {pingSlotPeriod} -> pingSlotPeriod) (\s@LoRaWANDeviceProfile' {} a -> s {pingSlotPeriod = a} :: LoRaWANDeviceProfile)

-- | The PingSlotFreq value.
loRaWANDeviceProfile_pingSlotFreq :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_pingSlotFreq = Lens.lens (\LoRaWANDeviceProfile' {pingSlotFreq} -> pingSlotFreq) (\s@LoRaWANDeviceProfile' {} a -> s {pingSlotFreq = a} :: LoRaWANDeviceProfile)

-- | The ClassCTimeout value.
loRaWANDeviceProfile_classCTimeout :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_classCTimeout = Lens.lens (\LoRaWANDeviceProfile' {classCTimeout} -> classCTimeout) (\s@LoRaWANDeviceProfile' {} a -> s {classCTimeout = a} :: LoRaWANDeviceProfile)

-- | The RXFreq2 value.
loRaWANDeviceProfile_rxFreq2 :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_rxFreq2 = Lens.lens (\LoRaWANDeviceProfile' {rxFreq2} -> rxFreq2) (\s@LoRaWANDeviceProfile' {} a -> s {rxFreq2 = a} :: LoRaWANDeviceProfile)

-- | The SupportsClassB value.
loRaWANDeviceProfile_supportsClassB :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Bool)
loRaWANDeviceProfile_supportsClassB = Lens.lens (\LoRaWANDeviceProfile' {supportsClassB} -> supportsClassB) (\s@LoRaWANDeviceProfile' {} a -> s {supportsClassB = a} :: LoRaWANDeviceProfile)

-- | The RXDROffset1 value.
loRaWANDeviceProfile_rxDrOffset1 :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Natural)
loRaWANDeviceProfile_rxDrOffset1 = Lens.lens (\LoRaWANDeviceProfile' {rxDrOffset1} -> rxDrOffset1) (\s@LoRaWANDeviceProfile' {} a -> s {rxDrOffset1 = a} :: LoRaWANDeviceProfile)

-- | The Supports32BitFCnt value.
loRaWANDeviceProfile_supports32BitFCnt :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Bool)
loRaWANDeviceProfile_supports32BitFCnt = Lens.lens (\LoRaWANDeviceProfile' {supports32BitFCnt} -> supports32BitFCnt) (\s@LoRaWANDeviceProfile' {} a -> s {supports32BitFCnt = a} :: LoRaWANDeviceProfile)

-- | The MAC version (such as OTAA 1.1 or OTAA 1.0.3) to use with this device
-- profile.
loRaWANDeviceProfile_macVersion :: Lens.Lens' LoRaWANDeviceProfile (Prelude.Maybe Prelude.Text)
loRaWANDeviceProfile_macVersion = Lens.lens (\LoRaWANDeviceProfile' {macVersion} -> macVersion) (\s@LoRaWANDeviceProfile' {} a -> s {macVersion = a} :: LoRaWANDeviceProfile)

instance Core.FromJSON LoRaWANDeviceProfile where
  parseJSON =
    Core.withObject
      "LoRaWANDeviceProfile"
      ( \x ->
          LoRaWANDeviceProfile'
            Prelude.<$> (x Core..:? "RxDataRate2")
            Prelude.<*> (x Core..:? "RfRegion")
            Prelude.<*> (x Core..:? "SupportsJoin")
            Prelude.<*> (x Core..:? "SupportsClassC")
            Prelude.<*> (x Core..:? "MaxEirp")
            Prelude.<*> ( x Core..:? "FactoryPresetFreqsList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MaxDutyCycle")
            Prelude.<*> (x Core..:? "RegParamsRevision")
            Prelude.<*> (x Core..:? "PingSlotDr")
            Prelude.<*> (x Core..:? "RxDelay1")
            Prelude.<*> (x Core..:? "ClassBTimeout")
            Prelude.<*> (x Core..:? "PingSlotPeriod")
            Prelude.<*> (x Core..:? "PingSlotFreq")
            Prelude.<*> (x Core..:? "ClassCTimeout")
            Prelude.<*> (x Core..:? "RxFreq2")
            Prelude.<*> (x Core..:? "SupportsClassB")
            Prelude.<*> (x Core..:? "RxDrOffset1")
            Prelude.<*> (x Core..:? "Supports32BitFCnt")
            Prelude.<*> (x Core..:? "MacVersion")
      )

instance Prelude.Hashable LoRaWANDeviceProfile where
  hashWithSalt _salt LoRaWANDeviceProfile' {..} =
    _salt `Prelude.hashWithSalt` rxDataRate2
      `Prelude.hashWithSalt` rfRegion
      `Prelude.hashWithSalt` supportsJoin
      `Prelude.hashWithSalt` supportsClassC
      `Prelude.hashWithSalt` maxEirp
      `Prelude.hashWithSalt` factoryPresetFreqsList
      `Prelude.hashWithSalt` maxDutyCycle
      `Prelude.hashWithSalt` regParamsRevision
      `Prelude.hashWithSalt` pingSlotDr
      `Prelude.hashWithSalt` rxDelay1
      `Prelude.hashWithSalt` classBTimeout
      `Prelude.hashWithSalt` pingSlotPeriod
      `Prelude.hashWithSalt` pingSlotFreq
      `Prelude.hashWithSalt` classCTimeout
      `Prelude.hashWithSalt` rxFreq2
      `Prelude.hashWithSalt` supportsClassB
      `Prelude.hashWithSalt` rxDrOffset1
      `Prelude.hashWithSalt` supports32BitFCnt
      `Prelude.hashWithSalt` macVersion

instance Prelude.NFData LoRaWANDeviceProfile where
  rnf LoRaWANDeviceProfile' {..} =
    Prelude.rnf rxDataRate2
      `Prelude.seq` Prelude.rnf rfRegion
      `Prelude.seq` Prelude.rnf supportsJoin
      `Prelude.seq` Prelude.rnf supportsClassC
      `Prelude.seq` Prelude.rnf maxEirp
      `Prelude.seq` Prelude.rnf factoryPresetFreqsList
      `Prelude.seq` Prelude.rnf maxDutyCycle
      `Prelude.seq` Prelude.rnf regParamsRevision
      `Prelude.seq` Prelude.rnf pingSlotDr
      `Prelude.seq` Prelude.rnf rxDelay1
      `Prelude.seq` Prelude.rnf classBTimeout
      `Prelude.seq` Prelude.rnf pingSlotPeriod
      `Prelude.seq` Prelude.rnf pingSlotFreq
      `Prelude.seq` Prelude.rnf classCTimeout
      `Prelude.seq` Prelude.rnf rxFreq2
      `Prelude.seq` Prelude.rnf supportsClassB
      `Prelude.seq` Prelude.rnf rxDrOffset1
      `Prelude.seq` Prelude.rnf supports32BitFCnt
      `Prelude.seq` Prelude.rnf macVersion

instance Core.ToJSON LoRaWANDeviceProfile where
  toJSON LoRaWANDeviceProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RxDataRate2" Core..=) Prelude.<$> rxDataRate2,
            ("RfRegion" Core..=) Prelude.<$> rfRegion,
            ("SupportsJoin" Core..=) Prelude.<$> supportsJoin,
            ("SupportsClassC" Core..=)
              Prelude.<$> supportsClassC,
            ("MaxEirp" Core..=) Prelude.<$> maxEirp,
            ("FactoryPresetFreqsList" Core..=)
              Prelude.<$> factoryPresetFreqsList,
            ("MaxDutyCycle" Core..=) Prelude.<$> maxDutyCycle,
            ("RegParamsRevision" Core..=)
              Prelude.<$> regParamsRevision,
            ("PingSlotDr" Core..=) Prelude.<$> pingSlotDr,
            ("RxDelay1" Core..=) Prelude.<$> rxDelay1,
            ("ClassBTimeout" Core..=) Prelude.<$> classBTimeout,
            ("PingSlotPeriod" Core..=)
              Prelude.<$> pingSlotPeriod,
            ("PingSlotFreq" Core..=) Prelude.<$> pingSlotFreq,
            ("ClassCTimeout" Core..=) Prelude.<$> classCTimeout,
            ("RxFreq2" Core..=) Prelude.<$> rxFreq2,
            ("SupportsClassB" Core..=)
              Prelude.<$> supportsClassB,
            ("RxDrOffset1" Core..=) Prelude.<$> rxDrOffset1,
            ("Supports32BitFCnt" Core..=)
              Prelude.<$> supports32BitFCnt,
            ("MacVersion" Core..=) Prelude.<$> macVersion
          ]
      )
