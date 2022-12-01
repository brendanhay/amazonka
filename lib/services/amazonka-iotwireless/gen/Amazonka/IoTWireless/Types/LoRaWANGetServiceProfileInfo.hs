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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANGetServiceProfileInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANGetServiceProfileInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANGetServiceProfileInfo object.
--
-- /See:/ 'newLoRaWANGetServiceProfileInfo' smart constructor.
data LoRaWANGetServiceProfileInfo = LoRaWANGetServiceProfileInfo'
  { -- | The HRAllowed value that describes whether handover roaming is allowed.
    hrAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The TargetPER value.
    targetPer :: Prelude.Maybe Prelude.Natural,
    -- | The NwkGeoLoc value.
    nwkGeoLoc :: Prelude.Maybe Prelude.Bool,
    -- | The DevStatusReqFreq value.
    devStatusReqFreq :: Prelude.Maybe Prelude.Natural,
    -- | The PRAllowed value that describes whether passive roaming is allowed.
    prAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The DLRatePolicy value.
    dlRatePolicy :: Prelude.Maybe Prelude.Text,
    -- | The AddGWMetaData value.
    addGwMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The ULRatePolicy value.
    ulRatePolicy :: Prelude.Maybe Prelude.Text,
    -- | The ULRate value.
    ulRate :: Prelude.Maybe Prelude.Natural,
    -- | The RAAllowed value that describes whether roaming activation is
    -- allowed.
    raAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The MinGwDiversity value.
    minGwDiversity :: Prelude.Maybe Prelude.Natural,
    -- | The ReportDevStatusMargin value.
    reportDevStatusMargin :: Prelude.Maybe Prelude.Bool,
    -- | The ChannelMask value.
    channelMask :: Prelude.Maybe Prelude.Text,
    -- | The ReportDevStatusBattery value.
    reportDevStatusBattery :: Prelude.Maybe Prelude.Bool,
    -- | The ULBucketSize value.
    ulBucketSize :: Prelude.Maybe Prelude.Natural,
    -- | The DLRate value.
    dlRate :: Prelude.Maybe Prelude.Natural,
    -- | The DRMax value.
    drMax :: Prelude.Maybe Prelude.Natural,
    -- | The DLBucketSize value.
    dlBucketSize :: Prelude.Maybe Prelude.Natural,
    -- | The DRMin value.
    drMin :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANGetServiceProfileInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hrAllowed', 'loRaWANGetServiceProfileInfo_hrAllowed' - The HRAllowed value that describes whether handover roaming is allowed.
--
-- 'targetPer', 'loRaWANGetServiceProfileInfo_targetPer' - The TargetPER value.
--
-- 'nwkGeoLoc', 'loRaWANGetServiceProfileInfo_nwkGeoLoc' - The NwkGeoLoc value.
--
-- 'devStatusReqFreq', 'loRaWANGetServiceProfileInfo_devStatusReqFreq' - The DevStatusReqFreq value.
--
-- 'prAllowed', 'loRaWANGetServiceProfileInfo_prAllowed' - The PRAllowed value that describes whether passive roaming is allowed.
--
-- 'dlRatePolicy', 'loRaWANGetServiceProfileInfo_dlRatePolicy' - The DLRatePolicy value.
--
-- 'addGwMetadata', 'loRaWANGetServiceProfileInfo_addGwMetadata' - The AddGWMetaData value.
--
-- 'ulRatePolicy', 'loRaWANGetServiceProfileInfo_ulRatePolicy' - The ULRatePolicy value.
--
-- 'ulRate', 'loRaWANGetServiceProfileInfo_ulRate' - The ULRate value.
--
-- 'raAllowed', 'loRaWANGetServiceProfileInfo_raAllowed' - The RAAllowed value that describes whether roaming activation is
-- allowed.
--
-- 'minGwDiversity', 'loRaWANGetServiceProfileInfo_minGwDiversity' - The MinGwDiversity value.
--
-- 'reportDevStatusMargin', 'loRaWANGetServiceProfileInfo_reportDevStatusMargin' - The ReportDevStatusMargin value.
--
-- 'channelMask', 'loRaWANGetServiceProfileInfo_channelMask' - The ChannelMask value.
--
-- 'reportDevStatusBattery', 'loRaWANGetServiceProfileInfo_reportDevStatusBattery' - The ReportDevStatusBattery value.
--
-- 'ulBucketSize', 'loRaWANGetServiceProfileInfo_ulBucketSize' - The ULBucketSize value.
--
-- 'dlRate', 'loRaWANGetServiceProfileInfo_dlRate' - The DLRate value.
--
-- 'drMax', 'loRaWANGetServiceProfileInfo_drMax' - The DRMax value.
--
-- 'dlBucketSize', 'loRaWANGetServiceProfileInfo_dlBucketSize' - The DLBucketSize value.
--
-- 'drMin', 'loRaWANGetServiceProfileInfo_drMin' - The DRMin value.
newLoRaWANGetServiceProfileInfo ::
  LoRaWANGetServiceProfileInfo
newLoRaWANGetServiceProfileInfo =
  LoRaWANGetServiceProfileInfo'
    { hrAllowed =
        Prelude.Nothing,
      targetPer = Prelude.Nothing,
      nwkGeoLoc = Prelude.Nothing,
      devStatusReqFreq = Prelude.Nothing,
      prAllowed = Prelude.Nothing,
      dlRatePolicy = Prelude.Nothing,
      addGwMetadata = Prelude.Nothing,
      ulRatePolicy = Prelude.Nothing,
      ulRate = Prelude.Nothing,
      raAllowed = Prelude.Nothing,
      minGwDiversity = Prelude.Nothing,
      reportDevStatusMargin = Prelude.Nothing,
      channelMask = Prelude.Nothing,
      reportDevStatusBattery = Prelude.Nothing,
      ulBucketSize = Prelude.Nothing,
      dlRate = Prelude.Nothing,
      drMax = Prelude.Nothing,
      dlBucketSize = Prelude.Nothing,
      drMin = Prelude.Nothing
    }

-- | The HRAllowed value that describes whether handover roaming is allowed.
loRaWANGetServiceProfileInfo_hrAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_hrAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {hrAllowed} -> hrAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {hrAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The TargetPER value.
loRaWANGetServiceProfileInfo_targetPer :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_targetPer = Lens.lens (\LoRaWANGetServiceProfileInfo' {targetPer} -> targetPer) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {targetPer = a} :: LoRaWANGetServiceProfileInfo)

-- | The NwkGeoLoc value.
loRaWANGetServiceProfileInfo_nwkGeoLoc :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_nwkGeoLoc = Lens.lens (\LoRaWANGetServiceProfileInfo' {nwkGeoLoc} -> nwkGeoLoc) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {nwkGeoLoc = a} :: LoRaWANGetServiceProfileInfo)

-- | The DevStatusReqFreq value.
loRaWANGetServiceProfileInfo_devStatusReqFreq :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_devStatusReqFreq = Lens.lens (\LoRaWANGetServiceProfileInfo' {devStatusReqFreq} -> devStatusReqFreq) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {devStatusReqFreq = a} :: LoRaWANGetServiceProfileInfo)

-- | The PRAllowed value that describes whether passive roaming is allowed.
loRaWANGetServiceProfileInfo_prAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_prAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {prAllowed} -> prAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {prAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLRatePolicy value.
loRaWANGetServiceProfileInfo_dlRatePolicy :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_dlRatePolicy = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlRatePolicy} -> dlRatePolicy) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlRatePolicy = a} :: LoRaWANGetServiceProfileInfo)

-- | The AddGWMetaData value.
loRaWANGetServiceProfileInfo_addGwMetadata :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_addGwMetadata = Lens.lens (\LoRaWANGetServiceProfileInfo' {addGwMetadata} -> addGwMetadata) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {addGwMetadata = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULRatePolicy value.
loRaWANGetServiceProfileInfo_ulRatePolicy :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_ulRatePolicy = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulRatePolicy} -> ulRatePolicy) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulRatePolicy = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULRate value.
loRaWANGetServiceProfileInfo_ulRate :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_ulRate = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulRate} -> ulRate) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulRate = a} :: LoRaWANGetServiceProfileInfo)

-- | The RAAllowed value that describes whether roaming activation is
-- allowed.
loRaWANGetServiceProfileInfo_raAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_raAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {raAllowed} -> raAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {raAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The MinGwDiversity value.
loRaWANGetServiceProfileInfo_minGwDiversity :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_minGwDiversity = Lens.lens (\LoRaWANGetServiceProfileInfo' {minGwDiversity} -> minGwDiversity) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {minGwDiversity = a} :: LoRaWANGetServiceProfileInfo)

-- | The ReportDevStatusMargin value.
loRaWANGetServiceProfileInfo_reportDevStatusMargin :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_reportDevStatusMargin = Lens.lens (\LoRaWANGetServiceProfileInfo' {reportDevStatusMargin} -> reportDevStatusMargin) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {reportDevStatusMargin = a} :: LoRaWANGetServiceProfileInfo)

-- | The ChannelMask value.
loRaWANGetServiceProfileInfo_channelMask :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_channelMask = Lens.lens (\LoRaWANGetServiceProfileInfo' {channelMask} -> channelMask) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {channelMask = a} :: LoRaWANGetServiceProfileInfo)

-- | The ReportDevStatusBattery value.
loRaWANGetServiceProfileInfo_reportDevStatusBattery :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_reportDevStatusBattery = Lens.lens (\LoRaWANGetServiceProfileInfo' {reportDevStatusBattery} -> reportDevStatusBattery) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {reportDevStatusBattery = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULBucketSize value.
loRaWANGetServiceProfileInfo_ulBucketSize :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_ulBucketSize = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulBucketSize} -> ulBucketSize) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulBucketSize = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLRate value.
loRaWANGetServiceProfileInfo_dlRate :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_dlRate = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlRate} -> dlRate) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlRate = a} :: LoRaWANGetServiceProfileInfo)

-- | The DRMax value.
loRaWANGetServiceProfileInfo_drMax :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_drMax = Lens.lens (\LoRaWANGetServiceProfileInfo' {drMax} -> drMax) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {drMax = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLBucketSize value.
loRaWANGetServiceProfileInfo_dlBucketSize :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_dlBucketSize = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlBucketSize} -> dlBucketSize) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlBucketSize = a} :: LoRaWANGetServiceProfileInfo)

-- | The DRMin value.
loRaWANGetServiceProfileInfo_drMin :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_drMin = Lens.lens (\LoRaWANGetServiceProfileInfo' {drMin} -> drMin) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {drMin = a} :: LoRaWANGetServiceProfileInfo)

instance Core.FromJSON LoRaWANGetServiceProfileInfo where
  parseJSON =
    Core.withObject
      "LoRaWANGetServiceProfileInfo"
      ( \x ->
          LoRaWANGetServiceProfileInfo'
            Prelude.<$> (x Core..:? "HrAllowed")
            Prelude.<*> (x Core..:? "TargetPer")
            Prelude.<*> (x Core..:? "NwkGeoLoc")
            Prelude.<*> (x Core..:? "DevStatusReqFreq")
            Prelude.<*> (x Core..:? "PrAllowed")
            Prelude.<*> (x Core..:? "DlRatePolicy")
            Prelude.<*> (x Core..:? "AddGwMetadata")
            Prelude.<*> (x Core..:? "UlRatePolicy")
            Prelude.<*> (x Core..:? "UlRate")
            Prelude.<*> (x Core..:? "RaAllowed")
            Prelude.<*> (x Core..:? "MinGwDiversity")
            Prelude.<*> (x Core..:? "ReportDevStatusMargin")
            Prelude.<*> (x Core..:? "ChannelMask")
            Prelude.<*> (x Core..:? "ReportDevStatusBattery")
            Prelude.<*> (x Core..:? "UlBucketSize")
            Prelude.<*> (x Core..:? "DlRate")
            Prelude.<*> (x Core..:? "DrMax")
            Prelude.<*> (x Core..:? "DlBucketSize")
            Prelude.<*> (x Core..:? "DrMin")
      )

instance
  Prelude.Hashable
    LoRaWANGetServiceProfileInfo
  where
  hashWithSalt _salt LoRaWANGetServiceProfileInfo' {..} =
    _salt `Prelude.hashWithSalt` hrAllowed
      `Prelude.hashWithSalt` targetPer
      `Prelude.hashWithSalt` nwkGeoLoc
      `Prelude.hashWithSalt` devStatusReqFreq
      `Prelude.hashWithSalt` prAllowed
      `Prelude.hashWithSalt` dlRatePolicy
      `Prelude.hashWithSalt` addGwMetadata
      `Prelude.hashWithSalt` ulRatePolicy
      `Prelude.hashWithSalt` ulRate
      `Prelude.hashWithSalt` raAllowed
      `Prelude.hashWithSalt` minGwDiversity
      `Prelude.hashWithSalt` reportDevStatusMargin
      `Prelude.hashWithSalt` channelMask
      `Prelude.hashWithSalt` reportDevStatusBattery
      `Prelude.hashWithSalt` ulBucketSize
      `Prelude.hashWithSalt` dlRate
      `Prelude.hashWithSalt` drMax
      `Prelude.hashWithSalt` dlBucketSize
      `Prelude.hashWithSalt` drMin

instance Prelude.NFData LoRaWANGetServiceProfileInfo where
  rnf LoRaWANGetServiceProfileInfo' {..} =
    Prelude.rnf hrAllowed
      `Prelude.seq` Prelude.rnf targetPer
      `Prelude.seq` Prelude.rnf nwkGeoLoc
      `Prelude.seq` Prelude.rnf devStatusReqFreq
      `Prelude.seq` Prelude.rnf prAllowed
      `Prelude.seq` Prelude.rnf dlRatePolicy
      `Prelude.seq` Prelude.rnf addGwMetadata
      `Prelude.seq` Prelude.rnf ulRatePolicy
      `Prelude.seq` Prelude.rnf ulRate
      `Prelude.seq` Prelude.rnf raAllowed
      `Prelude.seq` Prelude.rnf minGwDiversity
      `Prelude.seq` Prelude.rnf reportDevStatusMargin
      `Prelude.seq` Prelude.rnf channelMask
      `Prelude.seq` Prelude.rnf reportDevStatusBattery
      `Prelude.seq` Prelude.rnf ulBucketSize
      `Prelude.seq` Prelude.rnf dlRate
      `Prelude.seq` Prelude.rnf drMax
      `Prelude.seq` Prelude.rnf dlBucketSize
      `Prelude.seq` Prelude.rnf drMin
