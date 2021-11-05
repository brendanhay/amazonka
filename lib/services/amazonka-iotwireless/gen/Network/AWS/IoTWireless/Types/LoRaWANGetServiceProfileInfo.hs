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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANGetServiceProfileInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANGetServiceProfileInfo object.
--
-- /See:/ 'newLoRaWANGetServiceProfileInfo' smart constructor.
data LoRaWANGetServiceProfileInfo = LoRaWANGetServiceProfileInfo'
  { -- | The AddGWMetaData value.
    addGwMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The PRAllowed value that describes whether passive roaming is allowed.
    prAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The DRMax value.
    drMax :: Prelude.Maybe Prelude.Natural,
    -- | The DLRate value.
    dlRate :: Prelude.Maybe Prelude.Natural,
    -- | The DevStatusReqFreq value.
    devStatusReqFreq :: Prelude.Maybe Prelude.Natural,
    -- | The ReportDevStatusBattery value.
    reportDevStatusBattery :: Prelude.Maybe Prelude.Bool,
    -- | The DLBucketSize value.
    dlBucketSize :: Prelude.Maybe Prelude.Natural,
    -- | The ULRatePolicy value.
    ulRatePolicy :: Prelude.Maybe Prelude.Text,
    -- | The TargetPER value.
    targetPer :: Prelude.Maybe Prelude.Natural,
    -- | The DRMin value.
    drMin :: Prelude.Maybe Prelude.Natural,
    -- | The NwkGeoLoc value.
    nwkGeoLoc :: Prelude.Maybe Prelude.Bool,
    -- | The ChannelMask value.
    channelMask :: Prelude.Maybe Prelude.Text,
    -- | The DLRatePolicy value.
    dlRatePolicy :: Prelude.Maybe Prelude.Text,
    -- | The HRAllowed value that describes whether handover roaming is allowed.
    hrAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The ULBucketSize value.
    ulBucketSize :: Prelude.Maybe Prelude.Natural,
    -- | The ULRate value.
    ulRate :: Prelude.Maybe Prelude.Natural,
    -- | The ReportDevStatusMargin value.
    reportDevStatusMargin :: Prelude.Maybe Prelude.Bool,
    -- | The RAAllowed value that describes whether roaming activation is
    -- allowed.
    raAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The MinGwDiversity value.
    minGwDiversity :: Prelude.Maybe Prelude.Natural
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
-- 'addGwMetadata', 'loRaWANGetServiceProfileInfo_addGwMetadata' - The AddGWMetaData value.
--
-- 'prAllowed', 'loRaWANGetServiceProfileInfo_prAllowed' - The PRAllowed value that describes whether passive roaming is allowed.
--
-- 'drMax', 'loRaWANGetServiceProfileInfo_drMax' - The DRMax value.
--
-- 'dlRate', 'loRaWANGetServiceProfileInfo_dlRate' - The DLRate value.
--
-- 'devStatusReqFreq', 'loRaWANGetServiceProfileInfo_devStatusReqFreq' - The DevStatusReqFreq value.
--
-- 'reportDevStatusBattery', 'loRaWANGetServiceProfileInfo_reportDevStatusBattery' - The ReportDevStatusBattery value.
--
-- 'dlBucketSize', 'loRaWANGetServiceProfileInfo_dlBucketSize' - The DLBucketSize value.
--
-- 'ulRatePolicy', 'loRaWANGetServiceProfileInfo_ulRatePolicy' - The ULRatePolicy value.
--
-- 'targetPer', 'loRaWANGetServiceProfileInfo_targetPer' - The TargetPER value.
--
-- 'drMin', 'loRaWANGetServiceProfileInfo_drMin' - The DRMin value.
--
-- 'nwkGeoLoc', 'loRaWANGetServiceProfileInfo_nwkGeoLoc' - The NwkGeoLoc value.
--
-- 'channelMask', 'loRaWANGetServiceProfileInfo_channelMask' - The ChannelMask value.
--
-- 'dlRatePolicy', 'loRaWANGetServiceProfileInfo_dlRatePolicy' - The DLRatePolicy value.
--
-- 'hrAllowed', 'loRaWANGetServiceProfileInfo_hrAllowed' - The HRAllowed value that describes whether handover roaming is allowed.
--
-- 'ulBucketSize', 'loRaWANGetServiceProfileInfo_ulBucketSize' - The ULBucketSize value.
--
-- 'ulRate', 'loRaWANGetServiceProfileInfo_ulRate' - The ULRate value.
--
-- 'reportDevStatusMargin', 'loRaWANGetServiceProfileInfo_reportDevStatusMargin' - The ReportDevStatusMargin value.
--
-- 'raAllowed', 'loRaWANGetServiceProfileInfo_raAllowed' - The RAAllowed value that describes whether roaming activation is
-- allowed.
--
-- 'minGwDiversity', 'loRaWANGetServiceProfileInfo_minGwDiversity' - The MinGwDiversity value.
newLoRaWANGetServiceProfileInfo ::
  LoRaWANGetServiceProfileInfo
newLoRaWANGetServiceProfileInfo =
  LoRaWANGetServiceProfileInfo'
    { addGwMetadata =
        Prelude.Nothing,
      prAllowed = Prelude.Nothing,
      drMax = Prelude.Nothing,
      dlRate = Prelude.Nothing,
      devStatusReqFreq = Prelude.Nothing,
      reportDevStatusBattery = Prelude.Nothing,
      dlBucketSize = Prelude.Nothing,
      ulRatePolicy = Prelude.Nothing,
      targetPer = Prelude.Nothing,
      drMin = Prelude.Nothing,
      nwkGeoLoc = Prelude.Nothing,
      channelMask = Prelude.Nothing,
      dlRatePolicy = Prelude.Nothing,
      hrAllowed = Prelude.Nothing,
      ulBucketSize = Prelude.Nothing,
      ulRate = Prelude.Nothing,
      reportDevStatusMargin = Prelude.Nothing,
      raAllowed = Prelude.Nothing,
      minGwDiversity = Prelude.Nothing
    }

-- | The AddGWMetaData value.
loRaWANGetServiceProfileInfo_addGwMetadata :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_addGwMetadata = Lens.lens (\LoRaWANGetServiceProfileInfo' {addGwMetadata} -> addGwMetadata) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {addGwMetadata = a} :: LoRaWANGetServiceProfileInfo)

-- | The PRAllowed value that describes whether passive roaming is allowed.
loRaWANGetServiceProfileInfo_prAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_prAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {prAllowed} -> prAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {prAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The DRMax value.
loRaWANGetServiceProfileInfo_drMax :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_drMax = Lens.lens (\LoRaWANGetServiceProfileInfo' {drMax} -> drMax) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {drMax = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLRate value.
loRaWANGetServiceProfileInfo_dlRate :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_dlRate = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlRate} -> dlRate) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlRate = a} :: LoRaWANGetServiceProfileInfo)

-- | The DevStatusReqFreq value.
loRaWANGetServiceProfileInfo_devStatusReqFreq :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_devStatusReqFreq = Lens.lens (\LoRaWANGetServiceProfileInfo' {devStatusReqFreq} -> devStatusReqFreq) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {devStatusReqFreq = a} :: LoRaWANGetServiceProfileInfo)

-- | The ReportDevStatusBattery value.
loRaWANGetServiceProfileInfo_reportDevStatusBattery :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_reportDevStatusBattery = Lens.lens (\LoRaWANGetServiceProfileInfo' {reportDevStatusBattery} -> reportDevStatusBattery) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {reportDevStatusBattery = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLBucketSize value.
loRaWANGetServiceProfileInfo_dlBucketSize :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_dlBucketSize = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlBucketSize} -> dlBucketSize) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlBucketSize = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULRatePolicy value.
loRaWANGetServiceProfileInfo_ulRatePolicy :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_ulRatePolicy = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulRatePolicy} -> ulRatePolicy) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulRatePolicy = a} :: LoRaWANGetServiceProfileInfo)

-- | The TargetPER value.
loRaWANGetServiceProfileInfo_targetPer :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_targetPer = Lens.lens (\LoRaWANGetServiceProfileInfo' {targetPer} -> targetPer) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {targetPer = a} :: LoRaWANGetServiceProfileInfo)

-- | The DRMin value.
loRaWANGetServiceProfileInfo_drMin :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_drMin = Lens.lens (\LoRaWANGetServiceProfileInfo' {drMin} -> drMin) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {drMin = a} :: LoRaWANGetServiceProfileInfo)

-- | The NwkGeoLoc value.
loRaWANGetServiceProfileInfo_nwkGeoLoc :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_nwkGeoLoc = Lens.lens (\LoRaWANGetServiceProfileInfo' {nwkGeoLoc} -> nwkGeoLoc) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {nwkGeoLoc = a} :: LoRaWANGetServiceProfileInfo)

-- | The ChannelMask value.
loRaWANGetServiceProfileInfo_channelMask :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_channelMask = Lens.lens (\LoRaWANGetServiceProfileInfo' {channelMask} -> channelMask) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {channelMask = a} :: LoRaWANGetServiceProfileInfo)

-- | The DLRatePolicy value.
loRaWANGetServiceProfileInfo_dlRatePolicy :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Text)
loRaWANGetServiceProfileInfo_dlRatePolicy = Lens.lens (\LoRaWANGetServiceProfileInfo' {dlRatePolicy} -> dlRatePolicy) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {dlRatePolicy = a} :: LoRaWANGetServiceProfileInfo)

-- | The HRAllowed value that describes whether handover roaming is allowed.
loRaWANGetServiceProfileInfo_hrAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_hrAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {hrAllowed} -> hrAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {hrAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULBucketSize value.
loRaWANGetServiceProfileInfo_ulBucketSize :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_ulBucketSize = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulBucketSize} -> ulBucketSize) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulBucketSize = a} :: LoRaWANGetServiceProfileInfo)

-- | The ULRate value.
loRaWANGetServiceProfileInfo_ulRate :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_ulRate = Lens.lens (\LoRaWANGetServiceProfileInfo' {ulRate} -> ulRate) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {ulRate = a} :: LoRaWANGetServiceProfileInfo)

-- | The ReportDevStatusMargin value.
loRaWANGetServiceProfileInfo_reportDevStatusMargin :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_reportDevStatusMargin = Lens.lens (\LoRaWANGetServiceProfileInfo' {reportDevStatusMargin} -> reportDevStatusMargin) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {reportDevStatusMargin = a} :: LoRaWANGetServiceProfileInfo)

-- | The RAAllowed value that describes whether roaming activation is
-- allowed.
loRaWANGetServiceProfileInfo_raAllowed :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Bool)
loRaWANGetServiceProfileInfo_raAllowed = Lens.lens (\LoRaWANGetServiceProfileInfo' {raAllowed} -> raAllowed) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {raAllowed = a} :: LoRaWANGetServiceProfileInfo)

-- | The MinGwDiversity value.
loRaWANGetServiceProfileInfo_minGwDiversity :: Lens.Lens' LoRaWANGetServiceProfileInfo (Prelude.Maybe Prelude.Natural)
loRaWANGetServiceProfileInfo_minGwDiversity = Lens.lens (\LoRaWANGetServiceProfileInfo' {minGwDiversity} -> minGwDiversity) (\s@LoRaWANGetServiceProfileInfo' {} a -> s {minGwDiversity = a} :: LoRaWANGetServiceProfileInfo)

instance Core.FromJSON LoRaWANGetServiceProfileInfo where
  parseJSON =
    Core.withObject
      "LoRaWANGetServiceProfileInfo"
      ( \x ->
          LoRaWANGetServiceProfileInfo'
            Prelude.<$> (x Core..:? "AddGwMetadata")
            Prelude.<*> (x Core..:? "PrAllowed")
            Prelude.<*> (x Core..:? "DrMax")
            Prelude.<*> (x Core..:? "DlRate")
            Prelude.<*> (x Core..:? "DevStatusReqFreq")
            Prelude.<*> (x Core..:? "ReportDevStatusBattery")
            Prelude.<*> (x Core..:? "DlBucketSize")
            Prelude.<*> (x Core..:? "UlRatePolicy")
            Prelude.<*> (x Core..:? "TargetPer")
            Prelude.<*> (x Core..:? "DrMin")
            Prelude.<*> (x Core..:? "NwkGeoLoc")
            Prelude.<*> (x Core..:? "ChannelMask")
            Prelude.<*> (x Core..:? "DlRatePolicy")
            Prelude.<*> (x Core..:? "HrAllowed")
            Prelude.<*> (x Core..:? "UlBucketSize")
            Prelude.<*> (x Core..:? "UlRate")
            Prelude.<*> (x Core..:? "ReportDevStatusMargin")
            Prelude.<*> (x Core..:? "RaAllowed")
            Prelude.<*> (x Core..:? "MinGwDiversity")
      )

instance
  Prelude.Hashable
    LoRaWANGetServiceProfileInfo

instance Prelude.NFData LoRaWANGetServiceProfileInfo
