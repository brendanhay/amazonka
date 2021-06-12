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
-- Module      : Network.AWS.MediaPackage.Types.DashPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.DashPackage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.DashEncryption
import Network.AWS.MediaPackage.Types.ManifestLayout
import Network.AWS.MediaPackage.Types.PeriodTriggersElement
import Network.AWS.MediaPackage.Types.Profile
import Network.AWS.MediaPackage.Types.SegmentTemplateFormat
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.MediaPackage.Types.UtcTiming

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'newDashPackage' smart constructor.
data DashPackage = DashPackage'
  { -- | Minimum duration (in seconds) that a player will buffer media before
    -- starting the presentation.
    minBufferTimeSeconds :: Core.Maybe Core.Int,
    streamSelection :: Core.Maybe StreamSelection,
    -- | A list of triggers that controls when the outgoing Dynamic Adaptive
    -- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
    -- partitioned into multiple periods. If empty, the content will not be
    -- partitioned into more than one period. If the list contains \"ADS\", new
    -- periods will be created where the Channel source contains SCTE-35 ad
    -- markers.
    periodTriggers :: Core.Maybe [PeriodTriggersElement],
    adTriggers :: Core.Maybe [AdTriggersElement],
    -- | Time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Core.Maybe Core.Int,
    -- | Determines the position of some tags in the Media Presentation
    -- Description (MPD). When set to FULL, elements like SegmentTemplate and
    -- ContentProtection are included in each Representation. When set to
    -- COMPACT, duplicate elements are combined and presented at the
    -- AdaptationSet level.
    manifestLayout :: Core.Maybe ManifestLayout,
    -- | Minimum duration (in seconds) between potential changes to the Dynamic
    -- Adaptive Streaming over HTTP (DASH) Media Presentation Description
    -- (MPD).
    minUpdatePeriodSeconds :: Core.Maybe Core.Int,
    encryption :: Core.Maybe DashEncryption,
    adsOnDeliveryRestrictions :: Core.Maybe AdsOnDeliveryRestrictions,
    -- | Specifies the value attribute of the UTCTiming field when utcTiming is
    -- set to HTTP-ISO or HTTP-HEAD
    utcTimingUri :: Core.Maybe Core.Text,
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Core.Maybe Core.Int,
    -- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
    -- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
    profile :: Core.Maybe Profile,
    -- | Determines the type of SegmentTemplate included in the Media
    -- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
    -- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
    -- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
    -- SegmentTemplate, with $Time$ media URLs. When set to
    -- NUMBER_WITH_DURATION, only a duration is included in each
    -- SegmentTemplate, with $Number$ media URLs.
    segmentTemplateFormat :: Core.Maybe SegmentTemplateFormat,
    -- | Duration (in seconds) to delay live content before presentation.
    suggestedPresentationDelaySeconds :: Core.Maybe Core.Int,
    -- | Determines the type of UTCTiming included in the Media Presentation
    -- Description (MPD)
    utcTiming :: Core.Maybe UtcTiming
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DashPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minBufferTimeSeconds', 'dashPackage_minBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
--
-- 'streamSelection', 'dashPackage_streamSelection' - Undocumented member.
--
-- 'periodTriggers', 'dashPackage_periodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Channel source contains SCTE-35 ad
-- markers.
--
-- 'adTriggers', 'dashPackage_adTriggers' - Undocumented member.
--
-- 'manifestWindowSeconds', 'dashPackage_manifestWindowSeconds' - Time window (in seconds) contained in each manifest.
--
-- 'manifestLayout', 'dashPackage_manifestLayout' - Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
--
-- 'minUpdatePeriodSeconds', 'dashPackage_minUpdatePeriodSeconds' - Minimum duration (in seconds) between potential changes to the Dynamic
-- Adaptive Streaming over HTTP (DASH) Media Presentation Description
-- (MPD).
--
-- 'encryption', 'dashPackage_encryption' - Undocumented member.
--
-- 'adsOnDeliveryRestrictions', 'dashPackage_adsOnDeliveryRestrictions' - Undocumented member.
--
-- 'utcTimingUri', 'dashPackage_utcTimingUri' - Specifies the value attribute of the UTCTiming field when utcTiming is
-- set to HTTP-ISO or HTTP-HEAD
--
-- 'segmentDurationSeconds', 'dashPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'profile', 'dashPackage_profile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
--
-- 'segmentTemplateFormat', 'dashPackage_segmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
--
-- 'suggestedPresentationDelaySeconds', 'dashPackage_suggestedPresentationDelaySeconds' - Duration (in seconds) to delay live content before presentation.
--
-- 'utcTiming', 'dashPackage_utcTiming' - Determines the type of UTCTiming included in the Media Presentation
-- Description (MPD)
newDashPackage ::
  DashPackage
newDashPackage =
  DashPackage'
    { minBufferTimeSeconds = Core.Nothing,
      streamSelection = Core.Nothing,
      periodTriggers = Core.Nothing,
      adTriggers = Core.Nothing,
      manifestWindowSeconds = Core.Nothing,
      manifestLayout = Core.Nothing,
      minUpdatePeriodSeconds = Core.Nothing,
      encryption = Core.Nothing,
      adsOnDeliveryRestrictions = Core.Nothing,
      utcTimingUri = Core.Nothing,
      segmentDurationSeconds = Core.Nothing,
      profile = Core.Nothing,
      segmentTemplateFormat = Core.Nothing,
      suggestedPresentationDelaySeconds = Core.Nothing,
      utcTiming = Core.Nothing
    }

-- | Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
dashPackage_minBufferTimeSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dashPackage_minBufferTimeSeconds = Lens.lens (\DashPackage' {minBufferTimeSeconds} -> minBufferTimeSeconds) (\s@DashPackage' {} a -> s {minBufferTimeSeconds = a} :: DashPackage)

-- | Undocumented member.
dashPackage_streamSelection :: Lens.Lens' DashPackage (Core.Maybe StreamSelection)
dashPackage_streamSelection = Lens.lens (\DashPackage' {streamSelection} -> streamSelection) (\s@DashPackage' {} a -> s {streamSelection = a} :: DashPackage)

-- | A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Channel source contains SCTE-35 ad
-- markers.
dashPackage_periodTriggers :: Lens.Lens' DashPackage (Core.Maybe [PeriodTriggersElement])
dashPackage_periodTriggers = Lens.lens (\DashPackage' {periodTriggers} -> periodTriggers) (\s@DashPackage' {} a -> s {periodTriggers = a} :: DashPackage) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
dashPackage_adTriggers :: Lens.Lens' DashPackage (Core.Maybe [AdTriggersElement])
dashPackage_adTriggers = Lens.lens (\DashPackage' {adTriggers} -> adTriggers) (\s@DashPackage' {} a -> s {adTriggers = a} :: DashPackage) Core.. Lens.mapping Lens._Coerce

-- | Time window (in seconds) contained in each manifest.
dashPackage_manifestWindowSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dashPackage_manifestWindowSeconds = Lens.lens (\DashPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@DashPackage' {} a -> s {manifestWindowSeconds = a} :: DashPackage)

-- | Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
dashPackage_manifestLayout :: Lens.Lens' DashPackage (Core.Maybe ManifestLayout)
dashPackage_manifestLayout = Lens.lens (\DashPackage' {manifestLayout} -> manifestLayout) (\s@DashPackage' {} a -> s {manifestLayout = a} :: DashPackage)

-- | Minimum duration (in seconds) between potential changes to the Dynamic
-- Adaptive Streaming over HTTP (DASH) Media Presentation Description
-- (MPD).
dashPackage_minUpdatePeriodSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dashPackage_minUpdatePeriodSeconds = Lens.lens (\DashPackage' {minUpdatePeriodSeconds} -> minUpdatePeriodSeconds) (\s@DashPackage' {} a -> s {minUpdatePeriodSeconds = a} :: DashPackage)

-- | Undocumented member.
dashPackage_encryption :: Lens.Lens' DashPackage (Core.Maybe DashEncryption)
dashPackage_encryption = Lens.lens (\DashPackage' {encryption} -> encryption) (\s@DashPackage' {} a -> s {encryption = a} :: DashPackage)

-- | Undocumented member.
dashPackage_adsOnDeliveryRestrictions :: Lens.Lens' DashPackage (Core.Maybe AdsOnDeliveryRestrictions)
dashPackage_adsOnDeliveryRestrictions = Lens.lens (\DashPackage' {adsOnDeliveryRestrictions} -> adsOnDeliveryRestrictions) (\s@DashPackage' {} a -> s {adsOnDeliveryRestrictions = a} :: DashPackage)

-- | Specifies the value attribute of the UTCTiming field when utcTiming is
-- set to HTTP-ISO or HTTP-HEAD
dashPackage_utcTimingUri :: Lens.Lens' DashPackage (Core.Maybe Core.Text)
dashPackage_utcTimingUri = Lens.lens (\DashPackage' {utcTimingUri} -> utcTimingUri) (\s@DashPackage' {} a -> s {utcTimingUri = a} :: DashPackage)

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
dashPackage_segmentDurationSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dashPackage_segmentDurationSeconds = Lens.lens (\DashPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@DashPackage' {} a -> s {segmentDurationSeconds = a} :: DashPackage)

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
dashPackage_profile :: Lens.Lens' DashPackage (Core.Maybe Profile)
dashPackage_profile = Lens.lens (\DashPackage' {profile} -> profile) (\s@DashPackage' {} a -> s {profile = a} :: DashPackage)

-- | Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
dashPackage_segmentTemplateFormat :: Lens.Lens' DashPackage (Core.Maybe SegmentTemplateFormat)
dashPackage_segmentTemplateFormat = Lens.lens (\DashPackage' {segmentTemplateFormat} -> segmentTemplateFormat) (\s@DashPackage' {} a -> s {segmentTemplateFormat = a} :: DashPackage)

-- | Duration (in seconds) to delay live content before presentation.
dashPackage_suggestedPresentationDelaySeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dashPackage_suggestedPresentationDelaySeconds = Lens.lens (\DashPackage' {suggestedPresentationDelaySeconds} -> suggestedPresentationDelaySeconds) (\s@DashPackage' {} a -> s {suggestedPresentationDelaySeconds = a} :: DashPackage)

-- | Determines the type of UTCTiming included in the Media Presentation
-- Description (MPD)
dashPackage_utcTiming :: Lens.Lens' DashPackage (Core.Maybe UtcTiming)
dashPackage_utcTiming = Lens.lens (\DashPackage' {utcTiming} -> utcTiming) (\s@DashPackage' {} a -> s {utcTiming = a} :: DashPackage)

instance Core.FromJSON DashPackage where
  parseJSON =
    Core.withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            Core.<$> (x Core..:? "minBufferTimeSeconds")
            Core.<*> (x Core..:? "streamSelection")
            Core.<*> (x Core..:? "periodTriggers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "adTriggers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "manifestWindowSeconds")
            Core.<*> (x Core..:? "manifestLayout")
            Core.<*> (x Core..:? "minUpdatePeriodSeconds")
            Core.<*> (x Core..:? "encryption")
            Core.<*> (x Core..:? "adsOnDeliveryRestrictions")
            Core.<*> (x Core..:? "utcTimingUri")
            Core.<*> (x Core..:? "segmentDurationSeconds")
            Core.<*> (x Core..:? "profile")
            Core.<*> (x Core..:? "segmentTemplateFormat")
            Core.<*> (x Core..:? "suggestedPresentationDelaySeconds")
            Core.<*> (x Core..:? "utcTiming")
      )

instance Core.Hashable DashPackage

instance Core.NFData DashPackage

instance Core.ToJSON DashPackage where
  toJSON DashPackage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("minBufferTimeSeconds" Core..=)
              Core.<$> minBufferTimeSeconds,
            ("streamSelection" Core..=) Core.<$> streamSelection,
            ("periodTriggers" Core..=) Core.<$> periodTriggers,
            ("adTriggers" Core..=) Core.<$> adTriggers,
            ("manifestWindowSeconds" Core..=)
              Core.<$> manifestWindowSeconds,
            ("manifestLayout" Core..=) Core.<$> manifestLayout,
            ("minUpdatePeriodSeconds" Core..=)
              Core.<$> minUpdatePeriodSeconds,
            ("encryption" Core..=) Core.<$> encryption,
            ("adsOnDeliveryRestrictions" Core..=)
              Core.<$> adsOnDeliveryRestrictions,
            ("utcTimingUri" Core..=) Core.<$> utcTimingUri,
            ("segmentDurationSeconds" Core..=)
              Core.<$> segmentDurationSeconds,
            ("profile" Core..=) Core.<$> profile,
            ("segmentTemplateFormat" Core..=)
              Core.<$> segmentTemplateFormat,
            ("suggestedPresentationDelaySeconds" Core..=)
              Core.<$> suggestedPresentationDelaySeconds,
            ("utcTiming" Core..=) Core.<$> utcTiming
          ]
      )
