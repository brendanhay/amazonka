{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.DashPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.DashPackage
  ( DashPackage (..),

    -- * Smart constructor
    mkDashPackage,

    -- * Lenses
    dpAdTriggers,
    dpAdsOnDeliveryRestrictions,
    dpEncryption,
    dpManifestLayout,
    dpManifestWindowSeconds,
    dpMinBufferTimeSeconds,
    dpMinUpdatePeriodSeconds,
    dpPeriodTriggers,
    dpProfile,
    dpSegmentDurationSeconds,
    dpSegmentTemplateFormat,
    dpStreamSelection,
    dpSuggestedPresentationDelaySeconds,
    dpUtcTiming,
    dpUtcTimingUri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.AdTriggersElement as Types
import qualified Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions as Types
import qualified Network.AWS.MediaPackage.Types.DashEncryption as Types
import qualified Network.AWS.MediaPackage.Types.ManifestLayout as Types
import qualified Network.AWS.MediaPackage.Types.PeriodTriggersElement as Types
import qualified Network.AWS.MediaPackage.Types.Profile as Types
import qualified Network.AWS.MediaPackage.Types.SegmentTemplateFormat as Types
import qualified Network.AWS.MediaPackage.Types.StreamSelection as Types
import qualified Network.AWS.MediaPackage.Types.UtcTiming as Types
import qualified Network.AWS.Prelude as Core

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'mkDashPackage' smart constructor.
data DashPackage = DashPackage'
  { adTriggers :: Core.Maybe [Types.AdTriggersElement],
    adsOnDeliveryRestrictions :: Core.Maybe Types.AdsOnDeliveryRestrictions,
    encryption :: Core.Maybe Types.DashEncryption,
    -- | Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
    manifestLayout :: Core.Maybe Types.ManifestLayout,
    -- | Time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Core.Maybe Core.Int,
    -- | Minimum duration (in seconds) that a player will buffer media before starting the presentation.
    minBufferTimeSeconds :: Core.Maybe Core.Int,
    -- | Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
    minUpdatePeriodSeconds :: Core.Maybe Core.Int,
    -- | A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH)
    --
    -- Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not
    -- be partitioned into more than one period. If the list contains "ADS", new periods will be created where
    -- the Channel source contains SCTE-35 ad markers.
    periodTriggers :: Core.Maybe [Types.PeriodTriggersElement],
    -- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
    profile :: Core.Maybe Types.Profile,
    -- | Duration (in seconds) of each segment. Actual segments will be
    --
    -- rounded to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Core.Maybe Core.Int,
    -- | Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
    segmentTemplateFormat :: Core.Maybe Types.SegmentTemplateFormat,
    streamSelection :: Core.Maybe Types.StreamSelection,
    -- | Duration (in seconds) to delay live content before presentation.
    suggestedPresentationDelaySeconds :: Core.Maybe Core.Int,
    -- | Determines the type of UTCTiming included in the Media Presentation Description (MPD)
    utcTiming :: Core.Maybe Types.UtcTiming,
    -- | Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
    utcTimingUri :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DashPackage' value with any optional fields omitted.
mkDashPackage ::
  DashPackage
mkDashPackage =
  DashPackage'
    { adTriggers = Core.Nothing,
      adsOnDeliveryRestrictions = Core.Nothing,
      encryption = Core.Nothing,
      manifestLayout = Core.Nothing,
      manifestWindowSeconds = Core.Nothing,
      minBufferTimeSeconds = Core.Nothing,
      minUpdatePeriodSeconds = Core.Nothing,
      periodTriggers = Core.Nothing,
      profile = Core.Nothing,
      segmentDurationSeconds = Core.Nothing,
      segmentTemplateFormat = Core.Nothing,
      streamSelection = Core.Nothing,
      suggestedPresentationDelaySeconds = Core.Nothing,
      utcTiming = Core.Nothing,
      utcTimingUri = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAdTriggers :: Lens.Lens' DashPackage (Core.Maybe [Types.AdTriggersElement])
dpAdTriggers = Lens.field @"adTriggers"
{-# DEPRECATED dpAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAdsOnDeliveryRestrictions :: Lens.Lens' DashPackage (Core.Maybe Types.AdsOnDeliveryRestrictions)
dpAdsOnDeliveryRestrictions = Lens.field @"adsOnDeliveryRestrictions"
{-# DEPRECATED dpAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpEncryption :: Lens.Lens' DashPackage (Core.Maybe Types.DashEncryption)
dpEncryption = Lens.field @"encryption"
{-# DEPRECATED dpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
--
-- /Note:/ Consider using 'manifestLayout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpManifestLayout :: Lens.Lens' DashPackage (Core.Maybe Types.ManifestLayout)
dpManifestLayout = Lens.field @"manifestLayout"
{-# DEPRECATED dpManifestLayout "Use generic-lens or generic-optics with 'manifestLayout' instead." #-}

-- | Time window (in seconds) contained in each manifest.
--
-- /Note:/ Consider using 'manifestWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpManifestWindowSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dpManifestWindowSeconds = Lens.field @"manifestWindowSeconds"
{-# DEPRECATED dpManifestWindowSeconds "Use generic-lens or generic-optics with 'manifestWindowSeconds' instead." #-}

-- | Minimum duration (in seconds) that a player will buffer media before starting the presentation.
--
-- /Note:/ Consider using 'minBufferTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMinBufferTimeSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dpMinBufferTimeSeconds = Lens.field @"minBufferTimeSeconds"
{-# DEPRECATED dpMinBufferTimeSeconds "Use generic-lens or generic-optics with 'minBufferTimeSeconds' instead." #-}

-- | Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
--
-- /Note:/ Consider using 'minUpdatePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMinUpdatePeriodSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dpMinUpdatePeriodSeconds = Lens.field @"minUpdatePeriodSeconds"
{-# DEPRECATED dpMinUpdatePeriodSeconds "Use generic-lens or generic-optics with 'minUpdatePeriodSeconds' instead." #-}

-- | A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH)
--
-- Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not
-- be partitioned into more than one period. If the list contains "ADS", new periods will be created where
-- the Channel source contains SCTE-35 ad markers.
--
-- /Note:/ Consider using 'periodTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPeriodTriggers :: Lens.Lens' DashPackage (Core.Maybe [Types.PeriodTriggersElement])
dpPeriodTriggers = Lens.field @"periodTriggers"
{-# DEPRECATED dpPeriodTriggers "Use generic-lens or generic-optics with 'periodTriggers' instead." #-}

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProfile :: Lens.Lens' DashPackage (Core.Maybe Types.Profile)
dpProfile = Lens.field @"profile"
{-# DEPRECATED dpProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSegmentDurationSeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dpSegmentDurationSeconds = Lens.field @"segmentDurationSeconds"
{-# DEPRECATED dpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
--
-- /Note:/ Consider using 'segmentTemplateFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSegmentTemplateFormat :: Lens.Lens' DashPackage (Core.Maybe Types.SegmentTemplateFormat)
dpSegmentTemplateFormat = Lens.field @"segmentTemplateFormat"
{-# DEPRECATED dpSegmentTemplateFormat "Use generic-lens or generic-optics with 'segmentTemplateFormat' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpStreamSelection :: Lens.Lens' DashPackage (Core.Maybe Types.StreamSelection)
dpStreamSelection = Lens.field @"streamSelection"
{-# DEPRECATED dpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | Duration (in seconds) to delay live content before presentation.
--
-- /Note:/ Consider using 'suggestedPresentationDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSuggestedPresentationDelaySeconds :: Lens.Lens' DashPackage (Core.Maybe Core.Int)
dpSuggestedPresentationDelaySeconds = Lens.field @"suggestedPresentationDelaySeconds"
{-# DEPRECATED dpSuggestedPresentationDelaySeconds "Use generic-lens or generic-optics with 'suggestedPresentationDelaySeconds' instead." #-}

-- | Determines the type of UTCTiming included in the Media Presentation Description (MPD)
--
-- /Note:/ Consider using 'utcTiming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpUtcTiming :: Lens.Lens' DashPackage (Core.Maybe Types.UtcTiming)
dpUtcTiming = Lens.field @"utcTiming"
{-# DEPRECATED dpUtcTiming "Use generic-lens or generic-optics with 'utcTiming' instead." #-}

-- | Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
--
-- /Note:/ Consider using 'utcTimingUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpUtcTimingUri :: Lens.Lens' DashPackage (Core.Maybe Core.Text)
dpUtcTimingUri = Lens.field @"utcTimingUri"
{-# DEPRECATED dpUtcTimingUri "Use generic-lens or generic-optics with 'utcTimingUri' instead." #-}

instance Core.FromJSON DashPackage where
  toJSON DashPackage {..} =
    Core.object
      ( Core.catMaybes
          [ ("adTriggers" Core..=) Core.<$> adTriggers,
            ("adsOnDeliveryRestrictions" Core..=)
              Core.<$> adsOnDeliveryRestrictions,
            ("encryption" Core..=) Core.<$> encryption,
            ("manifestLayout" Core..=) Core.<$> manifestLayout,
            ("manifestWindowSeconds" Core..=) Core.<$> manifestWindowSeconds,
            ("minBufferTimeSeconds" Core..=) Core.<$> minBufferTimeSeconds,
            ("minUpdatePeriodSeconds" Core..=) Core.<$> minUpdatePeriodSeconds,
            ("periodTriggers" Core..=) Core.<$> periodTriggers,
            ("profile" Core..=) Core.<$> profile,
            ("segmentDurationSeconds" Core..=) Core.<$> segmentDurationSeconds,
            ("segmentTemplateFormat" Core..=) Core.<$> segmentTemplateFormat,
            ("streamSelection" Core..=) Core.<$> streamSelection,
            ("suggestedPresentationDelaySeconds" Core..=)
              Core.<$> suggestedPresentationDelaySeconds,
            ("utcTiming" Core..=) Core.<$> utcTiming,
            ("utcTimingUri" Core..=) Core.<$> utcTimingUri
          ]
      )

instance Core.FromJSON DashPackage where
  parseJSON =
    Core.withObject "DashPackage" Core.$
      \x ->
        DashPackage'
          Core.<$> (x Core..:? "adTriggers")
          Core.<*> (x Core..:? "adsOnDeliveryRestrictions")
          Core.<*> (x Core..:? "encryption")
          Core.<*> (x Core..:? "manifestLayout")
          Core.<*> (x Core..:? "manifestWindowSeconds")
          Core.<*> (x Core..:? "minBufferTimeSeconds")
          Core.<*> (x Core..:? "minUpdatePeriodSeconds")
          Core.<*> (x Core..:? "periodTriggers")
          Core.<*> (x Core..:? "profile")
          Core.<*> (x Core..:? "segmentDurationSeconds")
          Core.<*> (x Core..:? "segmentTemplateFormat")
          Core.<*> (x Core..:? "streamSelection")
          Core.<*> (x Core..:? "suggestedPresentationDelaySeconds")
          Core.<*> (x Core..:? "utcTiming")
          Core.<*> (x Core..:? "utcTimingUri")
