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
    dpAdsOnDeliveryRestrictions,
    dpMinBufferTimeSeconds,
    dpUtcTiming,
    dpSegmentTemplateFormat,
    dpProfile,
    dpSegmentDurationSeconds,
    dpUtcTimingURI,
    dpStreamSelection,
    dpEncryption,
    dpMinUpdatePeriodSeconds,
    dpManifestLayout,
    dpSuggestedPresentationDelaySeconds,
    dpManifestWindowSeconds,
    dpAdTriggers,
    dpPeriodTriggers,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'mkDashPackage' smart constructor.
data DashPackage = DashPackage'
  { adsOnDeliveryRestrictions ::
      Lude.Maybe AdsOnDeliveryRestrictions,
    minBufferTimeSeconds :: Lude.Maybe Lude.Int,
    utcTiming :: Lude.Maybe UtcTiming,
    segmentTemplateFormat :: Lude.Maybe SegmentTemplateFormat,
    profile :: Lude.Maybe Profile,
    segmentDurationSeconds :: Lude.Maybe Lude.Int,
    utcTimingURI :: Lude.Maybe Lude.Text,
    streamSelection :: Lude.Maybe StreamSelection,
    encryption :: Lude.Maybe DashEncryption,
    minUpdatePeriodSeconds :: Lude.Maybe Lude.Int,
    manifestLayout :: Lude.Maybe ManifestLayout,
    suggestedPresentationDelaySeconds :: Lude.Maybe Lude.Int,
    manifestWindowSeconds :: Lude.Maybe Lude.Int,
    adTriggers :: Lude.Maybe [AdTriggersElement],
    periodTriggers :: Lude.Maybe [PeriodTriggersElement]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashPackage' with the minimum fields required to make a request.
--
-- * 'adTriggers' - Undocumented field.
-- * 'adsOnDeliveryRestrictions' - Undocumented field.
-- * 'encryption' - Undocumented field.
-- * 'manifestLayout' - Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
-- * 'manifestWindowSeconds' - Time window (in seconds) contained in each manifest.
-- * 'minBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before starting the presentation.
-- * 'minUpdatePeriodSeconds' - Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
-- * 'periodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH)
--
-- Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not
-- be partitioned into more than one period. If the list contains "ADS", new periods will be created where
-- the Channel source contains SCTE-35 ad markers.
-- * 'profile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
-- * 'segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
-- * 'segmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
-- * 'streamSelection' - Undocumented field.
-- * 'suggestedPresentationDelaySeconds' - Duration (in seconds) to delay live content before presentation.
-- * 'utcTiming' - Determines the type of UTCTiming included in the Media Presentation Description (MPD)
-- * 'utcTimingURI' - Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
mkDashPackage ::
  DashPackage
mkDashPackage =
  DashPackage'
    { adsOnDeliveryRestrictions = Lude.Nothing,
      minBufferTimeSeconds = Lude.Nothing,
      utcTiming = Lude.Nothing,
      segmentTemplateFormat = Lude.Nothing,
      profile = Lude.Nothing,
      segmentDurationSeconds = Lude.Nothing,
      utcTimingURI = Lude.Nothing,
      streamSelection = Lude.Nothing,
      encryption = Lude.Nothing,
      minUpdatePeriodSeconds = Lude.Nothing,
      manifestLayout = Lude.Nothing,
      suggestedPresentationDelaySeconds = Lude.Nothing,
      manifestWindowSeconds = Lude.Nothing,
      adTriggers = Lude.Nothing,
      periodTriggers = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAdsOnDeliveryRestrictions :: Lens.Lens' DashPackage (Lude.Maybe AdsOnDeliveryRestrictions)
dpAdsOnDeliveryRestrictions = Lens.lens (adsOnDeliveryRestrictions :: DashPackage -> Lude.Maybe AdsOnDeliveryRestrictions) (\s a -> s {adsOnDeliveryRestrictions = a} :: DashPackage)
{-# DEPRECATED dpAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | Minimum duration (in seconds) that a player will buffer media before starting the presentation.
--
-- /Note:/ Consider using 'minBufferTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMinBufferTimeSeconds :: Lens.Lens' DashPackage (Lude.Maybe Lude.Int)
dpMinBufferTimeSeconds = Lens.lens (minBufferTimeSeconds :: DashPackage -> Lude.Maybe Lude.Int) (\s a -> s {minBufferTimeSeconds = a} :: DashPackage)
{-# DEPRECATED dpMinBufferTimeSeconds "Use generic-lens or generic-optics with 'minBufferTimeSeconds' instead." #-}

-- | Determines the type of UTCTiming included in the Media Presentation Description (MPD)
--
-- /Note:/ Consider using 'utcTiming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpUtcTiming :: Lens.Lens' DashPackage (Lude.Maybe UtcTiming)
dpUtcTiming = Lens.lens (utcTiming :: DashPackage -> Lude.Maybe UtcTiming) (\s a -> s {utcTiming = a} :: DashPackage)
{-# DEPRECATED dpUtcTiming "Use generic-lens or generic-optics with 'utcTiming' instead." #-}

-- | Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
--
-- /Note:/ Consider using 'segmentTemplateFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSegmentTemplateFormat :: Lens.Lens' DashPackage (Lude.Maybe SegmentTemplateFormat)
dpSegmentTemplateFormat = Lens.lens (segmentTemplateFormat :: DashPackage -> Lude.Maybe SegmentTemplateFormat) (\s a -> s {segmentTemplateFormat = a} :: DashPackage)
{-# DEPRECATED dpSegmentTemplateFormat "Use generic-lens or generic-optics with 'segmentTemplateFormat' instead." #-}

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProfile :: Lens.Lens' DashPackage (Lude.Maybe Profile)
dpProfile = Lens.lens (profile :: DashPackage -> Lude.Maybe Profile) (\s a -> s {profile = a} :: DashPackage)
{-# DEPRECATED dpProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSegmentDurationSeconds :: Lens.Lens' DashPackage (Lude.Maybe Lude.Int)
dpSegmentDurationSeconds = Lens.lens (segmentDurationSeconds :: DashPackage -> Lude.Maybe Lude.Int) (\s a -> s {segmentDurationSeconds = a} :: DashPackage)
{-# DEPRECATED dpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
--
-- /Note:/ Consider using 'utcTimingURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpUtcTimingURI :: Lens.Lens' DashPackage (Lude.Maybe Lude.Text)
dpUtcTimingURI = Lens.lens (utcTimingURI :: DashPackage -> Lude.Maybe Lude.Text) (\s a -> s {utcTimingURI = a} :: DashPackage)
{-# DEPRECATED dpUtcTimingURI "Use generic-lens or generic-optics with 'utcTimingURI' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpStreamSelection :: Lens.Lens' DashPackage (Lude.Maybe StreamSelection)
dpStreamSelection = Lens.lens (streamSelection :: DashPackage -> Lude.Maybe StreamSelection) (\s a -> s {streamSelection = a} :: DashPackage)
{-# DEPRECATED dpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpEncryption :: Lens.Lens' DashPackage (Lude.Maybe DashEncryption)
dpEncryption = Lens.lens (encryption :: DashPackage -> Lude.Maybe DashEncryption) (\s a -> s {encryption = a} :: DashPackage)
{-# DEPRECATED dpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
--
-- /Note:/ Consider using 'minUpdatePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMinUpdatePeriodSeconds :: Lens.Lens' DashPackage (Lude.Maybe Lude.Int)
dpMinUpdatePeriodSeconds = Lens.lens (minUpdatePeriodSeconds :: DashPackage -> Lude.Maybe Lude.Int) (\s a -> s {minUpdatePeriodSeconds = a} :: DashPackage)
{-# DEPRECATED dpMinUpdatePeriodSeconds "Use generic-lens or generic-optics with 'minUpdatePeriodSeconds' instead." #-}

-- | Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
--
-- /Note:/ Consider using 'manifestLayout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpManifestLayout :: Lens.Lens' DashPackage (Lude.Maybe ManifestLayout)
dpManifestLayout = Lens.lens (manifestLayout :: DashPackage -> Lude.Maybe ManifestLayout) (\s a -> s {manifestLayout = a} :: DashPackage)
{-# DEPRECATED dpManifestLayout "Use generic-lens or generic-optics with 'manifestLayout' instead." #-}

-- | Duration (in seconds) to delay live content before presentation.
--
-- /Note:/ Consider using 'suggestedPresentationDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSuggestedPresentationDelaySeconds :: Lens.Lens' DashPackage (Lude.Maybe Lude.Int)
dpSuggestedPresentationDelaySeconds = Lens.lens (suggestedPresentationDelaySeconds :: DashPackage -> Lude.Maybe Lude.Int) (\s a -> s {suggestedPresentationDelaySeconds = a} :: DashPackage)
{-# DEPRECATED dpSuggestedPresentationDelaySeconds "Use generic-lens or generic-optics with 'suggestedPresentationDelaySeconds' instead." #-}

-- | Time window (in seconds) contained in each manifest.
--
-- /Note:/ Consider using 'manifestWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpManifestWindowSeconds :: Lens.Lens' DashPackage (Lude.Maybe Lude.Int)
dpManifestWindowSeconds = Lens.lens (manifestWindowSeconds :: DashPackage -> Lude.Maybe Lude.Int) (\s a -> s {manifestWindowSeconds = a} :: DashPackage)
{-# DEPRECATED dpManifestWindowSeconds "Use generic-lens or generic-optics with 'manifestWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAdTriggers :: Lens.Lens' DashPackage (Lude.Maybe [AdTriggersElement])
dpAdTriggers = Lens.lens (adTriggers :: DashPackage -> Lude.Maybe [AdTriggersElement]) (\s a -> s {adTriggers = a} :: DashPackage)
{-# DEPRECATED dpAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH)
--
-- Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not
-- be partitioned into more than one period. If the list contains "ADS", new periods will be created where
-- the Channel source contains SCTE-35 ad markers.
--
-- /Note:/ Consider using 'periodTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPeriodTriggers :: Lens.Lens' DashPackage (Lude.Maybe [PeriodTriggersElement])
dpPeriodTriggers = Lens.lens (periodTriggers :: DashPackage -> Lude.Maybe [PeriodTriggersElement]) (\s a -> s {periodTriggers = a} :: DashPackage)
{-# DEPRECATED dpPeriodTriggers "Use generic-lens or generic-optics with 'periodTriggers' instead." #-}

instance Lude.FromJSON DashPackage where
  parseJSON =
    Lude.withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            Lude.<$> (x Lude..:? "adsOnDeliveryRestrictions")
            Lude.<*> (x Lude..:? "minBufferTimeSeconds")
            Lude.<*> (x Lude..:? "utcTiming")
            Lude.<*> (x Lude..:? "segmentTemplateFormat")
            Lude.<*> (x Lude..:? "profile")
            Lude.<*> (x Lude..:? "segmentDurationSeconds")
            Lude.<*> (x Lude..:? "utcTimingUri")
            Lude.<*> (x Lude..:? "streamSelection")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "minUpdatePeriodSeconds")
            Lude.<*> (x Lude..:? "manifestLayout")
            Lude.<*> (x Lude..:? "suggestedPresentationDelaySeconds")
            Lude.<*> (x Lude..:? "manifestWindowSeconds")
            Lude.<*> (x Lude..:? "adTriggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "periodTriggers" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DashPackage where
  toJSON DashPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("adsOnDeliveryRestrictions" Lude..=)
              Lude.<$> adsOnDeliveryRestrictions,
            ("minBufferTimeSeconds" Lude..=) Lude.<$> minBufferTimeSeconds,
            ("utcTiming" Lude..=) Lude.<$> utcTiming,
            ("segmentTemplateFormat" Lude..=) Lude.<$> segmentTemplateFormat,
            ("profile" Lude..=) Lude.<$> profile,
            ("segmentDurationSeconds" Lude..=) Lude.<$> segmentDurationSeconds,
            ("utcTimingUri" Lude..=) Lude.<$> utcTimingURI,
            ("streamSelection" Lude..=) Lude.<$> streamSelection,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("minUpdatePeriodSeconds" Lude..=) Lude.<$> minUpdatePeriodSeconds,
            ("manifestLayout" Lude..=) Lude.<$> manifestLayout,
            ("suggestedPresentationDelaySeconds" Lude..=)
              Lude.<$> suggestedPresentationDelaySeconds,
            ("manifestWindowSeconds" Lude..=) Lude.<$> manifestWindowSeconds,
            ("adTriggers" Lude..=) Lude.<$> adTriggers,
            ("periodTriggers" Lude..=) Lude.<$> periodTriggers
          ]
      )
