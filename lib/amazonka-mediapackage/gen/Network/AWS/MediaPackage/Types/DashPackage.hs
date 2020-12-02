{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.DashPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.DashPackage where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.DashEncryption
import Network.AWS.MediaPackage.Types.ManifestLayout
import Network.AWS.MediaPackage.Types.PeriodTriggersElement
import Network.AWS.MediaPackage.Types.Profile
import Network.AWS.MediaPackage.Types.SegmentTemplateFormat
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.MediaPackage.Types.UtcTiming
import Network.AWS.Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'dashPackage' smart constructor.
data DashPackage = DashPackage'
  { _dpAdsOnDeliveryRestrictions ::
      !(Maybe AdsOnDeliveryRestrictions),
    _dpMinBufferTimeSeconds :: !(Maybe Int),
    _dpUtcTiming :: !(Maybe UtcTiming),
    _dpSegmentTemplateFormat :: !(Maybe SegmentTemplateFormat),
    _dpProfile :: !(Maybe Profile),
    _dpSegmentDurationSeconds :: !(Maybe Int),
    _dpUtcTimingURI :: !(Maybe Text),
    _dpStreamSelection :: !(Maybe StreamSelection),
    _dpEncryption :: !(Maybe DashEncryption),
    _dpMinUpdatePeriodSeconds :: !(Maybe Int),
    _dpManifestLayout :: !(Maybe ManifestLayout),
    _dpSuggestedPresentationDelaySeconds :: !(Maybe Int),
    _dpManifestWindowSeconds :: !(Maybe Int),
    _dpAdTriggers :: !(Maybe [AdTriggersElement]),
    _dpPeriodTriggers :: !(Maybe [PeriodTriggersElement])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpAdsOnDeliveryRestrictions' - Undocumented member.
--
-- * 'dpMinBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before starting the presentation.
--
-- * 'dpUtcTiming' - Determines the type of UTCTiming included in the Media Presentation Description (MPD)
--
-- * 'dpSegmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
--
-- * 'dpProfile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
--
-- * 'dpSegmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
--
-- * 'dpUtcTimingURI' - Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
--
-- * 'dpStreamSelection' - Undocumented member.
--
-- * 'dpEncryption' - Undocumented member.
--
-- * 'dpMinUpdatePeriodSeconds' - Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
--
-- * 'dpManifestLayout' - Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
--
-- * 'dpSuggestedPresentationDelaySeconds' - Duration (in seconds) to delay live content before presentation.
--
-- * 'dpManifestWindowSeconds' - Time window (in seconds) contained in each manifest.
--
-- * 'dpAdTriggers' - Undocumented member.
--
-- * 'dpPeriodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not be partitioned into more than one period. If the list contains "ADS", new periods will be created where the Channel source contains SCTE-35 ad markers.
dashPackage ::
  DashPackage
dashPackage =
  DashPackage'
    { _dpAdsOnDeliveryRestrictions = Nothing,
      _dpMinBufferTimeSeconds = Nothing,
      _dpUtcTiming = Nothing,
      _dpSegmentTemplateFormat = Nothing,
      _dpProfile = Nothing,
      _dpSegmentDurationSeconds = Nothing,
      _dpUtcTimingURI = Nothing,
      _dpStreamSelection = Nothing,
      _dpEncryption = Nothing,
      _dpMinUpdatePeriodSeconds = Nothing,
      _dpManifestLayout = Nothing,
      _dpSuggestedPresentationDelaySeconds = Nothing,
      _dpManifestWindowSeconds = Nothing,
      _dpAdTriggers = Nothing,
      _dpPeriodTriggers = Nothing
    }

-- | Undocumented member.
dpAdsOnDeliveryRestrictions :: Lens' DashPackage (Maybe AdsOnDeliveryRestrictions)
dpAdsOnDeliveryRestrictions = lens _dpAdsOnDeliveryRestrictions (\s a -> s {_dpAdsOnDeliveryRestrictions = a})

-- | Minimum duration (in seconds) that a player will buffer media before starting the presentation.
dpMinBufferTimeSeconds :: Lens' DashPackage (Maybe Int)
dpMinBufferTimeSeconds = lens _dpMinBufferTimeSeconds (\s a -> s {_dpMinBufferTimeSeconds = a})

-- | Determines the type of UTCTiming included in the Media Presentation Description (MPD)
dpUtcTiming :: Lens' DashPackage (Maybe UtcTiming)
dpUtcTiming = lens _dpUtcTiming (\s a -> s {_dpUtcTiming = a})

-- | Determines the type of SegmentTemplate included in the Media Presentation Description (MPD).  When set to NUMBER_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Number$ media URLs.  When set to TIME_WITH_TIMELINE, a full timeline is presented in each SegmentTemplate, with $Time$ media URLs. When set to NUMBER_WITH_DURATION, only a duration is included in each SegmentTemplate, with $Number$ media URLs.
dpSegmentTemplateFormat :: Lens' DashPackage (Maybe SegmentTemplateFormat)
dpSegmentTemplateFormat = lens _dpSegmentTemplateFormat (\s a -> s {_dpSegmentTemplateFormat = a})

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
dpProfile :: Lens' DashPackage (Maybe Profile)
dpProfile = lens _dpProfile (\s a -> s {_dpProfile = a})

-- | Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
dpSegmentDurationSeconds :: Lens' DashPackage (Maybe Int)
dpSegmentDurationSeconds = lens _dpSegmentDurationSeconds (\s a -> s {_dpSegmentDurationSeconds = a})

-- | Specifies the value attribute of the UTCTiming field when utcTiming is set to HTTP-ISO or HTTP-HEAD
dpUtcTimingURI :: Lens' DashPackage (Maybe Text)
dpUtcTimingURI = lens _dpUtcTimingURI (\s a -> s {_dpUtcTimingURI = a})

-- | Undocumented member.
dpStreamSelection :: Lens' DashPackage (Maybe StreamSelection)
dpStreamSelection = lens _dpStreamSelection (\s a -> s {_dpStreamSelection = a})

-- | Undocumented member.
dpEncryption :: Lens' DashPackage (Maybe DashEncryption)
dpEncryption = lens _dpEncryption (\s a -> s {_dpEncryption = a})

-- | Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
dpMinUpdatePeriodSeconds :: Lens' DashPackage (Maybe Int)
dpMinUpdatePeriodSeconds = lens _dpMinUpdatePeriodSeconds (\s a -> s {_dpMinUpdatePeriodSeconds = a})

-- | Determines the position of some tags in the Media Presentation Description (MPD).  When set to FULL, elements like SegmentTemplate and ContentProtection are included in each Representation.  When set to COMPACT, duplicate elements are combined and presented at the AdaptationSet level.
dpManifestLayout :: Lens' DashPackage (Maybe ManifestLayout)
dpManifestLayout = lens _dpManifestLayout (\s a -> s {_dpManifestLayout = a})

-- | Duration (in seconds) to delay live content before presentation.
dpSuggestedPresentationDelaySeconds :: Lens' DashPackage (Maybe Int)
dpSuggestedPresentationDelaySeconds = lens _dpSuggestedPresentationDelaySeconds (\s a -> s {_dpSuggestedPresentationDelaySeconds = a})

-- | Time window (in seconds) contained in each manifest.
dpManifestWindowSeconds :: Lens' DashPackage (Maybe Int)
dpManifestWindowSeconds = lens _dpManifestWindowSeconds (\s a -> s {_dpManifestWindowSeconds = a})

-- | Undocumented member.
dpAdTriggers :: Lens' DashPackage [AdTriggersElement]
dpAdTriggers = lens _dpAdTriggers (\s a -> s {_dpAdTriggers = a}) . _Default . _Coerce

-- | A list of triggers that controls when the outgoing Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD) will be partitioned into multiple periods. If empty, the content will not be partitioned into more than one period. If the list contains "ADS", new periods will be created where the Channel source contains SCTE-35 ad markers.
dpPeriodTriggers :: Lens' DashPackage [PeriodTriggersElement]
dpPeriodTriggers = lens _dpPeriodTriggers (\s a -> s {_dpPeriodTriggers = a}) . _Default . _Coerce

instance FromJSON DashPackage where
  parseJSON =
    withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            <$> (x .:? "adsOnDeliveryRestrictions")
            <*> (x .:? "minBufferTimeSeconds")
            <*> (x .:? "utcTiming")
            <*> (x .:? "segmentTemplateFormat")
            <*> (x .:? "profile")
            <*> (x .:? "segmentDurationSeconds")
            <*> (x .:? "utcTimingUri")
            <*> (x .:? "streamSelection")
            <*> (x .:? "encryption")
            <*> (x .:? "minUpdatePeriodSeconds")
            <*> (x .:? "manifestLayout")
            <*> (x .:? "suggestedPresentationDelaySeconds")
            <*> (x .:? "manifestWindowSeconds")
            <*> (x .:? "adTriggers" .!= mempty)
            <*> (x .:? "periodTriggers" .!= mempty)
      )

instance Hashable DashPackage

instance NFData DashPackage

instance ToJSON DashPackage where
  toJSON DashPackage' {..} =
    object
      ( catMaybes
          [ ("adsOnDeliveryRestrictions" .=) <$> _dpAdsOnDeliveryRestrictions,
            ("minBufferTimeSeconds" .=) <$> _dpMinBufferTimeSeconds,
            ("utcTiming" .=) <$> _dpUtcTiming,
            ("segmentTemplateFormat" .=) <$> _dpSegmentTemplateFormat,
            ("profile" .=) <$> _dpProfile,
            ("segmentDurationSeconds" .=) <$> _dpSegmentDurationSeconds,
            ("utcTimingUri" .=) <$> _dpUtcTimingURI,
            ("streamSelection" .=) <$> _dpStreamSelection,
            ("encryption" .=) <$> _dpEncryption,
            ("minUpdatePeriodSeconds" .=) <$> _dpMinUpdatePeriodSeconds,
            ("manifestLayout" .=) <$> _dpManifestLayout,
            ("suggestedPresentationDelaySeconds" .=)
              <$> _dpSuggestedPresentationDelaySeconds,
            ("manifestWindowSeconds" .=) <$> _dpManifestWindowSeconds,
            ("adTriggers" .=) <$> _dpAdTriggers,
            ("periodTriggers" .=) <$> _dpPeriodTriggers
          ]
      )
