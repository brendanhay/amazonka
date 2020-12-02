{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsPackage where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.HlsEncryption
import Network.AWS.MediaPackage.Types.PlaylistType
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.Prelude

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'hlsPackage' smart constructor.
data HlsPackage = HlsPackage'
  { _hpAdsOnDeliveryRestrictions ::
      !(Maybe AdsOnDeliveryRestrictions),
    _hpUseAudioRenditionGroup :: !(Maybe Bool),
    _hpPlaylistType :: !(Maybe PlaylistType),
    _hpSegmentDurationSeconds :: !(Maybe Int),
    _hpProgramDateTimeIntervalSeconds :: !(Maybe Int),
    _hpStreamSelection :: !(Maybe StreamSelection),
    _hpAdMarkers :: !(Maybe AdMarkers),
    _hpEncryption :: !(Maybe HlsEncryption),
    _hpIncludeIframeOnlyStream :: !(Maybe Bool),
    _hpAdTriggers :: !(Maybe [AdTriggersElement]),
    _hpPlaylistWindowSeconds :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpAdsOnDeliveryRestrictions' - Undocumented member.
--
-- * 'hpUseAudioRenditionGroup' - When enabled, audio streams will be placed in rendition groups in the output.
--
-- * 'hpPlaylistType' - The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
--
-- * 'hpSegmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be rounded to the nearest multiple of the source fragment duration.
--
-- * 'hpProgramDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
--
-- * 'hpStreamSelection' - Undocumented member.
--
-- * 'hpAdMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source. "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events  in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value  that is greater than 0.
--
-- * 'hpEncryption' - Undocumented member.
--
-- * 'hpIncludeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- * 'hpAdTriggers' - Undocumented member.
--
-- * 'hpPlaylistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
hlsPackage ::
  HlsPackage
hlsPackage =
  HlsPackage'
    { _hpAdsOnDeliveryRestrictions = Nothing,
      _hpUseAudioRenditionGroup = Nothing,
      _hpPlaylistType = Nothing,
      _hpSegmentDurationSeconds = Nothing,
      _hpProgramDateTimeIntervalSeconds = Nothing,
      _hpStreamSelection = Nothing,
      _hpAdMarkers = Nothing,
      _hpEncryption = Nothing,
      _hpIncludeIframeOnlyStream = Nothing,
      _hpAdTriggers = Nothing,
      _hpPlaylistWindowSeconds = Nothing
    }

-- | Undocumented member.
hpAdsOnDeliveryRestrictions :: Lens' HlsPackage (Maybe AdsOnDeliveryRestrictions)
hpAdsOnDeliveryRestrictions = lens _hpAdsOnDeliveryRestrictions (\s a -> s {_hpAdsOnDeliveryRestrictions = a})

-- | When enabled, audio streams will be placed in rendition groups in the output.
hpUseAudioRenditionGroup :: Lens' HlsPackage (Maybe Bool)
hpUseAudioRenditionGroup = lens _hpUseAudioRenditionGroup (\s a -> s {_hpUseAudioRenditionGroup = a})

-- | The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
hpPlaylistType :: Lens' HlsPackage (Maybe PlaylistType)
hpPlaylistType = lens _hpPlaylistType (\s a -> s {_hpPlaylistType = a})

-- | Duration (in seconds) of each fragment. Actual fragments will be rounded to the nearest multiple of the source fragment duration.
hpSegmentDurationSeconds :: Lens' HlsPackage (Maybe Int)
hpSegmentDurationSeconds = lens _hpSegmentDurationSeconds (\s a -> s {_hpSegmentDurationSeconds = a})

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
hpProgramDateTimeIntervalSeconds :: Lens' HlsPackage (Maybe Int)
hpProgramDateTimeIntervalSeconds = lens _hpProgramDateTimeIntervalSeconds (\s a -> s {_hpProgramDateTimeIntervalSeconds = a})

-- | Undocumented member.
hpStreamSelection :: Lens' HlsPackage (Maybe StreamSelection)
hpStreamSelection = lens _hpStreamSelection (\s a -> s {_hpStreamSelection = a})

-- | This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source. "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events  in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value  that is greater than 0.
hpAdMarkers :: Lens' HlsPackage (Maybe AdMarkers)
hpAdMarkers = lens _hpAdMarkers (\s a -> s {_hpAdMarkers = a})

-- | Undocumented member.
hpEncryption :: Lens' HlsPackage (Maybe HlsEncryption)
hpEncryption = lens _hpEncryption (\s a -> s {_hpEncryption = a})

-- | When enabled, an I-Frame only stream will be included in the output.
hpIncludeIframeOnlyStream :: Lens' HlsPackage (Maybe Bool)
hpIncludeIframeOnlyStream = lens _hpIncludeIframeOnlyStream (\s a -> s {_hpIncludeIframeOnlyStream = a})

-- | Undocumented member.
hpAdTriggers :: Lens' HlsPackage [AdTriggersElement]
hpAdTriggers = lens _hpAdTriggers (\s a -> s {_hpAdTriggers = a}) . _Default . _Coerce

-- | Time window (in seconds) contained in each parent manifest.
hpPlaylistWindowSeconds :: Lens' HlsPackage (Maybe Int)
hpPlaylistWindowSeconds = lens _hpPlaylistWindowSeconds (\s a -> s {_hpPlaylistWindowSeconds = a})

instance FromJSON HlsPackage where
  parseJSON =
    withObject
      "HlsPackage"
      ( \x ->
          HlsPackage'
            <$> (x .:? "adsOnDeliveryRestrictions")
            <*> (x .:? "useAudioRenditionGroup")
            <*> (x .:? "playlistType")
            <*> (x .:? "segmentDurationSeconds")
            <*> (x .:? "programDateTimeIntervalSeconds")
            <*> (x .:? "streamSelection")
            <*> (x .:? "adMarkers")
            <*> (x .:? "encryption")
            <*> (x .:? "includeIframeOnlyStream")
            <*> (x .:? "adTriggers" .!= mempty)
            <*> (x .:? "playlistWindowSeconds")
      )

instance Hashable HlsPackage

instance NFData HlsPackage

instance ToJSON HlsPackage where
  toJSON HlsPackage' {..} =
    object
      ( catMaybes
          [ ("adsOnDeliveryRestrictions" .=) <$> _hpAdsOnDeliveryRestrictions,
            ("useAudioRenditionGroup" .=) <$> _hpUseAudioRenditionGroup,
            ("playlistType" .=) <$> _hpPlaylistType,
            ("segmentDurationSeconds" .=) <$> _hpSegmentDurationSeconds,
            ("programDateTimeIntervalSeconds" .=)
              <$> _hpProgramDateTimeIntervalSeconds,
            ("streamSelection" .=) <$> _hpStreamSelection,
            ("adMarkers" .=) <$> _hpAdMarkers,
            ("encryption" .=) <$> _hpEncryption,
            ("includeIframeOnlyStream" .=) <$> _hpIncludeIframeOnlyStream,
            ("adTriggers" .=) <$> _hpAdTriggers,
            ("playlistWindowSeconds" .=) <$> _hpPlaylistWindowSeconds
          ]
      )
