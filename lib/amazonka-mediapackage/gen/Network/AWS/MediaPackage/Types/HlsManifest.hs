{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsManifest where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.PlaylistType
import Network.AWS.Prelude

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'hlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { _hmManifestName :: !(Maybe Text),
    _hmURL :: !(Maybe Text),
    _hmPlaylistType :: !(Maybe PlaylistType),
    _hmProgramDateTimeIntervalSeconds :: !(Maybe Int),
    _hmAdMarkers :: !(Maybe AdMarkers),
    _hmIncludeIframeOnlyStream :: !(Maybe Bool),
    _hmPlaylistWindowSeconds :: !(Maybe Int),
    _hmId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmManifestName' - An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- * 'hmURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'hmPlaylistType' - The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
--
-- * 'hmProgramDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
--
-- * 'hmAdMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source. "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events  in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value  that is greater than 0.
--
-- * 'hmIncludeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- * 'hmPlaylistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- * 'hmId' - The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hlsManifest ::
  -- | 'hmId'
  Text ->
  HlsManifest
hlsManifest pId_ =
  HlsManifest'
    { _hmManifestName = Nothing,
      _hmURL = Nothing,
      _hmPlaylistType = Nothing,
      _hmProgramDateTimeIntervalSeconds = Nothing,
      _hmAdMarkers = Nothing,
      _hmIncludeIframeOnlyStream = Nothing,
      _hmPlaylistWindowSeconds = Nothing,
      _hmId = pId_
    }

-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
hmManifestName :: Lens' HlsManifest (Maybe Text)
hmManifestName = lens _hmManifestName (\s a -> s {_hmManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
hmURL :: Lens' HlsManifest (Maybe Text)
hmURL = lens _hmURL (\s a -> s {_hmURL = a})

-- | The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
hmPlaylistType :: Lens' HlsManifest (Maybe PlaylistType)
hmPlaylistType = lens _hmPlaylistType (\s a -> s {_hmPlaylistType = a})

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
hmProgramDateTimeIntervalSeconds :: Lens' HlsManifest (Maybe Int)
hmProgramDateTimeIntervalSeconds = lens _hmProgramDateTimeIntervalSeconds (\s a -> s {_hmProgramDateTimeIntervalSeconds = a})

-- | This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source. "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events  in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value  that is greater than 0.
hmAdMarkers :: Lens' HlsManifest (Maybe AdMarkers)
hmAdMarkers = lens _hmAdMarkers (\s a -> s {_hmAdMarkers = a})

-- | When enabled, an I-Frame only stream will be included in the output.
hmIncludeIframeOnlyStream :: Lens' HlsManifest (Maybe Bool)
hmIncludeIframeOnlyStream = lens _hmIncludeIframeOnlyStream (\s a -> s {_hmIncludeIframeOnlyStream = a})

-- | Time window (in seconds) contained in each parent manifest.
hmPlaylistWindowSeconds :: Lens' HlsManifest (Maybe Int)
hmPlaylistWindowSeconds = lens _hmPlaylistWindowSeconds (\s a -> s {_hmPlaylistWindowSeconds = a})

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hmId :: Lens' HlsManifest Text
hmId = lens _hmId (\s a -> s {_hmId = a})

instance FromJSON HlsManifest where
  parseJSON =
    withObject
      "HlsManifest"
      ( \x ->
          HlsManifest'
            <$> (x .:? "manifestName")
            <*> (x .:? "url")
            <*> (x .:? "playlistType")
            <*> (x .:? "programDateTimeIntervalSeconds")
            <*> (x .:? "adMarkers")
            <*> (x .:? "includeIframeOnlyStream")
            <*> (x .:? "playlistWindowSeconds")
            <*> (x .: "id")
      )

instance Hashable HlsManifest

instance NFData HlsManifest
