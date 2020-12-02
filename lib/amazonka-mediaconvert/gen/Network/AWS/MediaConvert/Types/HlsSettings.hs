{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
import Network.AWS.MediaConvert.Types.HlsAudioTrackType
import Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
import Network.AWS.Prelude

-- | Settings for HLS output groups
--
-- /See:/ 'hlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { _hsAudioRenditionSets ::
      !(Maybe Text),
    _hsIFrameOnlyManifest :: !(Maybe HlsIFrameOnlyManifest),
    _hsAudioGroupId :: !(Maybe Text),
    _hsSegmentModifier :: !(Maybe Text),
    _hsAudioOnlyContainer :: !(Maybe HlsAudioOnlyContainer),
    _hsAudioTrackType :: !(Maybe HlsAudioTrackType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'hsIFrameOnlyManifest' - When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
--
-- * 'hsAudioGroupId' - Specifies the group to which the audio Rendition belongs.
--
-- * 'hsSegmentModifier' - Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
--
-- * 'hsAudioOnlyContainer' - Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
--
-- * 'hsAudioTrackType' - Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
hlsSettings ::
  HlsSettings
hlsSettings =
  HlsSettings'
    { _hsAudioRenditionSets = Nothing,
      _hsIFrameOnlyManifest = Nothing,
      _hsAudioGroupId = Nothing,
      _hsSegmentModifier = Nothing,
      _hsAudioOnlyContainer = Nothing,
      _hsAudioTrackType = Nothing
    }

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
hsAudioRenditionSets :: Lens' HlsSettings (Maybe Text)
hsAudioRenditionSets = lens _hsAudioRenditionSets (\s a -> s {_hsAudioRenditionSets = a})

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
hsIFrameOnlyManifest :: Lens' HlsSettings (Maybe HlsIFrameOnlyManifest)
hsIFrameOnlyManifest = lens _hsIFrameOnlyManifest (\s a -> s {_hsIFrameOnlyManifest = a})

-- | Specifies the group to which the audio Rendition belongs.
hsAudioGroupId :: Lens' HlsSettings (Maybe Text)
hsAudioGroupId = lens _hsAudioGroupId (\s a -> s {_hsAudioGroupId = a})

-- | Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
hsSegmentModifier :: Lens' HlsSettings (Maybe Text)
hsSegmentModifier = lens _hsSegmentModifier (\s a -> s {_hsSegmentModifier = a})

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
hsAudioOnlyContainer :: Lens' HlsSettings (Maybe HlsAudioOnlyContainer)
hsAudioOnlyContainer = lens _hsAudioOnlyContainer (\s a -> s {_hsAudioOnlyContainer = a})

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
hsAudioTrackType :: Lens' HlsSettings (Maybe HlsAudioTrackType)
hsAudioTrackType = lens _hsAudioTrackType (\s a -> s {_hsAudioTrackType = a})

instance FromJSON HlsSettings where
  parseJSON =
    withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            <$> (x .:? "audioRenditionSets")
            <*> (x .:? "iFrameOnlyManifest")
            <*> (x .:? "audioGroupId")
            <*> (x .:? "segmentModifier")
            <*> (x .:? "audioOnlyContainer")
            <*> (x .:? "audioTrackType")
      )

instance Hashable HlsSettings

instance NFData HlsSettings

instance ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    object
      ( catMaybes
          [ ("audioRenditionSets" .=) <$> _hsAudioRenditionSets,
            ("iFrameOnlyManifest" .=) <$> _hsIFrameOnlyManifest,
            ("audioGroupId" .=) <$> _hsAudioGroupId,
            ("segmentModifier" .=) <$> _hsSegmentModifier,
            ("audioOnlyContainer" .=) <$> _hsAudioOnlyContainer,
            ("audioTrackType" .=) <$> _hsAudioTrackType
          ]
      )
