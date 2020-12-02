{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
import Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.Prelude

-- | Audio Only Hls Settings
--
-- /See:/ 'audioOnlyHlsSettings' smart constructor.
data AudioOnlyHlsSettings = AudioOnlyHlsSettings'
  { _aohsAudioOnlyImage ::
      !(Maybe InputLocation),
    _aohsSegmentType ::
      !(Maybe AudioOnlyHlsSegmentType),
    _aohsAudioGroupId :: !(Maybe Text),
    _aohsAudioTrackType ::
      !(Maybe AudioOnlyHlsTrackType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioOnlyHlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aohsAudioOnlyImage' - Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth. The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
--
-- * 'aohsSegmentType' - Specifies the segment type.
--
-- * 'aohsAudioGroupId' - Specifies the group to which the audio Rendition belongs.
--
-- * 'aohsAudioTrackType' - Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
audioOnlyHlsSettings ::
  AudioOnlyHlsSettings
audioOnlyHlsSettings =
  AudioOnlyHlsSettings'
    { _aohsAudioOnlyImage = Nothing,
      _aohsSegmentType = Nothing,
      _aohsAudioGroupId = Nothing,
      _aohsAudioTrackType = Nothing
    }

-- | Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth. The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
aohsAudioOnlyImage :: Lens' AudioOnlyHlsSettings (Maybe InputLocation)
aohsAudioOnlyImage = lens _aohsAudioOnlyImage (\s a -> s {_aohsAudioOnlyImage = a})

-- | Specifies the segment type.
aohsSegmentType :: Lens' AudioOnlyHlsSettings (Maybe AudioOnlyHlsSegmentType)
aohsSegmentType = lens _aohsSegmentType (\s a -> s {_aohsSegmentType = a})

-- | Specifies the group to which the audio Rendition belongs.
aohsAudioGroupId :: Lens' AudioOnlyHlsSettings (Maybe Text)
aohsAudioGroupId = lens _aohsAudioGroupId (\s a -> s {_aohsAudioGroupId = a})

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
aohsAudioTrackType :: Lens' AudioOnlyHlsSettings (Maybe AudioOnlyHlsTrackType)
aohsAudioTrackType = lens _aohsAudioTrackType (\s a -> s {_aohsAudioTrackType = a})

instance FromJSON AudioOnlyHlsSettings where
  parseJSON =
    withObject
      "AudioOnlyHlsSettings"
      ( \x ->
          AudioOnlyHlsSettings'
            <$> (x .:? "audioOnlyImage")
            <*> (x .:? "segmentType")
            <*> (x .:? "audioGroupId")
            <*> (x .:? "audioTrackType")
      )

instance Hashable AudioOnlyHlsSettings

instance NFData AudioOnlyHlsSettings

instance ToJSON AudioOnlyHlsSettings where
  toJSON AudioOnlyHlsSettings' {..} =
    object
      ( catMaybes
          [ ("audioOnlyImage" .=) <$> _aohsAudioOnlyImage,
            ("segmentType" .=) <$> _aohsSegmentType,
            ("audioGroupId" .=) <$> _aohsAudioGroupId,
            ("audioTrackType" .=) <$> _aohsAudioTrackType
          ]
      )
