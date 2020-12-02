{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.Mp4CslgAtom
import Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
import Network.AWS.MediaConvert.Types.Mp4MoovPlacement
import Network.AWS.Prelude

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- /See:/ 'mp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
  { _mMoovPlacement ::
      !(Maybe Mp4MoovPlacement),
    _mCttsVersion :: !(Maybe Nat),
    _mFreeSpaceBox :: !(Maybe Mp4FreeSpaceBox),
    _mAudioDuration :: !(Maybe CmfcAudioDuration),
    _mMp4MajorBrand :: !(Maybe Text),
    _mCslgAtom :: !(Maybe Mp4CslgAtom)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp4Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMoovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
--
-- * 'mCttsVersion' - Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
--
-- * 'mFreeSpaceBox' - Inserts a free-space box immediately after the moov box.
--
-- * 'mAudioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- * 'mMp4MajorBrand' - Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
--
-- * 'mCslgAtom' - When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
mp4Settings ::
  Mp4Settings
mp4Settings =
  Mp4Settings'
    { _mMoovPlacement = Nothing,
      _mCttsVersion = Nothing,
      _mFreeSpaceBox = Nothing,
      _mAudioDuration = Nothing,
      _mMp4MajorBrand = Nothing,
      _mCslgAtom = Nothing
    }

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
mMoovPlacement :: Lens' Mp4Settings (Maybe Mp4MoovPlacement)
mMoovPlacement = lens _mMoovPlacement (\s a -> s {_mMoovPlacement = a})

-- | Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
mCttsVersion :: Lens' Mp4Settings (Maybe Natural)
mCttsVersion = lens _mCttsVersion (\s a -> s {_mCttsVersion = a}) . mapping _Nat

-- | Inserts a free-space box immediately after the moov box.
mFreeSpaceBox :: Lens' Mp4Settings (Maybe Mp4FreeSpaceBox)
mFreeSpaceBox = lens _mFreeSpaceBox (\s a -> s {_mFreeSpaceBox = a})

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
mAudioDuration :: Lens' Mp4Settings (Maybe CmfcAudioDuration)
mAudioDuration = lens _mAudioDuration (\s a -> s {_mAudioDuration = a})

-- | Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
mMp4MajorBrand :: Lens' Mp4Settings (Maybe Text)
mMp4MajorBrand = lens _mMp4MajorBrand (\s a -> s {_mMp4MajorBrand = a})

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
mCslgAtom :: Lens' Mp4Settings (Maybe Mp4CslgAtom)
mCslgAtom = lens _mCslgAtom (\s a -> s {_mCslgAtom = a})

instance FromJSON Mp4Settings where
  parseJSON =
    withObject
      "Mp4Settings"
      ( \x ->
          Mp4Settings'
            <$> (x .:? "moovPlacement")
            <*> (x .:? "cttsVersion")
            <*> (x .:? "freeSpaceBox")
            <*> (x .:? "audioDuration")
            <*> (x .:? "mp4MajorBrand")
            <*> (x .:? "cslgAtom")
      )

instance Hashable Mp4Settings

instance NFData Mp4Settings

instance ToJSON Mp4Settings where
  toJSON Mp4Settings' {..} =
    object
      ( catMaybes
          [ ("moovPlacement" .=) <$> _mMoovPlacement,
            ("cttsVersion" .=) <$> _mCttsVersion,
            ("freeSpaceBox" .=) <$> _mFreeSpaceBox,
            ("audioDuration" .=) <$> _mAudioDuration,
            ("mp4MajorBrand" .=) <$> _mMp4MajorBrand,
            ("cslgAtom" .=) <$> _mCslgAtom
          ]
      )
