{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.AudioCodecOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options associated with your audio codec.
--
--
--
-- /See:/ 'audioCodecOptions' smart constructor.
data AudioCodecOptions = AudioCodecOptions'
  { _acoSigned ::
      !(Maybe Text),
    _acoBitDepth :: !(Maybe Text),
    _acoProfile :: !(Maybe Text),
    _acoBitOrder :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioCodecOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acoSigned' - You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec. Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned). The supported value is @Signed@ .
--
-- * 'acoBitDepth' - You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec. The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file. Valid values are @16@ and @24@ . The most common bit depth is @24@ .
--
-- * 'acoProfile' - You can only choose an audio profile when you specify AAC for the value of Audio:Codec. Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps. All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
--
-- * 'acoBitOrder' - You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec. The order the bits of a PCM sample are stored in. The supported value is @LittleEndian@ .
audioCodecOptions ::
  AudioCodecOptions
audioCodecOptions =
  AudioCodecOptions'
    { _acoSigned = Nothing,
      _acoBitDepth = Nothing,
      _acoProfile = Nothing,
      _acoBitOrder = Nothing
    }

-- | You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec. Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned). The supported value is @Signed@ .
acoSigned :: Lens' AudioCodecOptions (Maybe Text)
acoSigned = lens _acoSigned (\s a -> s {_acoSigned = a})

-- | You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec. The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file. Valid values are @16@ and @24@ . The most common bit depth is @24@ .
acoBitDepth :: Lens' AudioCodecOptions (Maybe Text)
acoBitDepth = lens _acoBitDepth (\s a -> s {_acoBitDepth = a})

-- | You can only choose an audio profile when you specify AAC for the value of Audio:Codec. Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps. All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
acoProfile :: Lens' AudioCodecOptions (Maybe Text)
acoProfile = lens _acoProfile (\s a -> s {_acoProfile = a})

-- | You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec. The order the bits of a PCM sample are stored in. The supported value is @LittleEndian@ .
acoBitOrder :: Lens' AudioCodecOptions (Maybe Text)
acoBitOrder = lens _acoBitOrder (\s a -> s {_acoBitOrder = a})

instance FromJSON AudioCodecOptions where
  parseJSON =
    withObject
      "AudioCodecOptions"
      ( \x ->
          AudioCodecOptions'
            <$> (x .:? "Signed")
            <*> (x .:? "BitDepth")
            <*> (x .:? "Profile")
            <*> (x .:? "BitOrder")
      )

instance Hashable AudioCodecOptions

instance NFData AudioCodecOptions

instance ToJSON AudioCodecOptions where
  toJSON AudioCodecOptions' {..} =
    object
      ( catMaybes
          [ ("Signed" .=) <$> _acoSigned,
            ("BitDepth" .=) <$> _acoBitDepth,
            ("Profile" .=) <$> _acoProfile,
            ("BitOrder" .=) <$> _acoBitOrder
          ]
      )
