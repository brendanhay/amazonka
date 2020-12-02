{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AiffSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AiffSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /See:/ 'aiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { _asBitDepth :: !(Maybe Nat),
    _asChannels :: !(Maybe Nat),
    _asSampleRate :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AiffSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asBitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- * 'asChannels' - Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
--
-- * 'asSampleRate' - Sample rate in hz.
aiffSettings ::
  AiffSettings
aiffSettings =
  AiffSettings'
    { _asBitDepth = Nothing,
      _asChannels = Nothing,
      _asSampleRate = Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
asBitDepth :: Lens' AiffSettings (Maybe Natural)
asBitDepth = lens _asBitDepth (\s a -> s {_asBitDepth = a}) . mapping _Nat

-- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
asChannels :: Lens' AiffSettings (Maybe Natural)
asChannels = lens _asChannels (\s a -> s {_asChannels = a}) . mapping _Nat

-- | Sample rate in hz.
asSampleRate :: Lens' AiffSettings (Maybe Natural)
asSampleRate = lens _asSampleRate (\s a -> s {_asSampleRate = a}) . mapping _Nat

instance FromJSON AiffSettings where
  parseJSON =
    withObject
      "AiffSettings"
      ( \x ->
          AiffSettings'
            <$> (x .:? "bitDepth") <*> (x .:? "channels") <*> (x .:? "sampleRate")
      )

instance Hashable AiffSettings

instance NFData AiffSettings

instance ToJSON AiffSettings where
  toJSON AiffSettings' {..} =
    object
      ( catMaybes
          [ ("bitDepth" .=) <$> _asBitDepth,
            ("channels" .=) <$> _asChannels,
            ("sampleRate" .=) <$> _asSampleRate
          ]
      )
