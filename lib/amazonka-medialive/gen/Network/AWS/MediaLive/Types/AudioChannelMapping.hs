{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioChannelMapping where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputChannelLevel
import Network.AWS.Prelude

-- | Audio Channel Mapping
--
-- /See:/ 'audioChannelMapping' smart constructor.
data AudioChannelMapping = AudioChannelMapping'
  { _acmOutputChannel ::
      !Nat,
    _acmInputChannelLevels :: ![InputChannelLevel]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acmOutputChannel' - The index of the output channel being produced.
--
-- * 'acmInputChannelLevels' - Indices and gain values for each input channel that should be remixed into this output channel.
audioChannelMapping ::
  -- | 'acmOutputChannel'
  Natural ->
  AudioChannelMapping
audioChannelMapping pOutputChannel_ =
  AudioChannelMapping'
    { _acmOutputChannel = _Nat # pOutputChannel_,
      _acmInputChannelLevels = mempty
    }

-- | The index of the output channel being produced.
acmOutputChannel :: Lens' AudioChannelMapping Natural
acmOutputChannel = lens _acmOutputChannel (\s a -> s {_acmOutputChannel = a}) . _Nat

-- | Indices and gain values for each input channel that should be remixed into this output channel.
acmInputChannelLevels :: Lens' AudioChannelMapping [InputChannelLevel]
acmInputChannelLevels = lens _acmInputChannelLevels (\s a -> s {_acmInputChannelLevels = a}) . _Coerce

instance FromJSON AudioChannelMapping where
  parseJSON =
    withObject
      "AudioChannelMapping"
      ( \x ->
          AudioChannelMapping'
            <$> (x .: "outputChannel") <*> (x .:? "inputChannelLevels" .!= mempty)
      )

instance Hashable AudioChannelMapping

instance NFData AudioChannelMapping

instance ToJSON AudioChannelMapping where
  toJSON AudioChannelMapping' {..} =
    object
      ( catMaybes
          [ Just ("outputChannel" .= _acmOutputChannel),
            Just ("inputChannelLevels" .= _acmInputChannelLevels)
          ]
      )
