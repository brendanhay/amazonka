{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputChannelLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputChannelLevel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Input Channel Level
--
-- /See:/ 'inputChannelLevel' smart constructor.
data InputChannelLevel = InputChannelLevel'
  { _iclInputChannel ::
      !Nat,
    _iclGain :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputChannelLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iclInputChannel' - The index of the input channel used as a source.
--
-- * 'iclGain' - Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
inputChannelLevel ::
  -- | 'iclInputChannel'
  Natural ->
  -- | 'iclGain'
  Int ->
  InputChannelLevel
inputChannelLevel pInputChannel_ pGain_ =
  InputChannelLevel'
    { _iclInputChannel = _Nat # pInputChannel_,
      _iclGain = pGain_
    }

-- | The index of the input channel used as a source.
iclInputChannel :: Lens' InputChannelLevel Natural
iclInputChannel = lens _iclInputChannel (\s a -> s {_iclInputChannel = a}) . _Nat

-- | Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
iclGain :: Lens' InputChannelLevel Int
iclGain = lens _iclGain (\s a -> s {_iclGain = a})

instance FromJSON InputChannelLevel where
  parseJSON =
    withObject
      "InputChannelLevel"
      ( \x ->
          InputChannelLevel' <$> (x .: "inputChannel") <*> (x .: "gain")
      )

instance Hashable InputChannelLevel

instance NFData InputChannelLevel

instance ToJSON InputChannelLevel where
  toJSON InputChannelLevel' {..} =
    object
      ( catMaybes
          [ Just ("inputChannel" .= _iclInputChannel),
            Just ("gain" .= _iclGain)
          ]
      )
