{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexVideoSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
import Network.AWS.Prelude

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'multiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { _mvsStatmuxSettings ::
      !(Maybe MultiplexStatmuxVideoSettings),
    _mvsConstantBitrate :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexVideoSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvsStatmuxSettings' - Statmux rate control settings. When this field is defined, ConstantBitrate must be undefined.
--
-- * 'mvsConstantBitrate' - The constant bitrate configuration for the video encode. When this field is defined, StatmuxSettings must be undefined.
multiplexVideoSettings ::
  MultiplexVideoSettings
multiplexVideoSettings =
  MultiplexVideoSettings'
    { _mvsStatmuxSettings = Nothing,
      _mvsConstantBitrate = Nothing
    }

-- | Statmux rate control settings. When this field is defined, ConstantBitrate must be undefined.
mvsStatmuxSettings :: Lens' MultiplexVideoSettings (Maybe MultiplexStatmuxVideoSettings)
mvsStatmuxSettings = lens _mvsStatmuxSettings (\s a -> s {_mvsStatmuxSettings = a})

-- | The constant bitrate configuration for the video encode. When this field is defined, StatmuxSettings must be undefined.
mvsConstantBitrate :: Lens' MultiplexVideoSettings (Maybe Natural)
mvsConstantBitrate = lens _mvsConstantBitrate (\s a -> s {_mvsConstantBitrate = a}) . mapping _Nat

instance FromJSON MultiplexVideoSettings where
  parseJSON =
    withObject
      "MultiplexVideoSettings"
      ( \x ->
          MultiplexVideoSettings'
            <$> (x .:? "statmuxSettings") <*> (x .:? "constantBitrate")
      )

instance Hashable MultiplexVideoSettings

instance NFData MultiplexVideoSettings

instance ToJSON MultiplexVideoSettings where
  toJSON MultiplexVideoSettings' {..} =
    object
      ( catMaybes
          [ ("statmuxSettings" .=) <$> _mvsStatmuxSettings,
            ("constantBitrate" .=) <$> _mvsConstantBitrate
          ]
      )
