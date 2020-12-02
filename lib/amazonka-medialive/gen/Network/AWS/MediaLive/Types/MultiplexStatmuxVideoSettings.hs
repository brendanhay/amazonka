{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statmux rate control settings
--
-- /See:/ 'multiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'
  { _msvsPriority ::
      !(Maybe Int),
    _msvsMinimumBitrate ::
      !(Maybe Nat),
    _msvsMaximumBitrate ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexStatmuxVideoSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msvsPriority' - The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
--
-- * 'msvsMinimumBitrate' - Minimum statmux bitrate.
--
-- * 'msvsMaximumBitrate' - Maximum statmux bitrate.
multiplexStatmuxVideoSettings ::
  MultiplexStatmuxVideoSettings
multiplexStatmuxVideoSettings =
  MultiplexStatmuxVideoSettings'
    { _msvsPriority = Nothing,
      _msvsMinimumBitrate = Nothing,
      _msvsMaximumBitrate = Nothing
    }

-- | The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
msvsPriority :: Lens' MultiplexStatmuxVideoSettings (Maybe Int)
msvsPriority = lens _msvsPriority (\s a -> s {_msvsPriority = a})

-- | Minimum statmux bitrate.
msvsMinimumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMinimumBitrate = lens _msvsMinimumBitrate (\s a -> s {_msvsMinimumBitrate = a}) . mapping _Nat

-- | Maximum statmux bitrate.
msvsMaximumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMaximumBitrate = lens _msvsMaximumBitrate (\s a -> s {_msvsMaximumBitrate = a}) . mapping _Nat

instance FromJSON MultiplexStatmuxVideoSettings where
  parseJSON =
    withObject
      "MultiplexStatmuxVideoSettings"
      ( \x ->
          MultiplexStatmuxVideoSettings'
            <$> (x .:? "priority")
            <*> (x .:? "minimumBitrate")
            <*> (x .:? "maximumBitrate")
      )

instance Hashable MultiplexStatmuxVideoSettings

instance NFData MultiplexStatmuxVideoSettings

instance ToJSON MultiplexStatmuxVideoSettings where
  toJSON MultiplexStatmuxVideoSettings' {..} =
    object
      ( catMaybes
          [ ("priority" .=) <$> _msvsPriority,
            ("minimumBitrate" .=) <$> _msvsMinimumBitrate,
            ("maximumBitrate" .=) <$> _msvsMaximumBitrate
          ]
      )
