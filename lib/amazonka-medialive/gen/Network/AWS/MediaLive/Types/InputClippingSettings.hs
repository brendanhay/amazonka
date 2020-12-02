{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputClippingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputClippingSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputTimecodeSource
import Network.AWS.MediaLive.Types.StartTimecode
import Network.AWS.MediaLive.Types.StopTimecode
import Network.AWS.Prelude

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /See:/ 'inputClippingSettings' smart constructor.
data InputClippingSettings = InputClippingSettings'
  { _icsStopTimecode ::
      !(Maybe StopTimecode),
    _icsStartTimecode :: !(Maybe StartTimecode),
    _icsInputTimecodeSource :: !InputTimecodeSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputClippingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsStopTimecode' - Settings to identify the end of the clip.
--
-- * 'icsStartTimecode' - Settings to identify the start of the clip.
--
-- * 'icsInputTimecodeSource' - The source of the timecodes in the source being clipped.
inputClippingSettings ::
  -- | 'icsInputTimecodeSource'
  InputTimecodeSource ->
  InputClippingSettings
inputClippingSettings pInputTimecodeSource_ =
  InputClippingSettings'
    { _icsStopTimecode = Nothing,
      _icsStartTimecode = Nothing,
      _icsInputTimecodeSource = pInputTimecodeSource_
    }

-- | Settings to identify the end of the clip.
icsStopTimecode :: Lens' InputClippingSettings (Maybe StopTimecode)
icsStopTimecode = lens _icsStopTimecode (\s a -> s {_icsStopTimecode = a})

-- | Settings to identify the start of the clip.
icsStartTimecode :: Lens' InputClippingSettings (Maybe StartTimecode)
icsStartTimecode = lens _icsStartTimecode (\s a -> s {_icsStartTimecode = a})

-- | The source of the timecodes in the source being clipped.
icsInputTimecodeSource :: Lens' InputClippingSettings InputTimecodeSource
icsInputTimecodeSource = lens _icsInputTimecodeSource (\s a -> s {_icsInputTimecodeSource = a})

instance FromJSON InputClippingSettings where
  parseJSON =
    withObject
      "InputClippingSettings"
      ( \x ->
          InputClippingSettings'
            <$> (x .:? "stopTimecode")
            <*> (x .:? "startTimecode")
            <*> (x .: "inputTimecodeSource")
      )

instance Hashable InputClippingSettings

instance NFData InputClippingSettings

instance ToJSON InputClippingSettings where
  toJSON InputClippingSettings' {..} =
    object
      ( catMaybes
          [ ("stopTimecode" .=) <$> _icsStopTimecode,
            ("startTimecode" .=) <$> _icsStartTimecode,
            Just ("inputTimecodeSource" .= _icsInputTimecodeSource)
          ]
      )
