{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StopTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StopTimecode where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.LastFrameClippingBehavior
import Network.AWS.Prelude

-- | Settings to identify the end of the clip.
--
-- /See:/ 'stopTimecode' smart constructor.
data StopTimecode = StopTimecode'
  { _stLastFrameClippingBehavior ::
      !(Maybe LastFrameClippingBehavior),
    _stTimecode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopTimecode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stLastFrameClippingBehavior' - If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
--
-- * 'stTimecode' - The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stopTimecode ::
  StopTimecode
stopTimecode =
  StopTimecode'
    { _stLastFrameClippingBehavior = Nothing,
      _stTimecode = Nothing
    }

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
stLastFrameClippingBehavior :: Lens' StopTimecode (Maybe LastFrameClippingBehavior)
stLastFrameClippingBehavior = lens _stLastFrameClippingBehavior (\s a -> s {_stLastFrameClippingBehavior = a})

-- | The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stTimecode :: Lens' StopTimecode (Maybe Text)
stTimecode = lens _stTimecode (\s a -> s {_stTimecode = a})

instance FromJSON StopTimecode where
  parseJSON =
    withObject
      "StopTimecode"
      ( \x ->
          StopTimecode'
            <$> (x .:? "lastFrameClippingBehavior") <*> (x .:? "timecode")
      )

instance Hashable StopTimecode

instance NFData StopTimecode

instance ToJSON StopTimecode where
  toJSON StopTimecode' {..} =
    object
      ( catMaybes
          [ ("lastFrameClippingBehavior" .=) <$> _stLastFrameClippingBehavior,
            ("timecode" .=) <$> _stTimecode
          ]
      )
