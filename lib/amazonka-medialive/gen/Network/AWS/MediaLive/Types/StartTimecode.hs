{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StartTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StartTimecode where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings to identify the start of the clip.
--
-- /See:/ 'startTimecode' smart constructor.
newtype StartTimecode = StartTimecode' {_sTimecode :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTimecode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTimecode' - The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
startTimecode ::
  StartTimecode
startTimecode = StartTimecode' {_sTimecode = Nothing}

-- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
sTimecode :: Lens' StartTimecode (Maybe Text)
sTimecode = lens _sTimecode (\s a -> s {_sTimecode = a})

instance FromJSON StartTimecode where
  parseJSON =
    withObject
      "StartTimecode"
      (\x -> StartTimecode' <$> (x .:? "timecode"))

instance Hashable StartTimecode

instance NFData StartTimecode

instance ToJSON StartTimecode where
  toJSON StartTimecode' {..} =
    object (catMaybes [("timecode" .=) <$> _sTimecode])
