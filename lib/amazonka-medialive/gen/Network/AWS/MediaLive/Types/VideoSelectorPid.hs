{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorPid
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorPid where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Video Selector Pid
--
-- /See:/ 'videoSelectorPid' smart constructor.
newtype VideoSelectorPid = VideoSelectorPid' {_vspPid :: Maybe Nat}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelectorPid' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vspPid' - Selects a specific PID from within a video source.
videoSelectorPid ::
  VideoSelectorPid
videoSelectorPid = VideoSelectorPid' {_vspPid = Nothing}

-- | Selects a specific PID from within a video source.
vspPid :: Lens' VideoSelectorPid (Maybe Natural)
vspPid = lens _vspPid (\s a -> s {_vspPid = a}) . mapping _Nat

instance FromJSON VideoSelectorPid where
  parseJSON =
    withObject
      "VideoSelectorPid"
      (\x -> VideoSelectorPid' <$> (x .:? "pid"))

instance Hashable VideoSelectorPid

instance NFData VideoSelectorPid

instance ToJSON VideoSelectorPid where
  toJSON VideoSelectorPid' {..} =
    object (catMaybes [("pid" .=) <$> _vspPid])
