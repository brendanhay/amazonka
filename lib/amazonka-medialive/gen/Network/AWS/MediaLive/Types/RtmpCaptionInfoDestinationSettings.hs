{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'rtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RtmpCaptionInfoDestinationSettings' with the minimum fields required to make a request.
rtmpCaptionInfoDestinationSettings ::
  RtmpCaptionInfoDestinationSettings
rtmpCaptionInfoDestinationSettings =
  RtmpCaptionInfoDestinationSettings'

instance FromJSON RtmpCaptionInfoDestinationSettings where
  parseJSON =
    withObject
      "RtmpCaptionInfoDestinationSettings"
      (\x -> pure RtmpCaptionInfoDestinationSettings')

instance Hashable RtmpCaptionInfoDestinationSettings

instance NFData RtmpCaptionInfoDestinationSettings

instance ToJSON RtmpCaptionInfoDestinationSettings where
  toJSON = const (Object mempty)
