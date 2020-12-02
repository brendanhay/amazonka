{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ResourceEndpointListItem where

import Network.AWS.KinesisVideo.Types.ChannelProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that describes the endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
--
--
-- /See:/ 'resourceEndpointListItem' smart constructor.
data ResourceEndpointListItem = ResourceEndpointListItem'
  { _reliProtocol ::
      !(Maybe ChannelProtocol),
    _reliResourceEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceEndpointListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reliProtocol' - The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- * 'reliResourceEndpoint' - The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
resourceEndpointListItem ::
  ResourceEndpointListItem
resourceEndpointListItem =
  ResourceEndpointListItem'
    { _reliProtocol = Nothing,
      _reliResourceEndpoint = Nothing
    }

-- | The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
reliProtocol :: Lens' ResourceEndpointListItem (Maybe ChannelProtocol)
reliProtocol = lens _reliProtocol (\s a -> s {_reliProtocol = a})

-- | The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
reliResourceEndpoint :: Lens' ResourceEndpointListItem (Maybe Text)
reliResourceEndpoint = lens _reliResourceEndpoint (\s a -> s {_reliResourceEndpoint = a})

instance FromJSON ResourceEndpointListItem where
  parseJSON =
    withObject
      "ResourceEndpointListItem"
      ( \x ->
          ResourceEndpointListItem'
            <$> (x .:? "Protocol") <*> (x .:? "ResourceEndpoint")
      )

instance Hashable ResourceEndpointListItem

instance NFData ResourceEndpointListItem
