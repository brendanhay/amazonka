{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration where

import Network.AWS.KinesisVideo.Types.ChannelProtocol
import Network.AWS.KinesisVideo.Types.ChannelRole
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains the endpoint configuration for the @SINGLE_MASTER@ channel type.
--
--
--
-- /See:/ 'singleMasterChannelEndpointConfiguration' smart constructor.
data SingleMasterChannelEndpointConfiguration = SingleMasterChannelEndpointConfiguration'
  { _smcecProtocols ::
      !( Maybe
           ( List1
               ChannelProtocol
           )
       ),
    _smcecRole ::
      !( Maybe
           ChannelRole
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SingleMasterChannelEndpointConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smcecProtocols' - This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
--
-- * 'smcecRole' - This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel.
singleMasterChannelEndpointConfiguration ::
  SingleMasterChannelEndpointConfiguration
singleMasterChannelEndpointConfiguration =
  SingleMasterChannelEndpointConfiguration'
    { _smcecProtocols =
        Nothing,
      _smcecRole = Nothing
    }

-- | This property is used to determine the nature of communication over this @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API returns a websocket endpoint. If @HTTPS@ is specified, this API returns an @HTTPS@ endpoint.
smcecProtocols :: Lens' SingleMasterChannelEndpointConfiguration (Maybe (NonEmpty ChannelProtocol))
smcecProtocols = lens _smcecProtocols (\s a -> s {_smcecProtocols = a}) . mapping _List1

-- | This property is used to determine messaging permissions in this @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API returns an endpoint that a client can use to receive offers from and send answers to any of the viewers on this signaling channel. If @VIEWER@ is specified, this API returns an endpoint that a client can use only to send offers to another @MASTER@ client on this signaling channel.
smcecRole :: Lens' SingleMasterChannelEndpointConfiguration (Maybe ChannelRole)
smcecRole = lens _smcecRole (\s a -> s {_smcecRole = a})

instance Hashable SingleMasterChannelEndpointConfiguration

instance NFData SingleMasterChannelEndpointConfiguration

instance ToJSON SingleMasterChannelEndpointConfiguration where
  toJSON SingleMasterChannelEndpointConfiguration' {..} =
    object
      ( catMaybes
          [("Protocols" .=) <$> _smcecProtocols, ("Role" .=) <$> _smcecRole]
      )
