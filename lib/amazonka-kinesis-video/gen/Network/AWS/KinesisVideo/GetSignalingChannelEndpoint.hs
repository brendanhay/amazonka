{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.GetSignalingChannelEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides an endpoint for the specified signaling channel to send and receive messages. This API uses the @SingleMasterChannelEndpointConfiguration@ input parameter, which consists of the @Protocols@ and @Role@ properties.
--
--
-- @Protocols@ is used to determine the communication mechanism. For example, if you specify @WSS@ as the protocol, this API produces a secure websocket endpoint. If you specify @HTTPS@ as the protocol, this API generates an HTTPS endpoint.
--
-- @Role@ determines the messaging permissions. A @MASTER@ role results in this API generating an endpoint that a client can use to communicate with any of the viewers on the channel. A @VIEWER@ role results in this API generating an endpoint that a client can use to communicate only with a @MASTER@ .
module Network.AWS.KinesisVideo.GetSignalingChannelEndpoint
  ( -- * Creating a Request
    getSignalingChannelEndpoint,
    GetSignalingChannelEndpoint,

    -- * Request Lenses
    gsceSingleMasterChannelEndpointConfiguration,
    gsceChannelARN,

    -- * Destructuring the Response
    getSignalingChannelEndpointResponse,
    GetSignalingChannelEndpointResponse,

    -- * Response Lenses
    gscersResourceEndpointList,
    gscersResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSignalingChannelEndpoint' smart constructor.
data GetSignalingChannelEndpoint = GetSignalingChannelEndpoint'
  { _gsceSingleMasterChannelEndpointConfiguration ::
      !( Maybe
           SingleMasterChannelEndpointConfiguration
       ),
    _gsceChannelARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSignalingChannelEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsceSingleMasterChannelEndpointConfiguration' - A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
--
-- * 'gsceChannelARN' - The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
getSignalingChannelEndpoint ::
  -- | 'gsceChannelARN'
  Text ->
  GetSignalingChannelEndpoint
getSignalingChannelEndpoint pChannelARN_ =
  GetSignalingChannelEndpoint'
    { _gsceSingleMasterChannelEndpointConfiguration =
        Nothing,
      _gsceChannelARN = pChannelARN_
    }

-- | A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
gsceSingleMasterChannelEndpointConfiguration :: Lens' GetSignalingChannelEndpoint (Maybe SingleMasterChannelEndpointConfiguration)
gsceSingleMasterChannelEndpointConfiguration = lens _gsceSingleMasterChannelEndpointConfiguration (\s a -> s {_gsceSingleMasterChannelEndpointConfiguration = a})

-- | The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
gsceChannelARN :: Lens' GetSignalingChannelEndpoint Text
gsceChannelARN = lens _gsceChannelARN (\s a -> s {_gsceChannelARN = a})

instance AWSRequest GetSignalingChannelEndpoint where
  type
    Rs GetSignalingChannelEndpoint =
      GetSignalingChannelEndpointResponse
  request = postJSON kinesisVideo
  response =
    receiveJSON
      ( \s h x ->
          GetSignalingChannelEndpointResponse'
            <$> (x .?> "ResourceEndpointList" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetSignalingChannelEndpoint

instance NFData GetSignalingChannelEndpoint

instance ToHeaders GetSignalingChannelEndpoint where
  toHeaders = const mempty

instance ToJSON GetSignalingChannelEndpoint where
  toJSON GetSignalingChannelEndpoint' {..} =
    object
      ( catMaybes
          [ ("SingleMasterChannelEndpointConfiguration" .=)
              <$> _gsceSingleMasterChannelEndpointConfiguration,
            Just ("ChannelARN" .= _gsceChannelARN)
          ]
      )

instance ToPath GetSignalingChannelEndpoint where
  toPath = const "/getSignalingChannelEndpoint"

instance ToQuery GetSignalingChannelEndpoint where
  toQuery = const mempty

-- | /See:/ 'getSignalingChannelEndpointResponse' smart constructor.
data GetSignalingChannelEndpointResponse = GetSignalingChannelEndpointResponse'
  { _gscersResourceEndpointList ::
      !( Maybe
           [ResourceEndpointListItem]
       ),
    _gscersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSignalingChannelEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscersResourceEndpointList' - A list of endpoints for the specified signaling channel.
--
-- * 'gscersResponseStatus' - -- | The response status code.
getSignalingChannelEndpointResponse ::
  -- | 'gscersResponseStatus'
  Int ->
  GetSignalingChannelEndpointResponse
getSignalingChannelEndpointResponse pResponseStatus_ =
  GetSignalingChannelEndpointResponse'
    { _gscersResourceEndpointList =
        Nothing,
      _gscersResponseStatus = pResponseStatus_
    }

-- | A list of endpoints for the specified signaling channel.
gscersResourceEndpointList :: Lens' GetSignalingChannelEndpointResponse [ResourceEndpointListItem]
gscersResourceEndpointList = lens _gscersResourceEndpointList (\s a -> s {_gscersResourceEndpointList = a}) . _Default . _Coerce

-- | -- | The response status code.
gscersResponseStatus :: Lens' GetSignalingChannelEndpointResponse Int
gscersResponseStatus = lens _gscersResponseStatus (\s a -> s {_gscersResponseStatus = a})

instance NFData GetSignalingChannelEndpointResponse
