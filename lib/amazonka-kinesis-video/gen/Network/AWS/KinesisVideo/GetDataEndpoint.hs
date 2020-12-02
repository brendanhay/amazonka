{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.GetDataEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an endpoint for a specified stream for either reading or writing. Use this endpoint in your application to read from the specified stream (using the @GetMedia@ or @GetMediaForFragmentList@ operations) or write to it (using the @PutMedia@ operation).
--
--
-- In the request, specify the stream either by @StreamName@ or @StreamARN@ .
--
module Network.AWS.KinesisVideo.GetDataEndpoint
    (
    -- * Creating a Request
      getDataEndpoint
    , GetDataEndpoint
    -- * Request Lenses
    , gdeStreamARN
    , gdeStreamName
    , gdeAPIName

    -- * Destructuring the Response
    , getDataEndpointResponse
    , GetDataEndpointResponse
    -- * Response Lenses
    , gdersDataEndpoint
    , gdersResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataEndpoint' smart constructor.
data GetDataEndpoint = GetDataEndpoint'
  { _gdeStreamARN  :: !(Maybe Text)
  , _gdeStreamName :: !(Maybe Text)
  , _gdeAPIName    :: !APIName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdeStreamARN' - The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
--
-- * 'gdeStreamName' - The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
--
-- * 'gdeAPIName' - The name of the API action for which to get an endpoint.
getDataEndpoint
    :: APIName -- ^ 'gdeAPIName'
    -> GetDataEndpoint
getDataEndpoint pAPIName_ =
  GetDataEndpoint'
    {_gdeStreamARN = Nothing, _gdeStreamName = Nothing, _gdeAPIName = pAPIName_}


-- | The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
gdeStreamARN :: Lens' GetDataEndpoint (Maybe Text)
gdeStreamARN = lens _gdeStreamARN (\ s a -> s{_gdeStreamARN = a})

-- | The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
gdeStreamName :: Lens' GetDataEndpoint (Maybe Text)
gdeStreamName = lens _gdeStreamName (\ s a -> s{_gdeStreamName = a})

-- | The name of the API action for which to get an endpoint.
gdeAPIName :: Lens' GetDataEndpoint APIName
gdeAPIName = lens _gdeAPIName (\ s a -> s{_gdeAPIName = a})

instance AWSRequest GetDataEndpoint where
        type Rs GetDataEndpoint = GetDataEndpointResponse
        request = postJSON kinesisVideo
        response
          = receiveJSON
              (\ s h x ->
                 GetDataEndpointResponse' <$>
                   (x .?> "DataEndpoint") <*> (pure (fromEnum s)))

instance Hashable GetDataEndpoint where

instance NFData GetDataEndpoint where

instance ToHeaders GetDataEndpoint where
        toHeaders = const mempty

instance ToJSON GetDataEndpoint where
        toJSON GetDataEndpoint'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _gdeStreamARN,
                  ("StreamName" .=) <$> _gdeStreamName,
                  Just ("APIName" .= _gdeAPIName)])

instance ToPath GetDataEndpoint where
        toPath = const "/getDataEndpoint"

instance ToQuery GetDataEndpoint where
        toQuery = const mempty

-- | /See:/ 'getDataEndpointResponse' smart constructor.
data GetDataEndpointResponse = GetDataEndpointResponse'
  { _gdersDataEndpoint   :: !(Maybe Text)
  , _gdersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdersDataEndpoint' - The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
--
-- * 'gdersResponseStatus' - -- | The response status code.
getDataEndpointResponse
    :: Int -- ^ 'gdersResponseStatus'
    -> GetDataEndpointResponse
getDataEndpointResponse pResponseStatus_ =
  GetDataEndpointResponse'
    {_gdersDataEndpoint = Nothing, _gdersResponseStatus = pResponseStatus_}


-- | The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
gdersDataEndpoint :: Lens' GetDataEndpointResponse (Maybe Text)
gdersDataEndpoint = lens _gdersDataEndpoint (\ s a -> s{_gdersDataEndpoint = a})

-- | -- | The response status code.
gdersResponseStatus :: Lens' GetDataEndpointResponse Int
gdersResponseStatus = lens _gdersResponseStatus (\ s a -> s{_gdersResponseStatus = a})

instance NFData GetDataEndpointResponse where
