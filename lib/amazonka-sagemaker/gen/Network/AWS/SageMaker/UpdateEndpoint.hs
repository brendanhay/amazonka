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
-- Module      : Network.AWS.SageMaker.UpdateEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys the new @EndpointConfig@ specified in the request, switches to using newly created endpoint, and then deletes resources provisioned for the endpoint using the previous @EndpointConfig@ (there is no availability loss).
--
--
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_DescribeEndpoint.html DescribeEndpoint> API.
--
module Network.AWS.SageMaker.UpdateEndpoint
    (
    -- * Creating a Request
      updateEndpoint
    , UpdateEndpoint
    -- * Request Lenses
    , ueEndpointName
    , ueEndpointConfigName

    -- * Destructuring the Response
    , updateEndpointResponse
    , UpdateEndpointResponse
    -- * Response Lenses
    , uersResponseStatus
    , uersEndpointARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { _ueEndpointName       :: !Text
  , _ueEndpointConfigName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueEndpointName' - The name of the endpoint whose configuration you want to update.
--
-- * 'ueEndpointConfigName' - The name of the new endpoint configuration.
updateEndpoint
    :: Text -- ^ 'ueEndpointName'
    -> Text -- ^ 'ueEndpointConfigName'
    -> UpdateEndpoint
updateEndpoint pEndpointName_ pEndpointConfigName_ =
  UpdateEndpoint'
    { _ueEndpointName = pEndpointName_
    , _ueEndpointConfigName = pEndpointConfigName_
    }


-- | The name of the endpoint whose configuration you want to update.
ueEndpointName :: Lens' UpdateEndpoint Text
ueEndpointName = lens _ueEndpointName (\ s a -> s{_ueEndpointName = a})

-- | The name of the new endpoint configuration.
ueEndpointConfigName :: Lens' UpdateEndpoint Text
ueEndpointConfigName = lens _ueEndpointConfigName (\ s a -> s{_ueEndpointConfigName = a})

instance AWSRequest UpdateEndpoint where
        type Rs UpdateEndpoint = UpdateEndpointResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEndpointResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "EndpointArn"))

instance Hashable UpdateEndpoint where

instance NFData UpdateEndpoint where

instance ToHeaders UpdateEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEndpoint where
        toJSON UpdateEndpoint'{..}
          = object
              (catMaybes
                 [Just ("EndpointName" .= _ueEndpointName),
                  Just
                    ("EndpointConfigName" .= _ueEndpointConfigName)])

instance ToPath UpdateEndpoint where
        toPath = const "/"

instance ToQuery UpdateEndpoint where
        toQuery = const mempty

-- | /See:/ 'updateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { _uersResponseStatus :: !Int
  , _uersEndpointARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uersResponseStatus' - -- | The response status code.
--
-- * 'uersEndpointARN' - The Amazon Resource Name (ARN) of the endpoint.
updateEndpointResponse
    :: Int -- ^ 'uersResponseStatus'
    -> Text -- ^ 'uersEndpointARN'
    -> UpdateEndpointResponse
updateEndpointResponse pResponseStatus_ pEndpointARN_ =
  UpdateEndpointResponse'
    {_uersResponseStatus = pResponseStatus_, _uersEndpointARN = pEndpointARN_}


-- | -- | The response status code.
uersResponseStatus :: Lens' UpdateEndpointResponse Int
uersResponseStatus = lens _uersResponseStatus (\ s a -> s{_uersResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the endpoint.
uersEndpointARN :: Lens' UpdateEndpointResponse Text
uersEndpointARN = lens _uersEndpointARN (\ s a -> s{_uersEndpointARN = a})

instance NFData UpdateEndpointResponse where
