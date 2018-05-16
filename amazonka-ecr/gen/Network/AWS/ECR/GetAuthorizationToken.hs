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
-- Module      : Network.AWS.ECR.GetAuthorizationToken
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token that is valid for a specified registry for 12 hours. This command allows you to use the @docker@ CLI to push and pull images with Amazon ECR. If you do not specify a registry, the default registry is assumed.
--
--
-- The @authorizationToken@ returned for each registry specified is a base64 encoded string that can be decoded and used in a @docker login@ command to authenticate to a registry. The AWS CLI offers an @aws ecr get-login@ command that simplifies the login process.
--
module Network.AWS.ECR.GetAuthorizationToken
    (
    -- * Creating a Request
      getAuthorizationToken
    , GetAuthorizationToken
    -- * Request Lenses
    , gatRegistryIds

    -- * Destructuring the Response
    , getAuthorizationTokenResponse
    , GetAuthorizationTokenResponse
    -- * Response Lenses
    , gatrsAuthorizationData
    , gatrsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAuthorizationToken' smart constructor.
newtype GetAuthorizationToken = GetAuthorizationToken'
  { _gatRegistryIds :: Maybe (List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizationToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatRegistryIds' - A list of AWS account IDs that are associated with the registries for which to get authorization tokens. If you do not specify a registry, the default registry is assumed.
getAuthorizationToken
    :: GetAuthorizationToken
getAuthorizationToken = GetAuthorizationToken' {_gatRegistryIds = Nothing}


-- | A list of AWS account IDs that are associated with the registries for which to get authorization tokens. If you do not specify a registry, the default registry is assumed.
gatRegistryIds :: Lens' GetAuthorizationToken (Maybe (NonEmpty Text))
gatRegistryIds = lens _gatRegistryIds (\ s a -> s{_gatRegistryIds = a}) . mapping _List1

instance AWSRequest GetAuthorizationToken where
        type Rs GetAuthorizationToken =
             GetAuthorizationTokenResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 GetAuthorizationTokenResponse' <$>
                   (x .?> "authorizationData" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetAuthorizationToken where

instance NFData GetAuthorizationToken where

instance ToHeaders GetAuthorizationToken where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.GetAuthorizationToken"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAuthorizationToken where
        toJSON GetAuthorizationToken'{..}
          = object
              (catMaybes [("registryIds" .=) <$> _gatRegistryIds])

instance ToPath GetAuthorizationToken where
        toPath = const "/"

instance ToQuery GetAuthorizationToken where
        toQuery = const mempty

-- | /See:/ 'getAuthorizationTokenResponse' smart constructor.
data GetAuthorizationTokenResponse = GetAuthorizationTokenResponse'
  { _gatrsAuthorizationData :: !(Maybe [AuthorizationData])
  , _gatrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizationTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatrsAuthorizationData' - A list of authorization token data objects that correspond to the @registryIds@ values in the request.
--
-- * 'gatrsResponseStatus' - -- | The response status code.
getAuthorizationTokenResponse
    :: Int -- ^ 'gatrsResponseStatus'
    -> GetAuthorizationTokenResponse
getAuthorizationTokenResponse pResponseStatus_ =
  GetAuthorizationTokenResponse'
    {_gatrsAuthorizationData = Nothing, _gatrsResponseStatus = pResponseStatus_}


-- | A list of authorization token data objects that correspond to the @registryIds@ values in the request.
gatrsAuthorizationData :: Lens' GetAuthorizationTokenResponse [AuthorizationData]
gatrsAuthorizationData = lens _gatrsAuthorizationData (\ s a -> s{_gatrsAuthorizationData = a}) . _Default . _Coerce

-- | -- | The response status code.
gatrsResponseStatus :: Lens' GetAuthorizationTokenResponse Int
gatrsResponseStatus = lens _gatrsResponseStatus (\ s a -> s{_gatrsResponseStatus = a})

instance NFData GetAuthorizationTokenResponse where
