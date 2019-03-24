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
-- Module      : Network.AWS.Connect.GetFederationToken
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
--
--
module Network.AWS.Connect.GetFederationToken
    (
    -- * Creating a Request
      getFederationToken
    , GetFederationToken
    -- * Request Lenses
    , gftInstanceId

    -- * Destructuring the Response
    , getFederationTokenResponse
    , GetFederationTokenResponse
    -- * Response Lenses
    , gftrsCredentials
    , gftrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFederationToken' smart constructor.
newtype GetFederationToken = GetFederationToken'
  { _gftInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFederationToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
getFederationToken
    :: Text -- ^ 'gftInstanceId'
    -> GetFederationToken
getFederationToken pInstanceId_ =
  GetFederationToken' {_gftInstanceId = pInstanceId_}


-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
gftInstanceId :: Lens' GetFederationToken Text
gftInstanceId = lens _gftInstanceId (\ s a -> s{_gftInstanceId = a})

instance AWSRequest GetFederationToken where
        type Rs GetFederationToken =
             GetFederationTokenResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 GetFederationTokenResponse' <$>
                   (x .?> "Credentials") <*> (pure (fromEnum s)))

instance Hashable GetFederationToken where

instance NFData GetFederationToken where

instance ToHeaders GetFederationToken where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetFederationToken where
        toPath GetFederationToken'{..}
          = mconcat ["/user/federate/", toBS _gftInstanceId]

instance ToQuery GetFederationToken where
        toQuery = const mempty

-- | /See:/ 'getFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { _gftrsCredentials    :: !(Maybe Credentials)
  , _gftrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFederationTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftrsCredentials' - The credentials to use for federation.
--
-- * 'gftrsResponseStatus' - -- | The response status code.
getFederationTokenResponse
    :: Int -- ^ 'gftrsResponseStatus'
    -> GetFederationTokenResponse
getFederationTokenResponse pResponseStatus_ =
  GetFederationTokenResponse'
    {_gftrsCredentials = Nothing, _gftrsResponseStatus = pResponseStatus_}


-- | The credentials to use for federation.
gftrsCredentials :: Lens' GetFederationTokenResponse (Maybe Credentials)
gftrsCredentials = lens _gftrsCredentials (\ s a -> s{_gftrsCredentials = a})

-- | -- | The response status code.
gftrsResponseStatus :: Lens' GetFederationTokenResponse Int
gftrsResponseStatus = lens _gftrsResponseStatus (\ s a -> s{_gftrsResponseStatus = a})

instance NFData GetFederationTokenResponse where
