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
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the policy for the specified application.
--
--
module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
    (
    -- * Creating a Request
      getApplicationPolicy
    , GetApplicationPolicy
    -- * Request Lenses
    , gapApplicationId

    -- * Destructuring the Response
    , getApplicationPolicyResponse
    , GetApplicationPolicyResponse
    -- * Response Lenses
    , gaprsStatements
    , gaprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'getApplicationPolicy' smart constructor.
newtype GetApplicationPolicy = GetApplicationPolicy'
  { _gapApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApplicationPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapApplicationId' - The ID of the application to get.
getApplicationPolicy
    :: Text -- ^ 'gapApplicationId'
    -> GetApplicationPolicy
getApplicationPolicy pApplicationId_ =
  GetApplicationPolicy' {_gapApplicationId = pApplicationId_}


-- | The ID of the application to get.
gapApplicationId :: Lens' GetApplicationPolicy Text
gapApplicationId = lens _gapApplicationId (\ s a -> s{_gapApplicationId = a})

instance AWSRequest GetApplicationPolicy where
        type Rs GetApplicationPolicy =
             GetApplicationPolicyResponse
        request = get serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationPolicyResponse' <$>
                   (x .?> "statements" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetApplicationPolicy where

instance NFData GetApplicationPolicy where

instance ToHeaders GetApplicationPolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetApplicationPolicy where
        toPath GetApplicationPolicy'{..}
          = mconcat
              ["/applications/", toBS _gapApplicationId, "/policy"]

instance ToQuery GetApplicationPolicy where
        toQuery = const mempty

-- | /See:/ 'getApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { _gaprsStatements     :: !(Maybe [ApplicationPolicyStatement])
  , _gaprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApplicationPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaprsStatements' - Array of policy statements applied to the application.
--
-- * 'gaprsResponseStatus' - -- | The response status code.
getApplicationPolicyResponse
    :: Int -- ^ 'gaprsResponseStatus'
    -> GetApplicationPolicyResponse
getApplicationPolicyResponse pResponseStatus_ =
  GetApplicationPolicyResponse'
    {_gaprsStatements = Nothing, _gaprsResponseStatus = pResponseStatus_}


-- | Array of policy statements applied to the application.
gaprsStatements :: Lens' GetApplicationPolicyResponse [ApplicationPolicyStatement]
gaprsStatements = lens _gaprsStatements (\ s a -> s{_gaprsStatements = a}) . _Default . _Coerce

-- | -- | The response status code.
gaprsResponseStatus :: Lens' GetApplicationPolicyResponse Int
gaprsResponseStatus = lens _gaprsResponseStatus (\ s a -> s{_gaprsResponseStatus = a})

instance NFData GetApplicationPolicyResponse where
