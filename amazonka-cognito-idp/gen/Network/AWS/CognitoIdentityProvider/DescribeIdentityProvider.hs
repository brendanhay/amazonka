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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific identity provider.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
    (
    -- * Creating a Request
      describeIdentityProvider
    , DescribeIdentityProvider
    -- * Request Lenses
    , dipUserPoolId
    , dipProviderName

    -- * Destructuring the Response
    , describeIdentityProviderResponse
    , DescribeIdentityProviderResponse
    -- * Response Lenses
    , diprsResponseStatus
    , diprsIdentityProvider
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { _dipUserPoolId   :: !Text
  , _dipProviderName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipUserPoolId' - The user pool ID.
--
-- * 'dipProviderName' - The identity provider name.
describeIdentityProvider
    :: Text -- ^ 'dipUserPoolId'
    -> Text -- ^ 'dipProviderName'
    -> DescribeIdentityProvider
describeIdentityProvider pUserPoolId_ pProviderName_ =
  DescribeIdentityProvider'
    {_dipUserPoolId = pUserPoolId_, _dipProviderName = pProviderName_}


-- | The user pool ID.
dipUserPoolId :: Lens' DescribeIdentityProvider Text
dipUserPoolId = lens _dipUserPoolId (\ s a -> s{_dipUserPoolId = a})

-- | The identity provider name.
dipProviderName :: Lens' DescribeIdentityProvider Text
dipProviderName = lens _dipProviderName (\ s a -> s{_dipProviderName = a})

instance AWSRequest DescribeIdentityProvider where
        type Rs DescribeIdentityProvider =
             DescribeIdentityProviderResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIdentityProviderResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "IdentityProvider"))

instance Hashable DescribeIdentityProvider where

instance NFData DescribeIdentityProvider where

instance ToHeaders DescribeIdentityProvider where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeIdentityProvider"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeIdentityProvider where
        toJSON DescribeIdentityProvider'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _dipUserPoolId),
                  Just ("ProviderName" .= _dipProviderName)])

instance ToPath DescribeIdentityProvider where
        toPath = const "/"

instance ToQuery DescribeIdentityProvider where
        toQuery = const mempty

-- | /See:/ 'describeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { _diprsResponseStatus   :: !Int
  , _diprsIdentityProvider :: !IdentityProviderType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentityProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diprsResponseStatus' - -- | The response status code.
--
-- * 'diprsIdentityProvider' - The identity provider that was deleted.
describeIdentityProviderResponse
    :: Int -- ^ 'diprsResponseStatus'
    -> IdentityProviderType -- ^ 'diprsIdentityProvider'
    -> DescribeIdentityProviderResponse
describeIdentityProviderResponse pResponseStatus_ pIdentityProvider_ =
  DescribeIdentityProviderResponse'
    { _diprsResponseStatus = pResponseStatus_
    , _diprsIdentityProvider = pIdentityProvider_
    }


-- | -- | The response status code.
diprsResponseStatus :: Lens' DescribeIdentityProviderResponse Int
diprsResponseStatus = lens _diprsResponseStatus (\ s a -> s{_diprsResponseStatus = a})

-- | The identity provider that was deleted.
diprsIdentityProvider :: Lens' DescribeIdentityProviderResponse IdentityProviderType
diprsIdentityProvider = lens _diprsIdentityProvider (\ s a -> s{_diprsIdentityProvider = a})

instance NFData DescribeIdentityProviderResponse
         where
