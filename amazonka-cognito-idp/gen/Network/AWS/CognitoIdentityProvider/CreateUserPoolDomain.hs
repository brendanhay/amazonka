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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain for a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
    (
    -- * Creating a Request
      createUserPoolDomain
    , CreateUserPoolDomain
    -- * Request Lenses
    , cupdDomain
    , cupdUserPoolId

    -- * Destructuring the Response
    , createUserPoolDomainResponse
    , CreateUserPoolDomainResponse
    -- * Response Lenses
    , cupdrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUserPoolDomain' smart constructor.
data CreateUserPoolDomain = CreateUserPoolDomain'
  { _cupdDomain     :: !Text
  , _cupdUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserPoolDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupdDomain' - The domain string.
--
-- * 'cupdUserPoolId' - The user pool ID.
createUserPoolDomain
    :: Text -- ^ 'cupdDomain'
    -> Text -- ^ 'cupdUserPoolId'
    -> CreateUserPoolDomain
createUserPoolDomain pDomain_ pUserPoolId_ =
  CreateUserPoolDomain' {_cupdDomain = pDomain_, _cupdUserPoolId = pUserPoolId_}


-- | The domain string.
cupdDomain :: Lens' CreateUserPoolDomain Text
cupdDomain = lens _cupdDomain (\ s a -> s{_cupdDomain = a})

-- | The user pool ID.
cupdUserPoolId :: Lens' CreateUserPoolDomain Text
cupdUserPoolId = lens _cupdUserPoolId (\ s a -> s{_cupdUserPoolId = a})

instance AWSRequest CreateUserPoolDomain where
        type Rs CreateUserPoolDomain =
             CreateUserPoolDomainResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 CreateUserPoolDomainResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateUserPoolDomain where

instance NFData CreateUserPoolDomain where

instance ToHeaders CreateUserPoolDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateUserPoolDomain"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserPoolDomain where
        toJSON CreateUserPoolDomain'{..}
          = object
              (catMaybes
                 [Just ("Domain" .= _cupdDomain),
                  Just ("UserPoolId" .= _cupdUserPoolId)])

instance ToPath CreateUserPoolDomain where
        toPath = const "/"

instance ToQuery CreateUserPoolDomain where
        toQuery = const mempty

-- | /See:/ 'createUserPoolDomainResponse' smart constructor.
newtype CreateUserPoolDomainResponse = CreateUserPoolDomainResponse'
  { _cupdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupdrsResponseStatus' - -- | The response status code.
createUserPoolDomainResponse
    :: Int -- ^ 'cupdrsResponseStatus'
    -> CreateUserPoolDomainResponse
createUserPoolDomainResponse pResponseStatus_ =
  CreateUserPoolDomainResponse' {_cupdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cupdrsResponseStatus :: Lens' CreateUserPoolDomainResponse Int
cupdrsResponseStatus = lens _cupdrsResponseStatus (\ s a -> s{_cupdrsResponseStatus = a})

instance NFData CreateUserPoolDomainResponse where
