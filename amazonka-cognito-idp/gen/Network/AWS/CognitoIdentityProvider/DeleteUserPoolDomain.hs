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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain for a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
    (
    -- * Creating a Request
      deleteUserPoolDomain
    , DeleteUserPoolDomain
    -- * Request Lenses
    , dupdDomain
    , dupdUserPoolId

    -- * Destructuring the Response
    , deleteUserPoolDomainResponse
    , DeleteUserPoolDomainResponse
    -- * Response Lenses
    , dupdrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserPoolDomain' smart constructor.
data DeleteUserPoolDomain = DeleteUserPoolDomain'
  { _dupdDomain     :: !Text
  , _dupdUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPoolDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupdDomain' - The domain string.
--
-- * 'dupdUserPoolId' - The user pool ID.
deleteUserPoolDomain
    :: Text -- ^ 'dupdDomain'
    -> Text -- ^ 'dupdUserPoolId'
    -> DeleteUserPoolDomain
deleteUserPoolDomain pDomain_ pUserPoolId_ =
  DeleteUserPoolDomain' {_dupdDomain = pDomain_, _dupdUserPoolId = pUserPoolId_}


-- | The domain string.
dupdDomain :: Lens' DeleteUserPoolDomain Text
dupdDomain = lens _dupdDomain (\ s a -> s{_dupdDomain = a})

-- | The user pool ID.
dupdUserPoolId :: Lens' DeleteUserPoolDomain Text
dupdUserPoolId = lens _dupdUserPoolId (\ s a -> s{_dupdUserPoolId = a})

instance AWSRequest DeleteUserPoolDomain where
        type Rs DeleteUserPoolDomain =
             DeleteUserPoolDomainResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUserPoolDomainResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteUserPoolDomain where

instance NFData DeleteUserPoolDomain where

instance ToHeaders DeleteUserPoolDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteUserPoolDomain"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserPoolDomain where
        toJSON DeleteUserPoolDomain'{..}
          = object
              (catMaybes
                 [Just ("Domain" .= _dupdDomain),
                  Just ("UserPoolId" .= _dupdUserPoolId)])

instance ToPath DeleteUserPoolDomain where
        toPath = const "/"

instance ToQuery DeleteUserPoolDomain where
        toQuery = const mempty

-- | /See:/ 'deleteUserPoolDomainResponse' smart constructor.
newtype DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse'
  { _dupdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupdrsResponseStatus' - -- | The response status code.
deleteUserPoolDomainResponse
    :: Int -- ^ 'dupdrsResponseStatus'
    -> DeleteUserPoolDomainResponse
deleteUserPoolDomainResponse pResponseStatus_ =
  DeleteUserPoolDomainResponse' {_dupdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dupdrsResponseStatus :: Lens' DeleteUserPoolDomainResponse Int
dupdrsResponseStatus = lens _dupdrsResponseStatus (\ s a -> s{_dupdrsResponseStatus = a})

instance NFData DeleteUserPoolDomainResponse where
