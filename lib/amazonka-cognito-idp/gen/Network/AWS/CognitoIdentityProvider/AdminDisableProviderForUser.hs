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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked @DestinationUser@ ) signs in, they must create a new user account. See .
--
--
-- This action is enabled only for admin access and requires developer credentials.
--
-- The @ProviderName@ must match the value specified when creating an IdP for the pool.
--
-- To disable a native username + password user, the @ProviderName@ value must be @Cognito@ and the @ProviderAttributeName@ must be @Cognito_Subject@ , with the @ProviderAttributeValue@ being the name that is used in the user pool for the user.
--
-- The @ProviderAttributeName@ must always be @Cognito_Subject@ for social identity providers. The @ProviderAttributeValue@ must always be the exact subject that was used when the user was originally linked as a source user.
--
-- For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the @ProviderAttributeName@ and @ProviderAttributeValue@ must be the same values that were used for the @SourceUser@ when the identities were originally linked in the call. (If the linking was done with @ProviderAttributeName@ set to @Cognito_Subject@ , the same applies here). However, if the user has already signed in, the @ProviderAttributeName@ must be @Cognito_Subject@ and @ProviderAttributeValue@ must be the subject of the SAML assertion.
--
module Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
    (
    -- * Creating a Request
      adminDisableProviderForUser
    , AdminDisableProviderForUser
    -- * Request Lenses
    , adpfuUserPoolId
    , adpfuUser

    -- * Destructuring the Response
    , adminDisableProviderForUserResponse
    , AdminDisableProviderForUserResponse
    -- * Response Lenses
    , adpfursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminDisableProviderForUser' smart constructor.
data AdminDisableProviderForUser = AdminDisableProviderForUser'
  { _adpfuUserPoolId :: !Text
  , _adpfuUser       :: !ProviderUserIdentifierType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDisableProviderForUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adpfuUserPoolId' - The user pool ID for the user pool.
--
-- * 'adpfuUser' - The user to be disabled.
adminDisableProviderForUser
    :: Text -- ^ 'adpfuUserPoolId'
    -> ProviderUserIdentifierType -- ^ 'adpfuUser'
    -> AdminDisableProviderForUser
adminDisableProviderForUser pUserPoolId_ pUser_ =
  AdminDisableProviderForUser'
    {_adpfuUserPoolId = pUserPoolId_, _adpfuUser = pUser_}


-- | The user pool ID for the user pool.
adpfuUserPoolId :: Lens' AdminDisableProviderForUser Text
adpfuUserPoolId = lens _adpfuUserPoolId (\ s a -> s{_adpfuUserPoolId = a})

-- | The user to be disabled.
adpfuUser :: Lens' AdminDisableProviderForUser ProviderUserIdentifierType
adpfuUser = lens _adpfuUser (\ s a -> s{_adpfuUser = a})

instance AWSRequest AdminDisableProviderForUser where
        type Rs AdminDisableProviderForUser =
             AdminDisableProviderForUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminDisableProviderForUserResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminDisableProviderForUser where

instance NFData AdminDisableProviderForUser where

instance ToHeaders AdminDisableProviderForUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminDisableProviderForUser"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminDisableProviderForUser where
        toJSON AdminDisableProviderForUser'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _adpfuUserPoolId),
                  Just ("User" .= _adpfuUser)])

instance ToPath AdminDisableProviderForUser where
        toPath = const "/"

instance ToQuery AdminDisableProviderForUser where
        toQuery = const mempty

-- | /See:/ 'adminDisableProviderForUserResponse' smart constructor.
newtype AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse'
  { _adpfursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDisableProviderForUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adpfursResponseStatus' - -- | The response status code.
adminDisableProviderForUserResponse
    :: Int -- ^ 'adpfursResponseStatus'
    -> AdminDisableProviderForUserResponse
adminDisableProviderForUserResponse pResponseStatus_ =
  AdminDisableProviderForUserResponse'
    {_adpfursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adpfursResponseStatus :: Lens' AdminDisableProviderForUserResponse Int
adpfursResponseStatus = lens _adpfursResponseStatus (\ s a -> s{_adpfursResponseStatus = a})

instance NFData AdminDisableProviderForUserResponse
         where
