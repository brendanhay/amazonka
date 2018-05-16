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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an existing user account in a user pool (@DestinationUser@ ) to an identity from an external identity provider (@SourceUser@ ) based on a specified attribute name and value from the external identity provider. This allows you to create a link from the existing user account to an external federated user identity that has not yet been used to sign in, so that the federated user identity can be used to sign in as the existing user account.
--
--
-- For example, if there is an existing user with a username and password, this API links that user to a federated user identity, so that when the federated user identity is used, the user signs in as the existing user account.
--
-- /Important:/ Because this API allows a user with an external federated identity to sign in as an existing user in the user pool, it is critical that it only be used with external identity providers and provider attributes that have been trusted by the application owner.
--
-- See also .
--
-- This action is enabled only for admin access and requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
    (
    -- * Creating a Request
      adminLinkProviderForUser
    , AdminLinkProviderForUser
    -- * Request Lenses
    , alpfuUserPoolId
    , alpfuDestinationUser
    , alpfuSourceUser

    -- * Destructuring the Response
    , adminLinkProviderForUserResponse
    , AdminLinkProviderForUserResponse
    -- * Response Lenses
    , alpfursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminLinkProviderForUser' smart constructor.
data AdminLinkProviderForUser = AdminLinkProviderForUser'
  { _alpfuUserPoolId      :: !Text
  , _alpfuDestinationUser :: !ProviderUserIdentifierType
  , _alpfuSourceUser      :: !ProviderUserIdentifierType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminLinkProviderForUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alpfuUserPoolId' - The user pool ID for the user pool.
--
-- * 'alpfuDestinationUser' - The existing user in the user pool to be linked to the external identity provider user account. Can be a native (Username + Password) Cognito User Pools user or a federated user (for example, a SAML or Facebook user). If the user doesn't exist, an exception is thrown. This is the user that is returned when the new user (with the linked identity provider attribute) signs in. For a native username + password user, the @ProviderAttributeValue@ for the @DestinationUser@ should be the username in the user pool. For a federated user, it should be the provider-specific @user_id@ . The @ProviderAttributeName@ of the @DestinationUser@ is ignored. The @ProviderName@ should be set to @Cognito@ for users in Cognito user pools.
--
-- * 'alpfuSourceUser' - An external identity provider account for a user who does not currently exist yet in the user pool. This user must be a federated user (for example, a SAML or Facebook user), not another native user. If the @SourceUser@ is a federated social identity provider user (Facebook, Google, or Login with Amazon), you must set the @ProviderAttributeName@ to @Cognito_Subject@ . For social identity providers, the @ProviderName@ will be @Facebook@ , @Google@ , or @LoginWithAmazon@ , and Cognito will automatically parse the Facebook, Google, and Login with Amazon tokens for @id@ , @sub@ , and @user_id@ , respectively. The @ProviderAttributeValue@ for the user must be the same value as the @id@ , @sub@ , or @user_id@ value found in the social identity provider token. For SAML, the @ProviderAttributeName@ can be any value that matches a claim in the SAML assertion. If you wish to link SAML users based on the subject of the SAML assertion, you should map the subject to a claim through the SAML identity provider and submit that claim name as the @ProviderAttributeName@ . If you set @ProviderAttributeName@ to @Cognito_Subject@ , Cognito will automatically parse the default unique identifier found in the subject from the SAML token.
adminLinkProviderForUser
    :: Text -- ^ 'alpfuUserPoolId'
    -> ProviderUserIdentifierType -- ^ 'alpfuDestinationUser'
    -> ProviderUserIdentifierType -- ^ 'alpfuSourceUser'
    -> AdminLinkProviderForUser
adminLinkProviderForUser pUserPoolId_ pDestinationUser_ pSourceUser_ =
  AdminLinkProviderForUser'
    { _alpfuUserPoolId = pUserPoolId_
    , _alpfuDestinationUser = pDestinationUser_
    , _alpfuSourceUser = pSourceUser_
    }


-- | The user pool ID for the user pool.
alpfuUserPoolId :: Lens' AdminLinkProviderForUser Text
alpfuUserPoolId = lens _alpfuUserPoolId (\ s a -> s{_alpfuUserPoolId = a})

-- | The existing user in the user pool to be linked to the external identity provider user account. Can be a native (Username + Password) Cognito User Pools user or a federated user (for example, a SAML or Facebook user). If the user doesn't exist, an exception is thrown. This is the user that is returned when the new user (with the linked identity provider attribute) signs in. For a native username + password user, the @ProviderAttributeValue@ for the @DestinationUser@ should be the username in the user pool. For a federated user, it should be the provider-specific @user_id@ . The @ProviderAttributeName@ of the @DestinationUser@ is ignored. The @ProviderName@ should be set to @Cognito@ for users in Cognito user pools.
alpfuDestinationUser :: Lens' AdminLinkProviderForUser ProviderUserIdentifierType
alpfuDestinationUser = lens _alpfuDestinationUser (\ s a -> s{_alpfuDestinationUser = a})

-- | An external identity provider account for a user who does not currently exist yet in the user pool. This user must be a federated user (for example, a SAML or Facebook user), not another native user. If the @SourceUser@ is a federated social identity provider user (Facebook, Google, or Login with Amazon), you must set the @ProviderAttributeName@ to @Cognito_Subject@ . For social identity providers, the @ProviderName@ will be @Facebook@ , @Google@ , or @LoginWithAmazon@ , and Cognito will automatically parse the Facebook, Google, and Login with Amazon tokens for @id@ , @sub@ , and @user_id@ , respectively. The @ProviderAttributeValue@ for the user must be the same value as the @id@ , @sub@ , or @user_id@ value found in the social identity provider token. For SAML, the @ProviderAttributeName@ can be any value that matches a claim in the SAML assertion. If you wish to link SAML users based on the subject of the SAML assertion, you should map the subject to a claim through the SAML identity provider and submit that claim name as the @ProviderAttributeName@ . If you set @ProviderAttributeName@ to @Cognito_Subject@ , Cognito will automatically parse the default unique identifier found in the subject from the SAML token.
alpfuSourceUser :: Lens' AdminLinkProviderForUser ProviderUserIdentifierType
alpfuSourceUser = lens _alpfuSourceUser (\ s a -> s{_alpfuSourceUser = a})

instance AWSRequest AdminLinkProviderForUser where
        type Rs AdminLinkProviderForUser =
             AdminLinkProviderForUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminLinkProviderForUserResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminLinkProviderForUser where

instance NFData AdminLinkProviderForUser where

instance ToHeaders AdminLinkProviderForUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminLinkProviderForUser"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminLinkProviderForUser where
        toJSON AdminLinkProviderForUser'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _alpfuUserPoolId),
                  Just ("DestinationUser" .= _alpfuDestinationUser),
                  Just ("SourceUser" .= _alpfuSourceUser)])

instance ToPath AdminLinkProviderForUser where
        toPath = const "/"

instance ToQuery AdminLinkProviderForUser where
        toQuery = const mempty

-- | /See:/ 'adminLinkProviderForUserResponse' smart constructor.
newtype AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse'
  { _alpfursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminLinkProviderForUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alpfursResponseStatus' - -- | The response status code.
adminLinkProviderForUserResponse
    :: Int -- ^ 'alpfursResponseStatus'
    -> AdminLinkProviderForUserResponse
adminLinkProviderForUserResponse pResponseStatus_ =
  AdminLinkProviderForUserResponse' {_alpfursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
alpfursResponseStatus :: Lens' AdminLinkProviderForUserResponse Int
alpfursResponseStatus = lens _alpfursResponseStatus (\ s a -> s{_alpfursResponseStatus = a})

instance NFData AdminLinkProviderForUserResponse
         where
