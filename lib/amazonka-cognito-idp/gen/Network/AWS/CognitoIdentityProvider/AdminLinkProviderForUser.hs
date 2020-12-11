{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an existing user account in a user pool (@DestinationUser@ ) to an identity from an external identity provider (@SourceUser@ ) based on a specified attribute name and value from the external identity provider. This allows you to create a link from the existing user account to an external federated user identity that has not yet been used to sign in, so that the federated user identity can be used to sign in as the existing user account.
--
-- For example, if there is an existing user with a username and password, this API links that user to a federated user identity, so that when the federated user identity is used, the user signs in as the existing user account.
-- /Important:/ Because this API allows a user with an external federated identity to sign in as an existing user in the user pool, it is critical that it only be used with external identity providers and provider attributes that have been trusted by the application owner.
-- This action is enabled only for admin access and requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
  ( -- * Creating a request
    AdminLinkProviderForUser (..),
    mkAdminLinkProviderForUser,

    -- ** Request lenses
    alpfuUserPoolId,
    alpfuDestinationUser,
    alpfuSourceUser,

    -- * Destructuring the response
    AdminLinkProviderForUserResponse (..),
    mkAdminLinkProviderForUserResponse,

    -- ** Response lenses
    alpfursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminLinkProviderForUser' smart constructor.
data AdminLinkProviderForUser = AdminLinkProviderForUser'
  { userPoolId ::
      Lude.Text,
    destinationUser ::
      ProviderUserIdentifierType,
    sourceUser :: ProviderUserIdentifierType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminLinkProviderForUser' with the minimum fields required to make a request.
--
-- * 'destinationUser' - The existing user in the user pool to be linked to the external identity provider user account. Can be a native (Username + Password) Cognito User Pools user or a federated user (for example, a SAML or Facebook user). If the user doesn't exist, an exception is thrown. This is the user that is returned when the new user (with the linked identity provider attribute) signs in.
--
-- For a native username + password user, the @ProviderAttributeValue@ for the @DestinationUser@ should be the username in the user pool. For a federated user, it should be the provider-specific @user_id@ .
-- The @ProviderAttributeName@ of the @DestinationUser@ is ignored.
-- The @ProviderName@ should be set to @Cognito@ for users in Cognito user pools.
-- * 'sourceUser' - An external identity provider account for a user who does not currently exist yet in the user pool. This user must be a federated user (for example, a SAML or Facebook user), not another native user.
--
-- If the @SourceUser@ is a federated social identity provider user (Facebook, Google, or Login with Amazon), you must set the @ProviderAttributeName@ to @Cognito_Subject@ . For social identity providers, the @ProviderName@ will be @Facebook@ , @Google@ , or @LoginWithAmazon@ , and Cognito will automatically parse the Facebook, Google, and Login with Amazon tokens for @id@ , @sub@ , and @user_id@ , respectively. The @ProviderAttributeValue@ for the user must be the same value as the @id@ , @sub@ , or @user_id@ value found in the social identity provider token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a claim in the SAML assertion. If you wish to link SAML users based on the subject of the SAML assertion, you should map the subject to a claim through the SAML identity provider and submit that claim name as the @ProviderAttributeName@ . If you set @ProviderAttributeName@ to @Cognito_Subject@ , Cognito will automatically parse the default unique identifier found in the subject from the SAML token.
-- * 'userPoolId' - The user pool ID for the user pool.
mkAdminLinkProviderForUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'destinationUser'
  ProviderUserIdentifierType ->
  -- | 'sourceUser'
  ProviderUserIdentifierType ->
  AdminLinkProviderForUser
mkAdminLinkProviderForUser
  pUserPoolId_
  pDestinationUser_
  pSourceUser_ =
    AdminLinkProviderForUser'
      { userPoolId = pUserPoolId_,
        destinationUser = pDestinationUser_,
        sourceUser = pSourceUser_
      }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpfuUserPoolId :: Lens.Lens' AdminLinkProviderForUser Lude.Text
alpfuUserPoolId = Lens.lens (userPoolId :: AdminLinkProviderForUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminLinkProviderForUser)
{-# DEPRECATED alpfuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The existing user in the user pool to be linked to the external identity provider user account. Can be a native (Username + Password) Cognito User Pools user or a federated user (for example, a SAML or Facebook user). If the user doesn't exist, an exception is thrown. This is the user that is returned when the new user (with the linked identity provider attribute) signs in.
--
-- For a native username + password user, the @ProviderAttributeValue@ for the @DestinationUser@ should be the username in the user pool. For a federated user, it should be the provider-specific @user_id@ .
-- The @ProviderAttributeName@ of the @DestinationUser@ is ignored.
-- The @ProviderName@ should be set to @Cognito@ for users in Cognito user pools.
--
-- /Note:/ Consider using 'destinationUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpfuDestinationUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
alpfuDestinationUser = Lens.lens (destinationUser :: AdminLinkProviderForUser -> ProviderUserIdentifierType) (\s a -> s {destinationUser = a} :: AdminLinkProviderForUser)
{-# DEPRECATED alpfuDestinationUser "Use generic-lens or generic-optics with 'destinationUser' instead." #-}

-- | An external identity provider account for a user who does not currently exist yet in the user pool. This user must be a federated user (for example, a SAML or Facebook user), not another native user.
--
-- If the @SourceUser@ is a federated social identity provider user (Facebook, Google, or Login with Amazon), you must set the @ProviderAttributeName@ to @Cognito_Subject@ . For social identity providers, the @ProviderName@ will be @Facebook@ , @Google@ , or @LoginWithAmazon@ , and Cognito will automatically parse the Facebook, Google, and Login with Amazon tokens for @id@ , @sub@ , and @user_id@ , respectively. The @ProviderAttributeValue@ for the user must be the same value as the @id@ , @sub@ , or @user_id@ value found in the social identity provider token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a claim in the SAML assertion. If you wish to link SAML users based on the subject of the SAML assertion, you should map the subject to a claim through the SAML identity provider and submit that claim name as the @ProviderAttributeName@ . If you set @ProviderAttributeName@ to @Cognito_Subject@ , Cognito will automatically parse the default unique identifier found in the subject from the SAML token.
--
-- /Note:/ Consider using 'sourceUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpfuSourceUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
alpfuSourceUser = Lens.lens (sourceUser :: AdminLinkProviderForUser -> ProviderUserIdentifierType) (\s a -> s {sourceUser = a} :: AdminLinkProviderForUser)
{-# DEPRECATED alpfuSourceUser "Use generic-lens or generic-optics with 'sourceUser' instead." #-}

instance Lude.AWSRequest AdminLinkProviderForUser where
  type Rs AdminLinkProviderForUser = AdminLinkProviderForUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminLinkProviderForUserResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminLinkProviderForUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminLinkProviderForUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminLinkProviderForUser where
  toJSON AdminLinkProviderForUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("DestinationUser" Lude..= destinationUser),
            Lude.Just ("SourceUser" Lude..= sourceUser)
          ]
      )

instance Lude.ToPath AdminLinkProviderForUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminLinkProviderForUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminLinkProviderForUserResponse' smart constructor.
newtype AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminLinkProviderForUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminLinkProviderForUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminLinkProviderForUserResponse
mkAdminLinkProviderForUserResponse pResponseStatus_ =
  AdminLinkProviderForUserResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpfursResponseStatus :: Lens.Lens' AdminLinkProviderForUserResponse Lude.Int
alpfursResponseStatus = Lens.lens (responseStatus :: AdminLinkProviderForUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminLinkProviderForUserResponse)
{-# DEPRECATED alpfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
