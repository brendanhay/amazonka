{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked @DestinationUser@ ) signs in, they must create a new user account. See <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminLinkProviderForUser.html AdminLinkProviderForUser> .
--
-- This action is enabled only for admin access and requires developer credentials.
-- The @ProviderName@ must match the value specified when creating an IdP for the pool.
-- To disable a native username + password user, the @ProviderName@ value must be @Cognito@ and the @ProviderAttributeName@ must be @Cognito_Subject@ , with the @ProviderAttributeValue@ being the name that is used in the user pool for the user.
-- The @ProviderAttributeName@ must always be @Cognito_Subject@ for social identity providers. The @ProviderAttributeValue@ must always be the exact subject that was used when the user was originally linked as a source user.
-- For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the @ProviderAttributeName@ and @ProviderAttributeValue@ must be the same values that were used for the @SourceUser@ when the identities were originally linked using @AdminLinkProviderForUser@ call. (If the linking was done with @ProviderAttributeName@ set to @Cognito_Subject@ , the same applies here). However, if the user has already signed in, the @ProviderAttributeName@ must be @Cognito_Subject@ and @ProviderAttributeValue@ must be the subject of the SAML assertion.
module Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
  ( -- * Creating a request
    AdminDisableProviderForUser (..),
    mkAdminDisableProviderForUser,

    -- ** Request lenses
    adpfuUserPoolId,
    adpfuUser,

    -- * Destructuring the response
    AdminDisableProviderForUserResponse (..),
    mkAdminDisableProviderForUserResponse,

    -- ** Response lenses
    adpfursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminDisableProviderForUser' smart constructor.
data AdminDisableProviderForUser = AdminDisableProviderForUser'
  { userPoolId ::
      Lude.Text,
    user :: ProviderUserIdentifierType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDisableProviderForUser' with the minimum fields required to make a request.
--
-- * 'user' - The user to be disabled.
-- * 'userPoolId' - The user pool ID for the user pool.
mkAdminDisableProviderForUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'user'
  ProviderUserIdentifierType ->
  AdminDisableProviderForUser
mkAdminDisableProviderForUser pUserPoolId_ pUser_ =
  AdminDisableProviderForUser'
    { userPoolId = pUserPoolId_,
      user = pUser_
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfuUserPoolId :: Lens.Lens' AdminDisableProviderForUser Lude.Text
adpfuUserPoolId = Lens.lens (userPoolId :: AdminDisableProviderForUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminDisableProviderForUser)
{-# DEPRECATED adpfuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user to be disabled.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfuUser :: Lens.Lens' AdminDisableProviderForUser ProviderUserIdentifierType
adpfuUser = Lens.lens (user :: AdminDisableProviderForUser -> ProviderUserIdentifierType) (\s a -> s {user = a} :: AdminDisableProviderForUser)
{-# DEPRECATED adpfuUser "Use generic-lens or generic-optics with 'user' instead." #-}

instance Lude.AWSRequest AdminDisableProviderForUser where
  type
    Rs AdminDisableProviderForUser =
      AdminDisableProviderForUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminDisableProviderForUserResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminDisableProviderForUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminDisableProviderForUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminDisableProviderForUser where
  toJSON AdminDisableProviderForUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("User" Lude..= user)
          ]
      )

instance Lude.ToPath AdminDisableProviderForUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminDisableProviderForUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminDisableProviderForUserResponse' smart constructor.
newtype AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse'
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

-- | Creates a value of 'AdminDisableProviderForUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminDisableProviderForUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminDisableProviderForUserResponse
mkAdminDisableProviderForUserResponse pResponseStatus_ =
  AdminDisableProviderForUserResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfursResponseStatus :: Lens.Lens' AdminDisableProviderForUserResponse Lude.Int
adpfursResponseStatus = Lens.lens (responseStatus :: AdminDisableProviderForUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminDisableProviderForUserResponse)
{-# DEPRECATED adpfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
