{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the user from signing in with the specified external (SAML or
-- social) identity provider. If the user to disable is a Cognito User
-- Pools native username + password user, they are not permitted to use
-- their password to sign-in. If the user to disable is a linked external
-- IdP user, any link between that user and an existing user is removed.
-- The next time the external user (no longer attached to the previously
-- linked @DestinationUser@) signs in, they must create a new user account.
-- See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminLinkProviderForUser.html AdminLinkProviderForUser>.
--
-- This action is enabled only for admin access and requires developer
-- credentials.
--
-- The @ProviderName@ must match the value specified when creating an IdP
-- for the pool.
--
-- To disable a native username + password user, the @ProviderName@ value
-- must be @Cognito@ and the @ProviderAttributeName@ must be
-- @Cognito_Subject@, with the @ProviderAttributeValue@ being the name that
-- is used in the user pool for the user.
--
-- The @ProviderAttributeName@ must always be @Cognito_Subject@ for social
-- identity providers. The @ProviderAttributeValue@ must always be the
-- exact subject that was used when the user was originally linked as a
-- source user.
--
-- For de-linking a SAML identity, there are two scenarios. If the linked
-- identity has not yet been used to sign-in, the @ProviderAttributeName@
-- and @ProviderAttributeValue@ must be the same values that were used for
-- the @SourceUser@ when the identities were originally linked using
-- @ AdminLinkProviderForUser@ call. (If the linking was done with
-- @ProviderAttributeName@ set to @Cognito_Subject@, the same applies
-- here). However, if the user has already signed in, the
-- @ProviderAttributeName@ must be @Cognito_Subject@ and
-- @ProviderAttributeValue@ must be the subject of the SAML assertion.
module Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
  ( -- * Creating a Request
    AdminDisableProviderForUser (..),
    newAdminDisableProviderForUser,

    -- * Request Lenses
    adminDisableProviderForUser_userPoolId,
    adminDisableProviderForUser_user,

    -- * Destructuring the Response
    AdminDisableProviderForUserResponse (..),
    newAdminDisableProviderForUserResponse,

    -- * Response Lenses
    adminDisableProviderForUserResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminDisableProviderForUser' smart constructor.
data AdminDisableProviderForUser = AdminDisableProviderForUser'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The user to be disabled.
    user :: ProviderUserIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDisableProviderForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminDisableProviderForUser_userPoolId' - The user pool ID for the user pool.
--
-- 'user', 'adminDisableProviderForUser_user' - The user to be disabled.
newAdminDisableProviderForUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'user'
  ProviderUserIdentifierType ->
  AdminDisableProviderForUser
newAdminDisableProviderForUser pUserPoolId_ pUser_ =
  AdminDisableProviderForUser'
    { userPoolId =
        pUserPoolId_,
      user = pUser_
    }

-- | The user pool ID for the user pool.
adminDisableProviderForUser_userPoolId :: Lens.Lens' AdminDisableProviderForUser Prelude.Text
adminDisableProviderForUser_userPoolId = Lens.lens (\AdminDisableProviderForUser' {userPoolId} -> userPoolId) (\s@AdminDisableProviderForUser' {} a -> s {userPoolId = a} :: AdminDisableProviderForUser)

-- | The user to be disabled.
adminDisableProviderForUser_user :: Lens.Lens' AdminDisableProviderForUser ProviderUserIdentifierType
adminDisableProviderForUser_user = Lens.lens (\AdminDisableProviderForUser' {user} -> user) (\s@AdminDisableProviderForUser' {} a -> s {user = a} :: AdminDisableProviderForUser)

instance
  Prelude.AWSRequest
    AdminDisableProviderForUser
  where
  type
    Rs AdminDisableProviderForUser =
      AdminDisableProviderForUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminDisableProviderForUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminDisableProviderForUser

instance Prelude.NFData AdminDisableProviderForUser

instance
  Prelude.ToHeaders
    AdminDisableProviderForUser
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminDisableProviderForUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminDisableProviderForUser where
  toJSON AdminDisableProviderForUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("User" Prelude..= user)
          ]
      )

instance Prelude.ToPath AdminDisableProviderForUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminDisableProviderForUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminDisableProviderForUserResponse' smart constructor.
data AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDisableProviderForUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminDisableProviderForUserResponse_httpStatus' - The response's http status code.
newAdminDisableProviderForUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminDisableProviderForUserResponse
newAdminDisableProviderForUserResponse pHttpStatus_ =
  AdminDisableProviderForUserResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminDisableProviderForUserResponse_httpStatus :: Lens.Lens' AdminDisableProviderForUserResponse Prelude.Int
adminDisableProviderForUserResponse_httpStatus = Lens.lens (\AdminDisableProviderForUserResponse' {httpStatus} -> httpStatus) (\s@AdminDisableProviderForUserResponse' {} a -> s {httpStatus = a} :: AdminDisableProviderForUserResponse)

instance
  Prelude.NFData
    AdminDisableProviderForUserResponse
