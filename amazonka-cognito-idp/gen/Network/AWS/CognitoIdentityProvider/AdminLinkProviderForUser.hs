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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an existing user account in a user pool (@DestinationUser@) to an
-- identity from an external identity provider (@SourceUser@) based on a
-- specified attribute name and value from the external identity provider.
-- This allows you to create a link from the existing user account to an
-- external federated user identity that has not yet been used to sign in,
-- so that the federated user identity can be used to sign in as the
-- existing user account.
--
-- For example, if there is an existing user with a username and password,
-- this API links that user to a federated user identity, so that when the
-- federated user identity is used, the user signs in as the existing user
-- account.
--
-- The maximum number of federated identities linked to a user is 5.
--
-- Because this API allows a user with an external federated identity to
-- sign in as an existing user in the user pool, it is critical that it
-- only be used with external identity providers and provider attributes
-- that have been trusted by the application owner.
--
-- This action is enabled only for admin access and requires developer
-- credentials.
module Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
  ( -- * Creating a Request
    AdminLinkProviderForUser (..),
    newAdminLinkProviderForUser,

    -- * Request Lenses
    adminLinkProviderForUser_userPoolId,
    adminLinkProviderForUser_destinationUser,
    adminLinkProviderForUser_sourceUser,

    -- * Destructuring the Response
    AdminLinkProviderForUserResponse (..),
    newAdminLinkProviderForUserResponse,

    -- * Response Lenses
    adminLinkProviderForUserResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminLinkProviderForUser' smart constructor.
data AdminLinkProviderForUser = AdminLinkProviderForUser'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The existing user in the user pool to be linked to the external identity
    -- provider user account. Can be a native (Username + Password) Cognito
    -- User Pools user or a federated user (for example, a SAML or Facebook
    -- user). If the user doesn\'t exist, an exception is thrown. This is the
    -- user that is returned when the new user (with the linked identity
    -- provider attribute) signs in.
    --
    -- For a native username + password user, the @ProviderAttributeValue@ for
    -- the @DestinationUser@ should be the username in the user pool. For a
    -- federated user, it should be the provider-specific @user_id@.
    --
    -- The @ProviderAttributeName@ of the @DestinationUser@ is ignored.
    --
    -- The @ProviderName@ should be set to @Cognito@ for users in Cognito user
    -- pools.
    destinationUser :: ProviderUserIdentifierType,
    -- | An external identity provider account for a user who does not currently
    -- exist yet in the user pool. This user must be a federated user (for
    -- example, a SAML or Facebook user), not another native user.
    --
    -- If the @SourceUser@ is a federated social identity provider user
    -- (Facebook, Google, or Login with Amazon), you must set the
    -- @ProviderAttributeName@ to @Cognito_Subject@. For social identity
    -- providers, the @ProviderName@ will be @Facebook@, @Google@, or
    -- @LoginWithAmazon@, and Cognito will automatically parse the Facebook,
    -- Google, and Login with Amazon tokens for @id@, @sub@, and @user_id@,
    -- respectively. The @ProviderAttributeValue@ for the user must be the same
    -- value as the @id@, @sub@, or @user_id@ value found in the social
    -- identity provider token.
    --
    -- For SAML, the @ProviderAttributeName@ can be any value that matches a
    -- claim in the SAML assertion. If you wish to link SAML users based on the
    -- subject of the SAML assertion, you should map the subject to a claim
    -- through the SAML identity provider and submit that claim name as the
    -- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
    -- @Cognito_Subject@, Cognito will automatically parse the default unique
    -- identifier found in the subject from the SAML token.
    sourceUser :: ProviderUserIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminLinkProviderForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminLinkProviderForUser_userPoolId' - The user pool ID for the user pool.
--
-- 'destinationUser', 'adminLinkProviderForUser_destinationUser' - The existing user in the user pool to be linked to the external identity
-- provider user account. Can be a native (Username + Password) Cognito
-- User Pools user or a federated user (for example, a SAML or Facebook
-- user). If the user doesn\'t exist, an exception is thrown. This is the
-- user that is returned when the new user (with the linked identity
-- provider attribute) signs in.
--
-- For a native username + password user, the @ProviderAttributeValue@ for
-- the @DestinationUser@ should be the username in the user pool. For a
-- federated user, it should be the provider-specific @user_id@.
--
-- The @ProviderAttributeName@ of the @DestinationUser@ is ignored.
--
-- The @ProviderName@ should be set to @Cognito@ for users in Cognito user
-- pools.
--
-- 'sourceUser', 'adminLinkProviderForUser_sourceUser' - An external identity provider account for a user who does not currently
-- exist yet in the user pool. This user must be a federated user (for
-- example, a SAML or Facebook user), not another native user.
--
-- If the @SourceUser@ is a federated social identity provider user
-- (Facebook, Google, or Login with Amazon), you must set the
-- @ProviderAttributeName@ to @Cognito_Subject@. For social identity
-- providers, the @ProviderName@ will be @Facebook@, @Google@, or
-- @LoginWithAmazon@, and Cognito will automatically parse the Facebook,
-- Google, and Login with Amazon tokens for @id@, @sub@, and @user_id@,
-- respectively. The @ProviderAttributeValue@ for the user must be the same
-- value as the @id@, @sub@, or @user_id@ value found in the social
-- identity provider token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a
-- claim in the SAML assertion. If you wish to link SAML users based on the
-- subject of the SAML assertion, you should map the subject to a claim
-- through the SAML identity provider and submit that claim name as the
-- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
-- @Cognito_Subject@, Cognito will automatically parse the default unique
-- identifier found in the subject from the SAML token.
newAdminLinkProviderForUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'destinationUser'
  ProviderUserIdentifierType ->
  -- | 'sourceUser'
  ProviderUserIdentifierType ->
  AdminLinkProviderForUser
newAdminLinkProviderForUser
  pUserPoolId_
  pDestinationUser_
  pSourceUser_ =
    AdminLinkProviderForUser'
      { userPoolId =
          pUserPoolId_,
        destinationUser = pDestinationUser_,
        sourceUser = pSourceUser_
      }

-- | The user pool ID for the user pool.
adminLinkProviderForUser_userPoolId :: Lens.Lens' AdminLinkProviderForUser Prelude.Text
adminLinkProviderForUser_userPoolId = Lens.lens (\AdminLinkProviderForUser' {userPoolId} -> userPoolId) (\s@AdminLinkProviderForUser' {} a -> s {userPoolId = a} :: AdminLinkProviderForUser)

-- | The existing user in the user pool to be linked to the external identity
-- provider user account. Can be a native (Username + Password) Cognito
-- User Pools user or a federated user (for example, a SAML or Facebook
-- user). If the user doesn\'t exist, an exception is thrown. This is the
-- user that is returned when the new user (with the linked identity
-- provider attribute) signs in.
--
-- For a native username + password user, the @ProviderAttributeValue@ for
-- the @DestinationUser@ should be the username in the user pool. For a
-- federated user, it should be the provider-specific @user_id@.
--
-- The @ProviderAttributeName@ of the @DestinationUser@ is ignored.
--
-- The @ProviderName@ should be set to @Cognito@ for users in Cognito user
-- pools.
adminLinkProviderForUser_destinationUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
adminLinkProviderForUser_destinationUser = Lens.lens (\AdminLinkProviderForUser' {destinationUser} -> destinationUser) (\s@AdminLinkProviderForUser' {} a -> s {destinationUser = a} :: AdminLinkProviderForUser)

-- | An external identity provider account for a user who does not currently
-- exist yet in the user pool. This user must be a federated user (for
-- example, a SAML or Facebook user), not another native user.
--
-- If the @SourceUser@ is a federated social identity provider user
-- (Facebook, Google, or Login with Amazon), you must set the
-- @ProviderAttributeName@ to @Cognito_Subject@. For social identity
-- providers, the @ProviderName@ will be @Facebook@, @Google@, or
-- @LoginWithAmazon@, and Cognito will automatically parse the Facebook,
-- Google, and Login with Amazon tokens for @id@, @sub@, and @user_id@,
-- respectively. The @ProviderAttributeValue@ for the user must be the same
-- value as the @id@, @sub@, or @user_id@ value found in the social
-- identity provider token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a
-- claim in the SAML assertion. If you wish to link SAML users based on the
-- subject of the SAML assertion, you should map the subject to a claim
-- through the SAML identity provider and submit that claim name as the
-- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
-- @Cognito_Subject@, Cognito will automatically parse the default unique
-- identifier found in the subject from the SAML token.
adminLinkProviderForUser_sourceUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
adminLinkProviderForUser_sourceUser = Lens.lens (\AdminLinkProviderForUser' {sourceUser} -> sourceUser) (\s@AdminLinkProviderForUser' {} a -> s {sourceUser = a} :: AdminLinkProviderForUser)

instance Prelude.AWSRequest AdminLinkProviderForUser where
  type
    Rs AdminLinkProviderForUser =
      AdminLinkProviderForUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminLinkProviderForUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminLinkProviderForUser

instance Prelude.NFData AdminLinkProviderForUser

instance Prelude.ToHeaders AdminLinkProviderForUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminLinkProviderForUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminLinkProviderForUser where
  toJSON AdminLinkProviderForUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just
              ("DestinationUser" Prelude..= destinationUser),
            Prelude.Just ("SourceUser" Prelude..= sourceUser)
          ]
      )

instance Prelude.ToPath AdminLinkProviderForUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminLinkProviderForUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminLinkProviderForUserResponse' smart constructor.
data AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminLinkProviderForUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminLinkProviderForUserResponse_httpStatus' - The response's http status code.
newAdminLinkProviderForUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminLinkProviderForUserResponse
newAdminLinkProviderForUserResponse pHttpStatus_ =
  AdminLinkProviderForUserResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminLinkProviderForUserResponse_httpStatus :: Lens.Lens' AdminLinkProviderForUserResponse Prelude.Int
adminLinkProviderForUserResponse_httpStatus = Lens.lens (\AdminLinkProviderForUserResponse' {httpStatus} -> httpStatus) (\s@AdminLinkProviderForUserResponse' {} a -> s {httpStatus = a} :: AdminLinkProviderForUserResponse)

instance
  Prelude.NFData
    AdminLinkProviderForUserResponse
