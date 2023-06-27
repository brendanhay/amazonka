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
-- Module      : Amazonka.CognitoIdentityProvider.AdminLinkProviderForUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an existing user account in a user pool (@DestinationUser@) to an
-- identity from an external IdP (@SourceUser@) based on a specified
-- attribute name and value from the external IdP. This allows you to
-- create a link from the existing user account to an external federated
-- user identity that has not yet been used to sign in. You can then use
-- the federated user identity to sign in as the existing user account.
--
-- For example, if there is an existing user with a username and password,
-- this API links that user to a federated user identity. When the user
-- signs in with a federated user identity, they sign in as the existing
-- user account.
--
-- The maximum number of federated identities linked to a user is five.
--
-- Because this API allows a user with an external federated identity to
-- sign in as an existing user in the user pool, it is critical that it
-- only be used with external IdPs and provider attributes that have been
-- trusted by the application owner.
--
-- This action is administrative and requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminLinkProviderForUser
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAdminLinkProviderForUser' smart constructor.
data AdminLinkProviderForUser = AdminLinkProviderForUser'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The existing user in the user pool that you want to assign to the
    -- external IdP user account. This user can be a native (Username +
    -- Password) Amazon Cognito user pools user or a federated user (for
    -- example, a SAML or Facebook user). If the user doesn\'t exist, Amazon
    -- Cognito generates an exception. Amazon Cognito returns this user when
    -- the new user (with the linked IdP attribute) signs in.
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
    -- All attributes in the DestinationUser profile must be mutable. If you
    -- have assigned the user any immutable custom attributes, the operation
    -- won\'t succeed.
    destinationUser :: ProviderUserIdentifierType,
    -- | An external IdP account for a user who doesn\'t exist yet in the user
    -- pool. This user must be a federated user (for example, a SAML or
    -- Facebook user), not another native user.
    --
    -- If the @SourceUser@ is using a federated social IdP, such as Facebook,
    -- Google, or Login with Amazon, you must set the @ProviderAttributeName@
    -- to @Cognito_Subject@. For social IdPs, the @ProviderName@ will be
    -- @Facebook@, @Google@, or @LoginWithAmazon@, and Amazon Cognito will
    -- automatically parse the Facebook, Google, and Login with Amazon tokens
    -- for @id@, @sub@, and @user_id@, respectively. The
    -- @ProviderAttributeValue@ for the user must be the same value as the
    -- @id@, @sub@, or @user_id@ value found in the social IdP token.
    --
    -- For SAML, the @ProviderAttributeName@ can be any value that matches a
    -- claim in the SAML assertion. If you want to link SAML users based on the
    -- subject of the SAML assertion, you should map the subject to a claim
    -- through the SAML IdP and submit that claim name as the
    -- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
    -- @Cognito_Subject@, Amazon Cognito will automatically parse the default
    -- unique identifier found in the subject from the SAML token.
    sourceUser :: ProviderUserIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'destinationUser', 'adminLinkProviderForUser_destinationUser' - The existing user in the user pool that you want to assign to the
-- external IdP user account. This user can be a native (Username +
-- Password) Amazon Cognito user pools user or a federated user (for
-- example, a SAML or Facebook user). If the user doesn\'t exist, Amazon
-- Cognito generates an exception. Amazon Cognito returns this user when
-- the new user (with the linked IdP attribute) signs in.
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
-- All attributes in the DestinationUser profile must be mutable. If you
-- have assigned the user any immutable custom attributes, the operation
-- won\'t succeed.
--
-- 'sourceUser', 'adminLinkProviderForUser_sourceUser' - An external IdP account for a user who doesn\'t exist yet in the user
-- pool. This user must be a federated user (for example, a SAML or
-- Facebook user), not another native user.
--
-- If the @SourceUser@ is using a federated social IdP, such as Facebook,
-- Google, or Login with Amazon, you must set the @ProviderAttributeName@
-- to @Cognito_Subject@. For social IdPs, the @ProviderName@ will be
-- @Facebook@, @Google@, or @LoginWithAmazon@, and Amazon Cognito will
-- automatically parse the Facebook, Google, and Login with Amazon tokens
-- for @id@, @sub@, and @user_id@, respectively. The
-- @ProviderAttributeValue@ for the user must be the same value as the
-- @id@, @sub@, or @user_id@ value found in the social IdP token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a
-- claim in the SAML assertion. If you want to link SAML users based on the
-- subject of the SAML assertion, you should map the subject to a claim
-- through the SAML IdP and submit that claim name as the
-- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
-- @Cognito_Subject@, Amazon Cognito will automatically parse the default
-- unique identifier found in the subject from the SAML token.
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

-- | The existing user in the user pool that you want to assign to the
-- external IdP user account. This user can be a native (Username +
-- Password) Amazon Cognito user pools user or a federated user (for
-- example, a SAML or Facebook user). If the user doesn\'t exist, Amazon
-- Cognito generates an exception. Amazon Cognito returns this user when
-- the new user (with the linked IdP attribute) signs in.
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
-- All attributes in the DestinationUser profile must be mutable. If you
-- have assigned the user any immutable custom attributes, the operation
-- won\'t succeed.
adminLinkProviderForUser_destinationUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
adminLinkProviderForUser_destinationUser = Lens.lens (\AdminLinkProviderForUser' {destinationUser} -> destinationUser) (\s@AdminLinkProviderForUser' {} a -> s {destinationUser = a} :: AdminLinkProviderForUser)

-- | An external IdP account for a user who doesn\'t exist yet in the user
-- pool. This user must be a federated user (for example, a SAML or
-- Facebook user), not another native user.
--
-- If the @SourceUser@ is using a federated social IdP, such as Facebook,
-- Google, or Login with Amazon, you must set the @ProviderAttributeName@
-- to @Cognito_Subject@. For social IdPs, the @ProviderName@ will be
-- @Facebook@, @Google@, or @LoginWithAmazon@, and Amazon Cognito will
-- automatically parse the Facebook, Google, and Login with Amazon tokens
-- for @id@, @sub@, and @user_id@, respectively. The
-- @ProviderAttributeValue@ for the user must be the same value as the
-- @id@, @sub@, or @user_id@ value found in the social IdP token.
--
-- For SAML, the @ProviderAttributeName@ can be any value that matches a
-- claim in the SAML assertion. If you want to link SAML users based on the
-- subject of the SAML assertion, you should map the subject to a claim
-- through the SAML IdP and submit that claim name as the
-- @ProviderAttributeName@. If you set @ProviderAttributeName@ to
-- @Cognito_Subject@, Amazon Cognito will automatically parse the default
-- unique identifier found in the subject from the SAML token.
adminLinkProviderForUser_sourceUser :: Lens.Lens' AdminLinkProviderForUser ProviderUserIdentifierType
adminLinkProviderForUser_sourceUser = Lens.lens (\AdminLinkProviderForUser' {sourceUser} -> sourceUser) (\s@AdminLinkProviderForUser' {} a -> s {sourceUser = a} :: AdminLinkProviderForUser)

instance Core.AWSRequest AdminLinkProviderForUser where
  type
    AWSResponse AdminLinkProviderForUser =
      AdminLinkProviderForUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminLinkProviderForUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminLinkProviderForUser where
  hashWithSalt _salt AdminLinkProviderForUser' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` destinationUser
      `Prelude.hashWithSalt` sourceUser

instance Prelude.NFData AdminLinkProviderForUser where
  rnf AdminLinkProviderForUser' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf destinationUser
      `Prelude.seq` Prelude.rnf sourceUser

instance Data.ToHeaders AdminLinkProviderForUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminLinkProviderForUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminLinkProviderForUser where
  toJSON AdminLinkProviderForUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just
              ("DestinationUser" Data..= destinationUser),
            Prelude.Just ("SourceUser" Data..= sourceUser)
          ]
      )

instance Data.ToPath AdminLinkProviderForUser where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminLinkProviderForUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminLinkProviderForUserResponse' smart constructor.
data AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AdminLinkProviderForUserResponse' {..} =
    Prelude.rnf httpStatus
