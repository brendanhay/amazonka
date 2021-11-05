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
-- Module      : Network.AWS.QuickSight.UpdateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon QuickSight user.
module Network.AWS.QuickSight.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_unapplyCustomPermissions,
    updateUser_customPermissionsName,
    updateUser_customFederationProviderUrl,
    updateUser_externalLoginFederationProviderType,
    updateUser_externalLoginId,
    updateUser_userName,
    updateUser_awsAccountId,
    updateUser_namespace,
    updateUser_email,
    updateUser_role,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_requestId,
    updateUserResponse_user,
    updateUserResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | A flag that you use to indicate that you want to remove all custom
    -- permissions from this user. Using this parameter resets the user to the
    -- state it was in before a custom permissions profile was applied. This
    -- parameter defaults to NULL and it doesn\'t accept any other value.
    unapplyCustomPermissions :: Prelude.Maybe Prelude.Bool,
    -- | (Enterprise edition only) The name of the custom permissions profile
    -- that you want to assign to this user. Customized permissions allows you
    -- to control a user\'s access by restricting access the following
    -- operations:
    --
    -- -   Create and update data sources
    --
    -- -   Create and update datasets
    --
    -- -   Create and update email reports
    --
    -- -   Subscribe to email reports
    --
    -- A set of custom permissions includes any combination of these
    -- restrictions. Currently, you need to create the profile names for custom
    -- permission sets by using the Amazon QuickSight console. Then, you use
    -- the @RegisterUser@ API operation to assign the named set of permissions
    -- to a Amazon QuickSight user.
    --
    -- Amazon QuickSight custom permissions are applied through IAMpolicies.
    -- Therefore, they override the permissions typically granted by assigning
    -- Amazon QuickSight users to one of the default security cohorts in Amazon
    -- QuickSight (admin, author, reader).
    --
    -- This feature is available only to Amazon QuickSight Enterprise edition
    -- subscriptions.
    customPermissionsName :: Prelude.Maybe Prelude.Text,
    -- | The URL of the custom OpenID Connect (OIDC) provider that provides
    -- identity to let a user federate into Amazon QuickSight with an
    -- associated Identity and Access Management(IAM) role. This parameter
    -- should only be used when @ExternalLoginFederationProviderType@ parameter
    -- is set to @CUSTOM_OIDC@.
    customFederationProviderUrl :: Prelude.Maybe Prelude.Text,
    -- | The type of supported external login provider that provides identity to
    -- let a user federate into Amazon QuickSight with an associated Identity
    -- and Access Management(IAM) role. The type of supported external login
    -- provider can be one of the following.
    --
    -- -   @COGNITO@: Amazon Cognito. The provider URL is
    --     cognito-identity.amazonaws.com. When choosing the @COGNITO@ provider
    --     type, don’t use the \"CustomFederationProviderUrl\" parameter which
    --     is only needed when the external provider is custom.
    --
    -- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider. When choosing
    --     @CUSTOM_OIDC@ type, use the @CustomFederationProviderUrl@ parameter
    --     to provide the custom OIDC provider URL.
    --
    -- -   @NONE@: This clears all the previously saved external login
    --     information for a user. Use @ DescribeUser @ API to check the
    --     external login information.
    externalLoginFederationProviderType :: Prelude.Maybe Prelude.Text,
    -- | The identity ID for a user in the external login provider.
    externalLoginId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight user name that you want to update.
    userName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the user is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace. Currently, you should set this to @default@.
    namespace :: Prelude.Text,
    -- | The email address of the user that you want to update.
    email :: Prelude.Text,
    -- | The Amazon QuickSight role of the user. The role can be one of the
    -- following default security cohorts:
    --
    -- -   @READER@: A user who has read-only access to dashboards.
    --
    -- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
    --     and dashboards.
    --
    -- -   @ADMIN@: A user who is an author, who can also manage Amazon
    --     QuickSight settings.
    --
    -- The name of the Amazon QuickSight role is invisible to the user except
    -- for the console screens dealing with permissions.
    role' :: UserRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unapplyCustomPermissions', 'updateUser_unapplyCustomPermissions' - A flag that you use to indicate that you want to remove all custom
-- permissions from this user. Using this parameter resets the user to the
-- state it was in before a custom permissions profile was applied. This
-- parameter defaults to NULL and it doesn\'t accept any other value.
--
-- 'customPermissionsName', 'updateUser_customPermissionsName' - (Enterprise edition only) The name of the custom permissions profile
-- that you want to assign to this user. Customized permissions allows you
-- to control a user\'s access by restricting access the following
-- operations:
--
-- -   Create and update data sources
--
-- -   Create and update datasets
--
-- -   Create and update email reports
--
-- -   Subscribe to email reports
--
-- A set of custom permissions includes any combination of these
-- restrictions. Currently, you need to create the profile names for custom
-- permission sets by using the Amazon QuickSight console. Then, you use
-- the @RegisterUser@ API operation to assign the named set of permissions
-- to a Amazon QuickSight user.
--
-- Amazon QuickSight custom permissions are applied through IAMpolicies.
-- Therefore, they override the permissions typically granted by assigning
-- Amazon QuickSight users to one of the default security cohorts in Amazon
-- QuickSight (admin, author, reader).
--
-- This feature is available only to Amazon QuickSight Enterprise edition
-- subscriptions.
--
-- 'customFederationProviderUrl', 'updateUser_customFederationProviderUrl' - The URL of the custom OpenID Connect (OIDC) provider that provides
-- identity to let a user federate into Amazon QuickSight with an
-- associated Identity and Access Management(IAM) role. This parameter
-- should only be used when @ExternalLoginFederationProviderType@ parameter
-- is set to @CUSTOM_OIDC@.
--
-- 'externalLoginFederationProviderType', 'updateUser_externalLoginFederationProviderType' - The type of supported external login provider that provides identity to
-- let a user federate into Amazon QuickSight with an associated Identity
-- and Access Management(IAM) role. The type of supported external login
-- provider can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com. When choosing the @COGNITO@ provider
--     type, don’t use the \"CustomFederationProviderUrl\" parameter which
--     is only needed when the external provider is custom.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider. When choosing
--     @CUSTOM_OIDC@ type, use the @CustomFederationProviderUrl@ parameter
--     to provide the custom OIDC provider URL.
--
-- -   @NONE@: This clears all the previously saved external login
--     information for a user. Use @ DescribeUser @ API to check the
--     external login information.
--
-- 'externalLoginId', 'updateUser_externalLoginId' - The identity ID for a user in the external login provider.
--
-- 'userName', 'updateUser_userName' - The Amazon QuickSight user name that you want to update.
--
-- 'awsAccountId', 'updateUser_awsAccountId' - The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'updateUser_namespace' - The namespace. Currently, you should set this to @default@.
--
-- 'email', 'updateUser_email' - The email address of the user that you want to update.
--
-- 'role'', 'updateUser_role' - The Amazon QuickSight role of the user. The role can be one of the
-- following default security cohorts:
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon
--     QuickSight settings.
--
-- The name of the Amazon QuickSight role is invisible to the user except
-- for the console screens dealing with permissions.
newUpdateUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'email'
  Prelude.Text ->
  -- | 'role''
  UserRole ->
  UpdateUser
newUpdateUser
  pUserName_
  pAwsAccountId_
  pNamespace_
  pEmail_
  pRole_ =
    UpdateUser'
      { unapplyCustomPermissions =
          Prelude.Nothing,
        customPermissionsName = Prelude.Nothing,
        customFederationProviderUrl = Prelude.Nothing,
        externalLoginFederationProviderType =
          Prelude.Nothing,
        externalLoginId = Prelude.Nothing,
        userName = pUserName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_,
        email = pEmail_,
        role' = pRole_
      }

-- | A flag that you use to indicate that you want to remove all custom
-- permissions from this user. Using this parameter resets the user to the
-- state it was in before a custom permissions profile was applied. This
-- parameter defaults to NULL and it doesn\'t accept any other value.
updateUser_unapplyCustomPermissions :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Bool)
updateUser_unapplyCustomPermissions = Lens.lens (\UpdateUser' {unapplyCustomPermissions} -> unapplyCustomPermissions) (\s@UpdateUser' {} a -> s {unapplyCustomPermissions = a} :: UpdateUser)

-- | (Enterprise edition only) The name of the custom permissions profile
-- that you want to assign to this user. Customized permissions allows you
-- to control a user\'s access by restricting access the following
-- operations:
--
-- -   Create and update data sources
--
-- -   Create and update datasets
--
-- -   Create and update email reports
--
-- -   Subscribe to email reports
--
-- A set of custom permissions includes any combination of these
-- restrictions. Currently, you need to create the profile names for custom
-- permission sets by using the Amazon QuickSight console. Then, you use
-- the @RegisterUser@ API operation to assign the named set of permissions
-- to a Amazon QuickSight user.
--
-- Amazon QuickSight custom permissions are applied through IAMpolicies.
-- Therefore, they override the permissions typically granted by assigning
-- Amazon QuickSight users to one of the default security cohorts in Amazon
-- QuickSight (admin, author, reader).
--
-- This feature is available only to Amazon QuickSight Enterprise edition
-- subscriptions.
updateUser_customPermissionsName :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_customPermissionsName = Lens.lens (\UpdateUser' {customPermissionsName} -> customPermissionsName) (\s@UpdateUser' {} a -> s {customPermissionsName = a} :: UpdateUser)

-- | The URL of the custom OpenID Connect (OIDC) provider that provides
-- identity to let a user federate into Amazon QuickSight with an
-- associated Identity and Access Management(IAM) role. This parameter
-- should only be used when @ExternalLoginFederationProviderType@ parameter
-- is set to @CUSTOM_OIDC@.
updateUser_customFederationProviderUrl :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_customFederationProviderUrl = Lens.lens (\UpdateUser' {customFederationProviderUrl} -> customFederationProviderUrl) (\s@UpdateUser' {} a -> s {customFederationProviderUrl = a} :: UpdateUser)

-- | The type of supported external login provider that provides identity to
-- let a user federate into Amazon QuickSight with an associated Identity
-- and Access Management(IAM) role. The type of supported external login
-- provider can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com. When choosing the @COGNITO@ provider
--     type, don’t use the \"CustomFederationProviderUrl\" parameter which
--     is only needed when the external provider is custom.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider. When choosing
--     @CUSTOM_OIDC@ type, use the @CustomFederationProviderUrl@ parameter
--     to provide the custom OIDC provider URL.
--
-- -   @NONE@: This clears all the previously saved external login
--     information for a user. Use @ DescribeUser @ API to check the
--     external login information.
updateUser_externalLoginFederationProviderType :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_externalLoginFederationProviderType = Lens.lens (\UpdateUser' {externalLoginFederationProviderType} -> externalLoginFederationProviderType) (\s@UpdateUser' {} a -> s {externalLoginFederationProviderType = a} :: UpdateUser)

-- | The identity ID for a user in the external login provider.
updateUser_externalLoginId :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_externalLoginId = Lens.lens (\UpdateUser' {externalLoginId} -> externalLoginId) (\s@UpdateUser' {} a -> s {externalLoginId = a} :: UpdateUser)

-- | The Amazon QuickSight user name that you want to update.
updateUser_userName :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userName = Lens.lens (\UpdateUser' {userName} -> userName) (\s@UpdateUser' {} a -> s {userName = a} :: UpdateUser)

-- | The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
updateUser_awsAccountId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_awsAccountId = Lens.lens (\UpdateUser' {awsAccountId} -> awsAccountId) (\s@UpdateUser' {} a -> s {awsAccountId = a} :: UpdateUser)

-- | The namespace. Currently, you should set this to @default@.
updateUser_namespace :: Lens.Lens' UpdateUser Prelude.Text
updateUser_namespace = Lens.lens (\UpdateUser' {namespace} -> namespace) (\s@UpdateUser' {} a -> s {namespace = a} :: UpdateUser)

-- | The email address of the user that you want to update.
updateUser_email :: Lens.Lens' UpdateUser Prelude.Text
updateUser_email = Lens.lens (\UpdateUser' {email} -> email) (\s@UpdateUser' {} a -> s {email = a} :: UpdateUser)

-- | The Amazon QuickSight role of the user. The role can be one of the
-- following default security cohorts:
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon
--     QuickSight settings.
--
-- The name of the Amazon QuickSight role is invisible to the user except
-- for the console screens dealing with permissions.
updateUser_role :: Lens.Lens' UpdateUser UserRole
updateUser_role = Lens.lens (\UpdateUser' {role'} -> role') (\s@UpdateUser' {} a -> s {role' = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser

instance Prelude.NFData UpdateUser

instance Core.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UnapplyCustomPermissions" Core..=)
              Prelude.<$> unapplyCustomPermissions,
            ("CustomPermissionsName" Core..=)
              Prelude.<$> customPermissionsName,
            ("CustomFederationProviderUrl" Core..=)
              Prelude.<$> customFederationProviderUrl,
            ("ExternalLoginFederationProviderType" Core..=)
              Prelude.<$> externalLoginFederationProviderType,
            ("ExternalLoginId" Core..=)
              Prelude.<$> externalLoginId,
            Prelude.Just ("Email" Core..= email),
            Prelude.Just ("Role" Core..= role')
          ]
      )

instance Core.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/namespaces/",
        Core.toBS namespace,
        "/users/",
        Core.toBS userName
      ]

instance Core.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight user.
    user :: Prelude.Maybe User,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateUserResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'user', 'updateUserResponse_user' - The Amazon QuickSight user.
--
-- 'status', 'updateUserResponse_status' - The HTTP status of the request.
newUpdateUserResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateUserResponse
newUpdateUserResponse pStatus_ =
  UpdateUserResponse'
    { requestId = Prelude.Nothing,
      user = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateUserResponse_requestId :: Lens.Lens' UpdateUserResponse (Prelude.Maybe Prelude.Text)
updateUserResponse_requestId = Lens.lens (\UpdateUserResponse' {requestId} -> requestId) (\s@UpdateUserResponse' {} a -> s {requestId = a} :: UpdateUserResponse)

-- | The Amazon QuickSight user.
updateUserResponse_user :: Lens.Lens' UpdateUserResponse (Prelude.Maybe User)
updateUserResponse_user = Lens.lens (\UpdateUserResponse' {user} -> user) (\s@UpdateUserResponse' {} a -> s {user = a} :: UpdateUserResponse)

-- | The HTTP status of the request.
updateUserResponse_status :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_status = Lens.lens (\UpdateUserResponse' {status} -> status) (\s@UpdateUserResponse' {} a -> s {status = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse
