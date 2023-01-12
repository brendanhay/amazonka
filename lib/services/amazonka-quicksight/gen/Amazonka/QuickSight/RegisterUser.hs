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
-- Module      : Amazonka.QuickSight.RegisterUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon QuickSight user, whose identity is associated with the
-- Identity and Access Management (IAM) identity or role specified in the
-- request.
module Amazonka.QuickSight.RegisterUser
  ( -- * Creating a Request
    RegisterUser (..),
    newRegisterUser,

    -- * Request Lenses
    registerUser_customFederationProviderUrl,
    registerUser_customPermissionsName,
    registerUser_externalLoginFederationProviderType,
    registerUser_externalLoginId,
    registerUser_iamArn,
    registerUser_sessionName,
    registerUser_userName,
    registerUser_identityType,
    registerUser_email,
    registerUser_userRole,
    registerUser_awsAccountId,
    registerUser_namespace,

    -- * Destructuring the Response
    RegisterUserResponse (..),
    newRegisterUserResponse,

    -- * Response Lenses
    registerUserResponse_requestId,
    registerUserResponse_user,
    registerUserResponse_userInvitationUrl,
    registerUserResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterUser' smart constructor.
data RegisterUser = RegisterUser'
  { -- | The URL of the custom OpenID Connect (OIDC) provider that provides
    -- identity to let a user federate into Amazon QuickSight with an
    -- associated Identity and Access Management(IAM) role. This parameter
    -- should only be used when @ExternalLoginFederationProviderType@ parameter
    -- is set to @CUSTOM_OIDC@.
    customFederationProviderUrl :: Prelude.Maybe Prelude.Text,
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
    -- To add custom permissions to an existing user, use @ UpdateUser @
    -- instead.
    --
    -- A set of custom permissions includes any combination of these
    -- restrictions. Currently, you need to create the profile names for custom
    -- permission sets by using the Amazon QuickSight console. Then, you use
    -- the @RegisterUser@ API operation to assign the named set of permissions
    -- to a Amazon QuickSight user.
    --
    -- Amazon QuickSight custom permissions are applied through IAM policies.
    -- Therefore, they override the permissions typically granted by assigning
    -- Amazon QuickSight users to one of the default security cohorts in Amazon
    -- QuickSight (admin, author, reader).
    --
    -- This feature is available only to Amazon QuickSight Enterprise edition
    -- subscriptions.
    customPermissionsName :: Prelude.Maybe Prelude.Text,
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
    externalLoginFederationProviderType :: Prelude.Maybe Prelude.Text,
    -- | The identity ID for a user in the external login provider.
    externalLoginId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM user or role that you are registering with Amazon
    -- QuickSight.
    iamArn :: Prelude.Maybe Prelude.Text,
    -- | You need to use this parameter only when you register one or more users
    -- using an assumed IAM role. You don\'t need to provide the session name
    -- for other scenarios, for example when you are registering an IAM user or
    -- an Amazon QuickSight user. You can register multiple users using the
    -- same IAM role if each user has a different session name. For more
    -- information on assuming IAM roles, see
    -- <https://docs.aws.amazon.com/cli/latest/reference/sts/assume-role.html assume-role>
    -- in the /CLI Reference./
    sessionName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight user name that you want to create for the user you
    -- are registering.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Amazon QuickSight supports several ways of managing the identity of
    -- users. This parameter accepts two values:
    --
    -- -   @IAM@: A user whose identity maps to an existing IAM user or role.
    --
    -- -   @QUICKSIGHT@: A user whose identity is owned and managed internally
    --     by Amazon QuickSight.
    identityType :: IdentityType,
    -- | The email address of the user that you want to register.
    email :: Prelude.Text,
    -- | The Amazon QuickSight role for the user. The user role can be one of the
    -- following:
    --
    -- -   @READER@: A user who has read-only access to dashboards.
    --
    -- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
    --     and dashboards.
    --
    -- -   @ADMIN@: A user who is an author, who can also manage Amazon
    --     QuickSight settings.
    --
    -- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
    --
    -- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
    userRole :: UserRole,
    -- | The ID for the Amazon Web Services account that the user is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace. Currently, you should set this to @default@.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customFederationProviderUrl', 'registerUser_customFederationProviderUrl' - The URL of the custom OpenID Connect (OIDC) provider that provides
-- identity to let a user federate into Amazon QuickSight with an
-- associated Identity and Access Management(IAM) role. This parameter
-- should only be used when @ExternalLoginFederationProviderType@ parameter
-- is set to @CUSTOM_OIDC@.
--
-- 'customPermissionsName', 'registerUser_customPermissionsName' - (Enterprise edition only) The name of the custom permissions profile
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
-- To add custom permissions to an existing user, use @ UpdateUser @
-- instead.
--
-- A set of custom permissions includes any combination of these
-- restrictions. Currently, you need to create the profile names for custom
-- permission sets by using the Amazon QuickSight console. Then, you use
-- the @RegisterUser@ API operation to assign the named set of permissions
-- to a Amazon QuickSight user.
--
-- Amazon QuickSight custom permissions are applied through IAM policies.
-- Therefore, they override the permissions typically granted by assigning
-- Amazon QuickSight users to one of the default security cohorts in Amazon
-- QuickSight (admin, author, reader).
--
-- This feature is available only to Amazon QuickSight Enterprise edition
-- subscriptions.
--
-- 'externalLoginFederationProviderType', 'registerUser_externalLoginFederationProviderType' - The type of supported external login provider that provides identity to
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
-- 'externalLoginId', 'registerUser_externalLoginId' - The identity ID for a user in the external login provider.
--
-- 'iamArn', 'registerUser_iamArn' - The ARN of the IAM user or role that you are registering with Amazon
-- QuickSight.
--
-- 'sessionName', 'registerUser_sessionName' - You need to use this parameter only when you register one or more users
-- using an assumed IAM role. You don\'t need to provide the session name
-- for other scenarios, for example when you are registering an IAM user or
-- an Amazon QuickSight user. You can register multiple users using the
-- same IAM role if each user has a different session name. For more
-- information on assuming IAM roles, see
-- <https://docs.aws.amazon.com/cli/latest/reference/sts/assume-role.html assume-role>
-- in the /CLI Reference./
--
-- 'userName', 'registerUser_userName' - The Amazon QuickSight user name that you want to create for the user you
-- are registering.
--
-- 'identityType', 'registerUser_identityType' - Amazon QuickSight supports several ways of managing the identity of
-- users. This parameter accepts two values:
--
-- -   @IAM@: A user whose identity maps to an existing IAM user or role.
--
-- -   @QUICKSIGHT@: A user whose identity is owned and managed internally
--     by Amazon QuickSight.
--
-- 'email', 'registerUser_email' - The email address of the user that you want to register.
--
-- 'userRole', 'registerUser_userRole' - The Amazon QuickSight role for the user. The user role can be one of the
-- following:
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon
--     QuickSight settings.
--
-- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
--
-- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
--
-- 'awsAccountId', 'registerUser_awsAccountId' - The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'registerUser_namespace' - The namespace. Currently, you should set this to @default@.
newRegisterUser ::
  -- | 'identityType'
  IdentityType ->
  -- | 'email'
  Prelude.Text ->
  -- | 'userRole'
  UserRole ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  RegisterUser
newRegisterUser
  pIdentityType_
  pEmail_
  pUserRole_
  pAwsAccountId_
  pNamespace_ =
    RegisterUser'
      { customFederationProviderUrl =
          Prelude.Nothing,
        customPermissionsName = Prelude.Nothing,
        externalLoginFederationProviderType =
          Prelude.Nothing,
        externalLoginId = Prelude.Nothing,
        iamArn = Prelude.Nothing,
        sessionName = Prelude.Nothing,
        userName = Prelude.Nothing,
        identityType = pIdentityType_,
        email = pEmail_,
        userRole = pUserRole_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The URL of the custom OpenID Connect (OIDC) provider that provides
-- identity to let a user federate into Amazon QuickSight with an
-- associated Identity and Access Management(IAM) role. This parameter
-- should only be used when @ExternalLoginFederationProviderType@ parameter
-- is set to @CUSTOM_OIDC@.
registerUser_customFederationProviderUrl :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_customFederationProviderUrl = Lens.lens (\RegisterUser' {customFederationProviderUrl} -> customFederationProviderUrl) (\s@RegisterUser' {} a -> s {customFederationProviderUrl = a} :: RegisterUser)

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
-- To add custom permissions to an existing user, use @ UpdateUser @
-- instead.
--
-- A set of custom permissions includes any combination of these
-- restrictions. Currently, you need to create the profile names for custom
-- permission sets by using the Amazon QuickSight console. Then, you use
-- the @RegisterUser@ API operation to assign the named set of permissions
-- to a Amazon QuickSight user.
--
-- Amazon QuickSight custom permissions are applied through IAM policies.
-- Therefore, they override the permissions typically granted by assigning
-- Amazon QuickSight users to one of the default security cohorts in Amazon
-- QuickSight (admin, author, reader).
--
-- This feature is available only to Amazon QuickSight Enterprise edition
-- subscriptions.
registerUser_customPermissionsName :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_customPermissionsName = Lens.lens (\RegisterUser' {customPermissionsName} -> customPermissionsName) (\s@RegisterUser' {} a -> s {customPermissionsName = a} :: RegisterUser)

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
registerUser_externalLoginFederationProviderType :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_externalLoginFederationProviderType = Lens.lens (\RegisterUser' {externalLoginFederationProviderType} -> externalLoginFederationProviderType) (\s@RegisterUser' {} a -> s {externalLoginFederationProviderType = a} :: RegisterUser)

-- | The identity ID for a user in the external login provider.
registerUser_externalLoginId :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_externalLoginId = Lens.lens (\RegisterUser' {externalLoginId} -> externalLoginId) (\s@RegisterUser' {} a -> s {externalLoginId = a} :: RegisterUser)

-- | The ARN of the IAM user or role that you are registering with Amazon
-- QuickSight.
registerUser_iamArn :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_iamArn = Lens.lens (\RegisterUser' {iamArn} -> iamArn) (\s@RegisterUser' {} a -> s {iamArn = a} :: RegisterUser)

-- | You need to use this parameter only when you register one or more users
-- using an assumed IAM role. You don\'t need to provide the session name
-- for other scenarios, for example when you are registering an IAM user or
-- an Amazon QuickSight user. You can register multiple users using the
-- same IAM role if each user has a different session name. For more
-- information on assuming IAM roles, see
-- <https://docs.aws.amazon.com/cli/latest/reference/sts/assume-role.html assume-role>
-- in the /CLI Reference./
registerUser_sessionName :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_sessionName = Lens.lens (\RegisterUser' {sessionName} -> sessionName) (\s@RegisterUser' {} a -> s {sessionName = a} :: RegisterUser)

-- | The Amazon QuickSight user name that you want to create for the user you
-- are registering.
registerUser_userName :: Lens.Lens' RegisterUser (Prelude.Maybe Prelude.Text)
registerUser_userName = Lens.lens (\RegisterUser' {userName} -> userName) (\s@RegisterUser' {} a -> s {userName = a} :: RegisterUser)

-- | Amazon QuickSight supports several ways of managing the identity of
-- users. This parameter accepts two values:
--
-- -   @IAM@: A user whose identity maps to an existing IAM user or role.
--
-- -   @QUICKSIGHT@: A user whose identity is owned and managed internally
--     by Amazon QuickSight.
registerUser_identityType :: Lens.Lens' RegisterUser IdentityType
registerUser_identityType = Lens.lens (\RegisterUser' {identityType} -> identityType) (\s@RegisterUser' {} a -> s {identityType = a} :: RegisterUser)

-- | The email address of the user that you want to register.
registerUser_email :: Lens.Lens' RegisterUser Prelude.Text
registerUser_email = Lens.lens (\RegisterUser' {email} -> email) (\s@RegisterUser' {} a -> s {email = a} :: RegisterUser)

-- | The Amazon QuickSight role for the user. The user role can be one of the
-- following:
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon
--     QuickSight settings.
--
-- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
--
-- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
registerUser_userRole :: Lens.Lens' RegisterUser UserRole
registerUser_userRole = Lens.lens (\RegisterUser' {userRole} -> userRole) (\s@RegisterUser' {} a -> s {userRole = a} :: RegisterUser)

-- | The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
registerUser_awsAccountId :: Lens.Lens' RegisterUser Prelude.Text
registerUser_awsAccountId = Lens.lens (\RegisterUser' {awsAccountId} -> awsAccountId) (\s@RegisterUser' {} a -> s {awsAccountId = a} :: RegisterUser)

-- | The namespace. Currently, you should set this to @default@.
registerUser_namespace :: Lens.Lens' RegisterUser Prelude.Text
registerUser_namespace = Lens.lens (\RegisterUser' {namespace} -> namespace) (\s@RegisterUser' {} a -> s {namespace = a} :: RegisterUser)

instance Core.AWSRequest RegisterUser where
  type AWSResponse RegisterUser = RegisterUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterUserResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "User")
            Prelude.<*> (x Data..?> "UserInvitationUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterUser where
  hashWithSalt _salt RegisterUser' {..} =
    _salt
      `Prelude.hashWithSalt` customFederationProviderUrl
      `Prelude.hashWithSalt` customPermissionsName
      `Prelude.hashWithSalt` externalLoginFederationProviderType
      `Prelude.hashWithSalt` externalLoginId
      `Prelude.hashWithSalt` iamArn
      `Prelude.hashWithSalt` sessionName
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` userRole
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData RegisterUser where
  rnf RegisterUser' {..} =
    Prelude.rnf customFederationProviderUrl
      `Prelude.seq` Prelude.rnf customPermissionsName
      `Prelude.seq` Prelude.rnf externalLoginFederationProviderType
      `Prelude.seq` Prelude.rnf externalLoginId
      `Prelude.seq` Prelude.rnf iamArn
      `Prelude.seq` Prelude.rnf sessionName
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf userRole
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders RegisterUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterUser where
  toJSON RegisterUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomFederationProviderUrl" Data..=)
              Prelude.<$> customFederationProviderUrl,
            ("CustomPermissionsName" Data..=)
              Prelude.<$> customPermissionsName,
            ("ExternalLoginFederationProviderType" Data..=)
              Prelude.<$> externalLoginFederationProviderType,
            ("ExternalLoginId" Data..=)
              Prelude.<$> externalLoginId,
            ("IamArn" Data..=) Prelude.<$> iamArn,
            ("SessionName" Data..=) Prelude.<$> sessionName,
            ("UserName" Data..=) Prelude.<$> userName,
            Prelude.Just ("IdentityType" Data..= identityType),
            Prelude.Just ("Email" Data..= email),
            Prelude.Just ("UserRole" Data..= userRole)
          ]
      )

instance Data.ToPath RegisterUser where
  toPath RegisterUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/users"
      ]

instance Data.ToQuery RegisterUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterUserResponse' smart constructor.
data RegisterUserResponse = RegisterUserResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The user\'s user name.
    user :: Prelude.Maybe User,
    -- | The URL the user visits to complete registration and provide a password.
    -- This is returned only for users with an identity type of @QUICKSIGHT@.
    userInvitationUrl :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'registerUserResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'user', 'registerUserResponse_user' - The user\'s user name.
--
-- 'userInvitationUrl', 'registerUserResponse_userInvitationUrl' - The URL the user visits to complete registration and provide a password.
-- This is returned only for users with an identity type of @QUICKSIGHT@.
--
-- 'status', 'registerUserResponse_status' - The HTTP status of the request.
newRegisterUserResponse ::
  -- | 'status'
  Prelude.Int ->
  RegisterUserResponse
newRegisterUserResponse pStatus_ =
  RegisterUserResponse'
    { requestId = Prelude.Nothing,
      user = Prelude.Nothing,
      userInvitationUrl = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
registerUserResponse_requestId :: Lens.Lens' RegisterUserResponse (Prelude.Maybe Prelude.Text)
registerUserResponse_requestId = Lens.lens (\RegisterUserResponse' {requestId} -> requestId) (\s@RegisterUserResponse' {} a -> s {requestId = a} :: RegisterUserResponse)

-- | The user\'s user name.
registerUserResponse_user :: Lens.Lens' RegisterUserResponse (Prelude.Maybe User)
registerUserResponse_user = Lens.lens (\RegisterUserResponse' {user} -> user) (\s@RegisterUserResponse' {} a -> s {user = a} :: RegisterUserResponse)

-- | The URL the user visits to complete registration and provide a password.
-- This is returned only for users with an identity type of @QUICKSIGHT@.
registerUserResponse_userInvitationUrl :: Lens.Lens' RegisterUserResponse (Prelude.Maybe Prelude.Text)
registerUserResponse_userInvitationUrl = Lens.lens (\RegisterUserResponse' {userInvitationUrl} -> userInvitationUrl) (\s@RegisterUserResponse' {} a -> s {userInvitationUrl = a} :: RegisterUserResponse)

-- | The HTTP status of the request.
registerUserResponse_status :: Lens.Lens' RegisterUserResponse Prelude.Int
registerUserResponse_status = Lens.lens (\RegisterUserResponse' {status} -> status) (\s@RegisterUserResponse' {} a -> s {status = a} :: RegisterUserResponse)

instance Prelude.NFData RegisterUserResponse where
  rnf RegisterUserResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf userInvitationUrl
      `Prelude.seq` Prelude.rnf status
