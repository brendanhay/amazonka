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
-- Module      : Network.AWS.Connect.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user account for the specified Amazon Connect instance.
--
-- For information about how to create user accounts using the Amazon
-- Connect console, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/user-management.html Add Users>
-- in the /Amazon Connect Administrator Guide/.
module Network.AWS.Connect.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_identityInfo,
    createUser_hierarchyGroupId,
    createUser_directoryUserId,
    createUser_password,
    createUser_tags,
    createUser_username,
    createUser_phoneConfig,
    createUser_securityProfileIds,
    createUser_routingProfileId,
    createUser_instanceId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_userArn,
    createUserResponse_userId,
    createUserResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The information about the identity of the user.
    identityInfo :: Core.Maybe UserIdentityInfo,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Core.Maybe Core.Text,
    -- | The identifier of the user account in the directory used for identity
    -- management. If Amazon Connect cannot access the directory, you can
    -- specify this identifier to authenticate users. If you include the
    -- identifier, we assume that Amazon Connect cannot access the directory.
    -- Otherwise, the identity information is used to authenticate users from
    -- your directory.
    --
    -- This parameter is required if you are using an existing directory for
    -- identity management in Amazon Connect when Amazon Connect cannot access
    -- your directory to authenticate users. If you are using SAML for identity
    -- management and include this parameter, an error is returned.
    directoryUserId :: Core.Maybe Core.Text,
    -- | The password for the user account. A password is required if you are
    -- using Amazon Connect for identity management. Otherwise, it is an error
    -- to include a password.
    password :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The user name for the account. For instances not using SAML for identity
    -- management, the user name can include up to 20 characters. If you are
    -- using SAML for identity management, the user name can include up to 64
    -- characters from [a-zA-Z0-9_-.\\\@]+.
    username :: Core.Text,
    -- | The phone settings for the user.
    phoneConfig :: UserPhoneConfig,
    -- | The identifier of the security profile for the user.
    securityProfileIds :: Core.NonEmpty Core.Text,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityInfo', 'createUser_identityInfo' - The information about the identity of the user.
--
-- 'hierarchyGroupId', 'createUser_hierarchyGroupId' - The identifier of the hierarchy group for the user.
--
-- 'directoryUserId', 'createUser_directoryUserId' - The identifier of the user account in the directory used for identity
-- management. If Amazon Connect cannot access the directory, you can
-- specify this identifier to authenticate users. If you include the
-- identifier, we assume that Amazon Connect cannot access the directory.
-- Otherwise, the identity information is used to authenticate users from
-- your directory.
--
-- This parameter is required if you are using an existing directory for
-- identity management in Amazon Connect when Amazon Connect cannot access
-- your directory to authenticate users. If you are using SAML for identity
-- management and include this parameter, an error is returned.
--
-- 'password', 'createUser_password' - The password for the user account. A password is required if you are
-- using Amazon Connect for identity management. Otherwise, it is an error
-- to include a password.
--
-- 'tags', 'createUser_tags' - One or more tags.
--
-- 'username', 'createUser_username' - The user name for the account. For instances not using SAML for identity
-- management, the user name can include up to 20 characters. If you are
-- using SAML for identity management, the user name can include up to 64
-- characters from [a-zA-Z0-9_-.\\\@]+.
--
-- 'phoneConfig', 'createUser_phoneConfig' - The phone settings for the user.
--
-- 'securityProfileIds', 'createUser_securityProfileIds' - The identifier of the security profile for the user.
--
-- 'routingProfileId', 'createUser_routingProfileId' - The identifier of the routing profile for the user.
--
-- 'instanceId', 'createUser_instanceId' - The identifier of the Amazon Connect instance.
newCreateUser ::
  -- | 'username'
  Core.Text ->
  -- | 'phoneConfig'
  UserPhoneConfig ->
  -- | 'securityProfileIds'
  Core.NonEmpty Core.Text ->
  -- | 'routingProfileId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  CreateUser
newCreateUser
  pUsername_
  pPhoneConfig_
  pSecurityProfileIds_
  pRoutingProfileId_
  pInstanceId_ =
    CreateUser'
      { identityInfo = Core.Nothing,
        hierarchyGroupId = Core.Nothing,
        directoryUserId = Core.Nothing,
        password = Core.Nothing,
        tags = Core.Nothing,
        username = pUsername_,
        phoneConfig = pPhoneConfig_,
        securityProfileIds =
          Lens._Coerce Lens.# pSecurityProfileIds_,
        routingProfileId = pRoutingProfileId_,
        instanceId = pInstanceId_
      }

-- | The information about the identity of the user.
createUser_identityInfo :: Lens.Lens' CreateUser (Core.Maybe UserIdentityInfo)
createUser_identityInfo = Lens.lens (\CreateUser' {identityInfo} -> identityInfo) (\s@CreateUser' {} a -> s {identityInfo = a} :: CreateUser)

-- | The identifier of the hierarchy group for the user.
createUser_hierarchyGroupId :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_hierarchyGroupId = Lens.lens (\CreateUser' {hierarchyGroupId} -> hierarchyGroupId) (\s@CreateUser' {} a -> s {hierarchyGroupId = a} :: CreateUser)

-- | The identifier of the user account in the directory used for identity
-- management. If Amazon Connect cannot access the directory, you can
-- specify this identifier to authenticate users. If you include the
-- identifier, we assume that Amazon Connect cannot access the directory.
-- Otherwise, the identity information is used to authenticate users from
-- your directory.
--
-- This parameter is required if you are using an existing directory for
-- identity management in Amazon Connect when Amazon Connect cannot access
-- your directory to authenticate users. If you are using SAML for identity
-- management and include this parameter, an error is returned.
createUser_directoryUserId :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_directoryUserId = Lens.lens (\CreateUser' {directoryUserId} -> directoryUserId) (\s@CreateUser' {} a -> s {directoryUserId = a} :: CreateUser)

-- | The password for the user account. A password is required if you are
-- using Amazon Connect for identity management. Otherwise, it is an error
-- to include a password.
createUser_password :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser)

-- | One or more tags.
createUser_tags :: Lens.Lens' CreateUser (Core.Maybe (Core.HashMap Core.Text Core.Text))
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Core.. Lens.mapping Lens._Coerce

-- | The user name for the account. For instances not using SAML for identity
-- management, the user name can include up to 20 characters. If you are
-- using SAML for identity management, the user name can include up to 64
-- characters from [a-zA-Z0-9_-.\\\@]+.
createUser_username :: Lens.Lens' CreateUser Core.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The phone settings for the user.
createUser_phoneConfig :: Lens.Lens' CreateUser UserPhoneConfig
createUser_phoneConfig = Lens.lens (\CreateUser' {phoneConfig} -> phoneConfig) (\s@CreateUser' {} a -> s {phoneConfig = a} :: CreateUser)

-- | The identifier of the security profile for the user.
createUser_securityProfileIds :: Lens.Lens' CreateUser (Core.NonEmpty Core.Text)
createUser_securityProfileIds = Lens.lens (\CreateUser' {securityProfileIds} -> securityProfileIds) (\s@CreateUser' {} a -> s {securityProfileIds = a} :: CreateUser) Core.. Lens._Coerce

-- | The identifier of the routing profile for the user.
createUser_routingProfileId :: Lens.Lens' CreateUser Core.Text
createUser_routingProfileId = Lens.lens (\CreateUser' {routingProfileId} -> routingProfileId) (\s@CreateUser' {} a -> s {routingProfileId = a} :: CreateUser)

-- | The identifier of the Amazon Connect instance.
createUser_instanceId :: Lens.Lens' CreateUser Core.Text
createUser_instanceId = Lens.lens (\CreateUser' {instanceId} -> instanceId) (\s@CreateUser' {} a -> s {instanceId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..?> "UserArn")
            Core.<*> (x Core..?> "UserId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IdentityInfo" Core..=) Core.<$> identityInfo,
            ("HierarchyGroupId" Core..=)
              Core.<$> hierarchyGroupId,
            ("DirectoryUserId" Core..=) Core.<$> directoryUserId,
            ("Password" Core..=) Core.<$> password,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Username" Core..= username),
            Core.Just ("PhoneConfig" Core..= phoneConfig),
            Core.Just
              ("SecurityProfileIds" Core..= securityProfileIds),
            Core.Just
              ("RoutingProfileId" Core..= routingProfileId)
          ]
      )

instance Core.ToPath CreateUser where
  toPath CreateUser' {..} =
    Core.mconcat ["/users/", Core.toBS instanceId]

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The Amazon Resource Name (ARN) of the user account.
    userArn :: Core.Maybe Core.Text,
    -- | The identifier of the user account.
    userId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'createUserResponse_userArn' - The Amazon Resource Name (ARN) of the user account.
--
-- 'userId', 'createUserResponse_userId' - The identifier of the user account.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userArn = Core.Nothing,
      userId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the user account.
createUserResponse_userArn :: Lens.Lens' CreateUserResponse (Core.Maybe Core.Text)
createUserResponse_userArn = Lens.lens (\CreateUserResponse' {userArn} -> userArn) (\s@CreateUserResponse' {} a -> s {userArn = a} :: CreateUserResponse)

-- | The identifier of the user account.
createUserResponse_userId :: Lens.Lens' CreateUserResponse (Core.Maybe Core.Text)
createUserResponse_userId = Lens.lens (\CreateUserResponse' {userId} -> userId) (\s@CreateUserResponse' {} a -> s {userId = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
