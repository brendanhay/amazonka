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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The information about the identity of the user.
    identityInfo :: Prelude.Maybe UserIdentityInfo,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
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
    directoryUserId :: Prelude.Maybe Prelude.Text,
    -- | The password for the user account. A password is required if you are
    -- using Amazon Connect for identity management. Otherwise, it is an error
    -- to include a password.
    password :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user name for the account. For instances not using SAML for identity
    -- management, the user name can include up to 20 characters. If you are
    -- using SAML for identity management, the user name can include up to 64
    -- characters from [a-zA-Z0-9_-.\\\@]+.
    username :: Prelude.Text,
    -- | The phone settings for the user.
    phoneConfig :: UserPhoneConfig,
    -- | The identifier of the security profile for the user.
    securityProfileIds :: Prelude.NonEmpty Prelude.Text,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'phoneConfig'
  UserPhoneConfig ->
  -- | 'securityProfileIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  CreateUser
newCreateUser
  pUsername_
  pPhoneConfig_
  pSecurityProfileIds_
  pRoutingProfileId_
  pInstanceId_ =
    CreateUser'
      { identityInfo = Prelude.Nothing,
        hierarchyGroupId = Prelude.Nothing,
        directoryUserId = Prelude.Nothing,
        password = Prelude.Nothing,
        tags = Prelude.Nothing,
        username = pUsername_,
        phoneConfig = pPhoneConfig_,
        securityProfileIds =
          Lens._Coerce Lens.# pSecurityProfileIds_,
        routingProfileId = pRoutingProfileId_,
        instanceId = pInstanceId_
      }

-- | The information about the identity of the user.
createUser_identityInfo :: Lens.Lens' CreateUser (Prelude.Maybe UserIdentityInfo)
createUser_identityInfo = Lens.lens (\CreateUser' {identityInfo} -> identityInfo) (\s@CreateUser' {} a -> s {identityInfo = a} :: CreateUser)

-- | The identifier of the hierarchy group for the user.
createUser_hierarchyGroupId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
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
createUser_directoryUserId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_directoryUserId = Lens.lens (\CreateUser' {directoryUserId} -> directoryUserId) (\s@CreateUser' {} a -> s {directoryUserId = a} :: CreateUser)

-- | The password for the user account. A password is required if you are
-- using Amazon Connect for identity management. Otherwise, it is an error
-- to include a password.
createUser_password :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser)

-- | One or more tags.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Lens._Coerce

-- | The user name for the account. For instances not using SAML for identity
-- management, the user name can include up to 20 characters. If you are
-- using SAML for identity management, the user name can include up to 64
-- characters from [a-zA-Z0-9_-.\\\@]+.
createUser_username :: Lens.Lens' CreateUser Prelude.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The phone settings for the user.
createUser_phoneConfig :: Lens.Lens' CreateUser UserPhoneConfig
createUser_phoneConfig = Lens.lens (\CreateUser' {phoneConfig} -> phoneConfig) (\s@CreateUser' {} a -> s {phoneConfig = a} :: CreateUser)

-- | The identifier of the security profile for the user.
createUser_securityProfileIds :: Lens.Lens' CreateUser (Prelude.NonEmpty Prelude.Text)
createUser_securityProfileIds = Lens.lens (\CreateUser' {securityProfileIds} -> securityProfileIds) (\s@CreateUser' {} a -> s {securityProfileIds = a} :: CreateUser) Prelude.. Lens._Coerce

-- | The identifier of the routing profile for the user.
createUser_routingProfileId :: Lens.Lens' CreateUser Prelude.Text
createUser_routingProfileId = Lens.lens (\CreateUser' {routingProfileId} -> routingProfileId) (\s@CreateUser' {} a -> s {routingProfileId = a} :: CreateUser)

-- | The identifier of the Amazon Connect instance.
createUser_instanceId :: Lens.Lens' CreateUser Prelude.Text
createUser_instanceId = Lens.lens (\CreateUser' {instanceId} -> instanceId) (\s@CreateUser' {} a -> s {instanceId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Core..?> "UserArn")
            Prelude.<*> (x Core..?> "UserId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser

instance Prelude.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IdentityInfo" Core..=) Prelude.<$> identityInfo,
            ("HierarchyGroupId" Core..=)
              Prelude.<$> hierarchyGroupId,
            ("DirectoryUserId" Core..=)
              Prelude.<$> directoryUserId,
            ("Password" Core..=) Prelude.<$> password,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Username" Core..= username),
            Prelude.Just ("PhoneConfig" Core..= phoneConfig),
            Prelude.Just
              ("SecurityProfileIds" Core..= securityProfileIds),
            Prelude.Just
              ("RoutingProfileId" Core..= routingProfileId)
          ]
      )

instance Core.ToPath CreateUser where
  toPath CreateUser' {..} =
    Prelude.mconcat ["/users/", Core.toBS instanceId]

instance Core.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The Amazon Resource Name (ARN) of the user account.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userArn = Prelude.Nothing,
      userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the user account.
createUserResponse_userArn :: Lens.Lens' CreateUserResponse (Prelude.Maybe Prelude.Text)
createUserResponse_userArn = Lens.lens (\CreateUserResponse' {userArn} -> userArn) (\s@CreateUserResponse' {} a -> s {userArn = a} :: CreateUserResponse)

-- | The identifier of the user account.
createUserResponse_userId :: Lens.Lens' CreateUserResponse (Prelude.Maybe Prelude.Text)
createUserResponse_userId = Lens.lens (\CreateUserResponse' {userId} -> userId) (\s@CreateUserResponse' {} a -> s {userId = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse
