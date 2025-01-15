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
-- Module      : Amazonka.Transfer.UpdateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns new properties to a user. Parameters you pass modify any or all
-- of the following: the home directory, role, and policy for the
-- @UserName@ and @ServerId@ you specify.
--
-- The response returns the @ServerId@ and the @UserName@ for the updated
-- user.
module Amazonka.Transfer.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_homeDirectory,
    updateUser_homeDirectoryMappings,
    updateUser_homeDirectoryType,
    updateUser_policy,
    updateUser_posixProfile,
    updateUser_role,
    updateUser_serverId,
    updateUser_userName,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_httpStatus,
    updateUserResponse_serverId,
    updateUserResponse_userName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
    -- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
    -- paths and keys should be visible to your user and how you want to make
    -- them visible. You must specify the @Entry@ and @Target@ pair, where
    -- @Entry@ shows how the path is made visible and @Target@ is the actual
    -- Amazon S3 or Amazon EFS path. If you only specify a target, it is
    -- displayed as is. You also must ensure that your Identity and Access
    -- Management (IAM) role provides access to paths in @Target@. This value
    -- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
    --
    -- The following is an @Entry@ and @Target@ pair example.
    --
    -- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    --
    -- In most cases, you can use this value instead of the session policy to
    -- lock down your user to the designated home directory (\"@chroot@\"). To
    -- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
    -- HomeDirectory parameter value.
    --
    -- The following is an @Entry@ and @Target@ pair example for @chroot@.
    --
    -- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    homeDirectoryMappings :: Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry),
    -- | The type of landing directory (folder) that you want your users\' home
    -- directory to be when they log in to the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or Amazon EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    -- | A session policy for your user so that you can use the same Identity and
    -- Access Management (IAM) role across multiple users. This policy scopes
    -- down a user\'s access to portions of their Amazon S3 bucket. Variables
    -- that you can use inside this policy include @${Transfer:UserName}@,
    -- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
    --
    -- This policy applies only when the domain of @ServerId@ is Amazon S3.
    -- Amazon EFS does not use session policies.
    --
    -- For session policies, Transfer Family stores the policy as a JSON blob,
    -- instead of the Amazon Resource Name (ARN) of the policy. You save the
    -- policy as a JSON blob and pass it in the @Policy@ argument.
    --
    -- For an example of a session policy, see
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy Creating a session policy>.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
    -- in the /Amazon Web Services Security Token Service API Reference/.
    policy :: Prelude.Maybe Prelude.Text,
    -- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
    -- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
    -- your users\' access to your Amazon Elastic File Systems (Amazon EFS).
    -- The POSIX permissions that are set on files and directories in your file
    -- system determines the level of access your users get when transferring
    -- files into and out of your Amazon EFS file systems.
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that controls your users\' access to your Amazon S3 bucket or
    -- Amazon EFS file system. The policies attached to this role determine the
    -- level of access that you want to provide your users when transferring
    -- files into and out of your Amazon S3 bucket or Amazon EFS file system.
    -- The IAM role should also contain a trust relationship that allows the
    -- server to access your resources when servicing your users\' transfer
    -- requests.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A system-assigned unique identifier for a server instance that the user
    -- account is assigned to.
    serverId :: Prelude.Text,
    -- | A unique string that identifies a user and is associated with a server
    -- as specified by the @ServerId@. This user name must be a minimum of 3
    -- and a maximum of 100 characters long. The following are valid
    -- characters: a-z, A-Z, 0-9, underscore \'_\', hyphen \'-\', period \'.\',
    -- and at sign \'\@\'. The user name can\'t start with a hyphen, period, or
    -- at sign.
    userName :: Prelude.Text
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
-- 'homeDirectory', 'updateUser_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'homeDirectoryMappings', 'updateUser_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
--
-- The following is an @Entry@ and @Target@ pair example.
--
-- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- In most cases, you can use this value instead of the session policy to
-- lock down your user to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
-- HomeDirectory parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- 'homeDirectoryType', 'updateUser_homeDirectoryType' - The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
--
-- 'policy', 'updateUser_policy' - A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
--
-- This policy applies only when the domain of @ServerId@ is Amazon S3.
-- Amazon EFS does not use session policies.
--
-- For session policies, Transfer Family stores the policy as a JSON blob,
-- instead of the Amazon Resource Name (ARN) of the policy. You save the
-- policy as a JSON blob and pass it in the @Policy@ argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy Creating a session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web Services Security Token Service API Reference/.
--
-- 'posixProfile', 'updateUser_posixProfile' - Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon Elastic File Systems (Amazon EFS).
-- The POSIX permissions that are set on files and directories in your file
-- system determines the level of access your users get when transferring
-- files into and out of your Amazon EFS file systems.
--
-- 'role'', 'updateUser_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
--
-- 'serverId', 'updateUser_serverId' - A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
--
-- 'userName', 'updateUser_userName' - A unique string that identifies a user and is associated with a server
-- as specified by the @ServerId@. This user name must be a minimum of 3
-- and a maximum of 100 characters long. The following are valid
-- characters: a-z, A-Z, 0-9, underscore \'_\', hyphen \'-\', period \'.\',
-- and at sign \'\@\'. The user name can\'t start with a hyphen, period, or
-- at sign.
newUpdateUser ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  UpdateUser
newUpdateUser pServerId_ pUserName_ =
  UpdateUser'
    { homeDirectory = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      policy = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      role' = Prelude.Nothing,
      serverId = pServerId_,
      userName = pUserName_
    }

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
updateUser_homeDirectory :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_homeDirectory = Lens.lens (\UpdateUser' {homeDirectory} -> homeDirectory) (\s@UpdateUser' {} a -> s {homeDirectory = a} :: UpdateUser)

-- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
--
-- The following is an @Entry@ and @Target@ pair example.
--
-- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- In most cases, you can use this value instead of the session policy to
-- lock down your user to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
-- HomeDirectory parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
updateUser_homeDirectoryMappings :: Lens.Lens' UpdateUser (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
updateUser_homeDirectoryMappings = Lens.lens (\UpdateUser' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@UpdateUser' {} a -> s {homeDirectoryMappings = a} :: UpdateUser) Prelude.. Lens.mapping Lens.coerced

-- | The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
updateUser_homeDirectoryType :: Lens.Lens' UpdateUser (Prelude.Maybe HomeDirectoryType)
updateUser_homeDirectoryType = Lens.lens (\UpdateUser' {homeDirectoryType} -> homeDirectoryType) (\s@UpdateUser' {} a -> s {homeDirectoryType = a} :: UpdateUser)

-- | A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
--
-- This policy applies only when the domain of @ServerId@ is Amazon S3.
-- Amazon EFS does not use session policies.
--
-- For session policies, Transfer Family stores the policy as a JSON blob,
-- instead of the Amazon Resource Name (ARN) of the policy. You save the
-- policy as a JSON blob and pass it in the @Policy@ argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy Creating a session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web Services Security Token Service API Reference/.
updateUser_policy :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_policy = Lens.lens (\UpdateUser' {policy} -> policy) (\s@UpdateUser' {} a -> s {policy = a} :: UpdateUser)

-- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon Elastic File Systems (Amazon EFS).
-- The POSIX permissions that are set on files and directories in your file
-- system determines the level of access your users get when transferring
-- files into and out of your Amazon EFS file systems.
updateUser_posixProfile :: Lens.Lens' UpdateUser (Prelude.Maybe PosixProfile)
updateUser_posixProfile = Lens.lens (\UpdateUser' {posixProfile} -> posixProfile) (\s@UpdateUser' {} a -> s {posixProfile = a} :: UpdateUser)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
updateUser_role :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_role = Lens.lens (\UpdateUser' {role'} -> role') (\s@UpdateUser' {} a -> s {role' = a} :: UpdateUser)

-- | A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
updateUser_serverId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_serverId = Lens.lens (\UpdateUser' {serverId} -> serverId) (\s@UpdateUser' {} a -> s {serverId = a} :: UpdateUser)

-- | A unique string that identifies a user and is associated with a server
-- as specified by the @ServerId@. This user name must be a minimum of 3
-- and a maximum of 100 characters long. The following are valid
-- characters: a-z, A-Z, 0-9, underscore \'_\', hyphen \'-\', period \'.\',
-- and at sign \'\@\'. The user name can\'t start with a hyphen, period, or
-- at sign.
updateUser_userName :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userName = Lens.lens (\UpdateUser' {userName} -> userName) (\s@UpdateUser' {} a -> s {userName = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..:> "UserName")
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt
      `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` homeDirectoryMappings
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` posixProfile
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf homeDirectory `Prelude.seq`
      Prelude.rnf homeDirectoryMappings `Prelude.seq`
        Prelude.rnf homeDirectoryType `Prelude.seq`
          Prelude.rnf policy `Prelude.seq`
            Prelude.rnf posixProfile `Prelude.seq`
              Prelude.rnf role' `Prelude.seq`
                Prelude.rnf serverId `Prelude.seq`
                  Prelude.rnf userName

instance Data.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TransferService.UpdateUser" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HomeDirectory" Data..=) Prelude.<$> homeDirectory,
            ("HomeDirectoryMappings" Data..=)
              Prelude.<$> homeDirectoryMappings,
            ("HomeDirectoryType" Data..=)
              Prelude.<$> homeDirectoryType,
            ("Policy" Data..=) Prelude.<$> policy,
            ("PosixProfile" Data..=) Prelude.<$> posixProfile,
            ("Role" Data..=) Prelude.<$> role',
            Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("UserName" Data..= userName)
          ]
      )

instance Data.ToPath UpdateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | @UpdateUserResponse@ returns the user name and identifier for the
-- request to update a user\'s properties.
--
-- /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server instance that the user
    -- account is assigned to.
    serverId :: Prelude.Text,
    -- | The unique identifier for a user that is assigned to a server instance
    -- that was specified in the request.
    userName :: Prelude.Text
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
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'updateUserResponse_serverId' - A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
--
-- 'userName', 'updateUserResponse_userName' - The unique identifier for a user that is assigned to a server instance
-- that was specified in the request.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  UpdateUserResponse
newUpdateUserResponse
  pHttpStatus_
  pServerId_
  pUserName_ =
    UpdateUserResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        userName = pUserName_
      }

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

-- | A system-assigned unique identifier for a server instance that the user
-- account is assigned to.
updateUserResponse_serverId :: Lens.Lens' UpdateUserResponse Prelude.Text
updateUserResponse_serverId = Lens.lens (\UpdateUserResponse' {serverId} -> serverId) (\s@UpdateUserResponse' {} a -> s {serverId = a} :: UpdateUserResponse)

-- | The unique identifier for a user that is assigned to a server instance
-- that was specified in the request.
updateUserResponse_userName :: Lens.Lens' UpdateUserResponse Prelude.Text
updateUserResponse_userName = Lens.lens (\UpdateUserResponse' {userName} -> userName) (\s@UpdateUserResponse' {} a -> s {userName = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf serverId `Prelude.seq`
        Prelude.rnf userName
