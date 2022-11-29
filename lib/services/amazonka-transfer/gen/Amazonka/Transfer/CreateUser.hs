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
-- Module      : Amazonka.Transfer.CreateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user and associates them with an existing file transfer
-- protocol-enabled server. You can only create and associate users with
-- servers that have the @IdentityProviderType@ set to @SERVICE_MANAGED@.
-- Using parameters for @CreateUser@, you can specify the user name, set
-- the home directory, store the user\'s public key, and assign the user\'s
-- Identity and Access Management (IAM) role. You can also optionally add a
-- session policy, and assign metadata with tags that can be used to group
-- and search for users.
module Amazonka.Transfer.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_tags,
    createUser_homeDirectory,
    createUser_policy,
    createUser_sshPublicKeyBody,
    createUser_posixProfile,
    createUser_homeDirectoryType,
    createUser_homeDirectoryMappings,
    createUser_role,
    createUser_serverId,
    createUser_userName,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
    createUserResponse_serverId,
    createUserResponse_userName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | Key-value pairs that can be used to group and search for users. Tags are
    -- metadata attached to users for any purpose.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
    -- in the /Amazon Web Services Security Token Service API Reference/.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The public portion of the Secure Shell (SSH) key used to authenticate
    -- the user to the server.
    --
    -- The three standard SSH public key format elements are @\<key type>@,
    -- @\<body base64>@, and an optional @\<comment>@, with spaces between each
    -- element.
    --
    -- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
    --
    -- -   For RSA keys, the key type is @ssh-rsa@.
    --
    -- -   For ED25519 keys, the key type is @ssh-ed25519@.
    --
    -- -   For ECDSA keys, the key type is either @ecdsa-sha2-nistp256@,
    --     @ecdsa-sha2-nistp384@, or @ecdsa-sha2-nistp521@, depending on the
    --     size of the key you generated.
    sshPublicKeyBody :: Prelude.Maybe Prelude.Text,
    -- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
    -- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
    -- your users\' access to your Amazon EFS file systems. The POSIX
    -- permissions that are set on files and directories in Amazon EFS
    -- determine the level of access your users get when transferring files
    -- into and out of your Amazon EFS file systems.
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | The type of landing directory (folder) that you want your users\' home
    -- directory to be when they log in to the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or Amazon EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
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
    -- lock your user down to the designated home directory (\"@chroot@\"). To
    -- do this, you can set @Entry@ to @\/@ and set @Target@ to the
    -- HomeDirectory parameter value.
    --
    -- The following is an @Entry@ and @Target@ pair example for @chroot@.
    --
    -- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    homeDirectoryMappings :: Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry),
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that controls your users\' access to your Amazon S3 bucket or
    -- Amazon EFS file system. The policies attached to this role determine the
    -- level of access that you want to provide your users when transferring
    -- files into and out of your Amazon S3 bucket or Amazon EFS file system.
    -- The IAM role should also contain a trust relationship that allows the
    -- server to access your resources when servicing your users\' transfer
    -- requests.
    role' :: Prelude.Text,
    -- | A system-assigned unique identifier for a server instance. This is the
    -- specific server that you added your user to.
    serverId :: Prelude.Text,
    -- | A unique string that identifies a user and is associated with a
    -- @ServerId@. This user name must be a minimum of 3 and a maximum of 100
    -- characters long. The following are valid characters: a-z, A-Z, 0-9,
    -- underscore \'_\', hyphen \'-\', period \'.\', and at sign \'\@\'. The
    -- user name can\'t start with a hyphen, period, or at sign.
    userName :: Prelude.Text
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
-- 'tags', 'createUser_tags' - Key-value pairs that can be used to group and search for users. Tags are
-- metadata attached to users for any purpose.
--
-- 'homeDirectory', 'createUser_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'policy', 'createUser_policy' - A session policy for your user so that you can use the same Identity and
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
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web Services Security Token Service API Reference/.
--
-- 'sshPublicKeyBody', 'createUser_sshPublicKeyBody' - The public portion of the Secure Shell (SSH) key used to authenticate
-- the user to the server.
--
-- The three standard SSH public key format elements are @\<key type>@,
-- @\<body base64>@, and an optional @\<comment>@, with spaces between each
-- element.
--
-- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
--
-- -   For RSA keys, the key type is @ssh-rsa@.
--
-- -   For ED25519 keys, the key type is @ssh-ed25519@.
--
-- -   For ECDSA keys, the key type is either @ecdsa-sha2-nistp256@,
--     @ecdsa-sha2-nistp384@, or @ecdsa-sha2-nistp521@, depending on the
--     size of the key you generated.
--
-- 'posixProfile', 'createUser_posixProfile' - Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon EFS file systems. The POSIX
-- permissions that are set on files and directories in Amazon EFS
-- determine the level of access your users get when transferring files
-- into and out of your Amazon EFS file systems.
--
-- 'homeDirectoryType', 'createUser_homeDirectoryType' - The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
--
-- 'homeDirectoryMappings', 'createUser_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
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
-- lock your user down to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to @\/@ and set @Target@ to the
-- HomeDirectory parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- 'role'', 'createUser_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
--
-- 'serverId', 'createUser_serverId' - A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
--
-- 'userName', 'createUser_userName' - A unique string that identifies a user and is associated with a
-- @ServerId@. This user name must be a minimum of 3 and a maximum of 100
-- characters long. The following are valid characters: a-z, A-Z, 0-9,
-- underscore \'_\', hyphen \'-\', period \'.\', and at sign \'\@\'. The
-- user name can\'t start with a hyphen, period, or at sign.
newCreateUser ::
  -- | 'role''
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  CreateUser
newCreateUser pRole_ pServerId_ pUserName_ =
  CreateUser'
    { tags = Prelude.Nothing,
      homeDirectory = Prelude.Nothing,
      policy = Prelude.Nothing,
      sshPublicKeyBody = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      role' = pRole_,
      serverId = pServerId_,
      userName = pUserName_
    }

-- | Key-value pairs that can be used to group and search for users. Tags are
-- metadata attached to users for any purpose.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty Tag))
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
createUser_homeDirectory :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_homeDirectory = Lens.lens (\CreateUser' {homeDirectory} -> homeDirectory) (\s@CreateUser' {} a -> s {homeDirectory = a} :: CreateUser)

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
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web Services Security Token Service API Reference/.
createUser_policy :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_policy = Lens.lens (\CreateUser' {policy} -> policy) (\s@CreateUser' {} a -> s {policy = a} :: CreateUser)

-- | The public portion of the Secure Shell (SSH) key used to authenticate
-- the user to the server.
--
-- The three standard SSH public key format elements are @\<key type>@,
-- @\<body base64>@, and an optional @\<comment>@, with spaces between each
-- element.
--
-- Transfer Family accepts RSA, ECDSA, and ED25519 keys.
--
-- -   For RSA keys, the key type is @ssh-rsa@.
--
-- -   For ED25519 keys, the key type is @ssh-ed25519@.
--
-- -   For ECDSA keys, the key type is either @ecdsa-sha2-nistp256@,
--     @ecdsa-sha2-nistp384@, or @ecdsa-sha2-nistp521@, depending on the
--     size of the key you generated.
createUser_sshPublicKeyBody :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_sshPublicKeyBody = Lens.lens (\CreateUser' {sshPublicKeyBody} -> sshPublicKeyBody) (\s@CreateUser' {} a -> s {sshPublicKeyBody = a} :: CreateUser)

-- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon EFS file systems. The POSIX
-- permissions that are set on files and directories in Amazon EFS
-- determine the level of access your users get when transferring files
-- into and out of your Amazon EFS file systems.
createUser_posixProfile :: Lens.Lens' CreateUser (Prelude.Maybe PosixProfile)
createUser_posixProfile = Lens.lens (\CreateUser' {posixProfile} -> posixProfile) (\s@CreateUser' {} a -> s {posixProfile = a} :: CreateUser)

-- | The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
createUser_homeDirectoryType :: Lens.Lens' CreateUser (Prelude.Maybe HomeDirectoryType)
createUser_homeDirectoryType = Lens.lens (\CreateUser' {homeDirectoryType} -> homeDirectoryType) (\s@CreateUser' {} a -> s {homeDirectoryType = a} :: CreateUser)

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
-- lock your user down to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to @\/@ and set @Target@ to the
-- HomeDirectory parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
createUser_homeDirectoryMappings :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
createUser_homeDirectoryMappings = Lens.lens (\CreateUser' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@CreateUser' {} a -> s {homeDirectoryMappings = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
createUser_role :: Lens.Lens' CreateUser Prelude.Text
createUser_role = Lens.lens (\CreateUser' {role'} -> role') (\s@CreateUser' {} a -> s {role' = a} :: CreateUser)

-- | A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
createUser_serverId :: Lens.Lens' CreateUser Prelude.Text
createUser_serverId = Lens.lens (\CreateUser' {serverId} -> serverId) (\s@CreateUser' {} a -> s {serverId = a} :: CreateUser)

-- | A unique string that identifies a user and is associated with a
-- @ServerId@. This user name must be a minimum of 3 and a maximum of 100
-- characters long. The following are valid characters: a-z, A-Z, 0-9,
-- underscore \'_\', hyphen \'-\', period \'.\', and at sign \'\@\'. The
-- user name can\'t start with a hyphen, period, or at sign.
createUser_userName :: Lens.Lens' CreateUser Prelude.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ServerId")
            Prelude.<*> (x Core..:> "UserName")
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` sshPublicKeyBody
      `Prelude.hashWithSalt` posixProfile
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` homeDirectoryMappings
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf homeDirectory
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf sshPublicKeyBody
      `Prelude.seq` Prelude.rnf posixProfile
      `Prelude.seq` Prelude.rnf homeDirectoryType
      `Prelude.seq` Prelude.rnf homeDirectoryMappings
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf userName

instance Core.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TransferService.CreateUser" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("HomeDirectory" Core..=) Prelude.<$> homeDirectory,
            ("Policy" Core..=) Prelude.<$> policy,
            ("SshPublicKeyBody" Core..=)
              Prelude.<$> sshPublicKeyBody,
            ("PosixProfile" Core..=) Prelude.<$> posixProfile,
            ("HomeDirectoryType" Core..=)
              Prelude.<$> homeDirectoryType,
            ("HomeDirectoryMappings" Core..=)
              Prelude.<$> homeDirectoryMappings,
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("ServerId" Core..= serverId),
            Prelude.Just ("UserName" Core..= userName)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the server that the user is attached to.
    serverId :: Prelude.Text,
    -- | A unique string that identifies a user account associated with a server.
    userName :: Prelude.Text
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
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'createUserResponse_serverId' - The identifier of the server that the user is attached to.
--
-- 'userName', 'createUserResponse_userName' - A unique string that identifies a user account associated with a server.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  CreateUserResponse
newCreateUserResponse
  pHttpStatus_
  pServerId_
  pUserName_ =
    CreateUserResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        userName = pUserName_
      }

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

-- | The identifier of the server that the user is attached to.
createUserResponse_serverId :: Lens.Lens' CreateUserResponse Prelude.Text
createUserResponse_serverId = Lens.lens (\CreateUserResponse' {serverId} -> serverId) (\s@CreateUserResponse' {} a -> s {serverId = a} :: CreateUserResponse)

-- | A unique string that identifies a user account associated with a server.
createUserResponse_userName :: Lens.Lens' CreateUserResponse Prelude.Text
createUserResponse_userName = Lens.lens (\CreateUserResponse' {userName} -> userName) (\s@CreateUserResponse' {} a -> s {userName = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf userName
