{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.Types.DescribedUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.HomeDirectoryMapEntry
import Amazonka.Transfer.Types.HomeDirectoryType
import Amazonka.Transfer.Types.PosixProfile
import Amazonka.Transfer.Types.SshPublicKey
import Amazonka.Transfer.Types.Tag

-- | Describes the properties of a user that was specified.
--
-- /See:/ 'newDescribedUser' smart constructor.
data DescribedUser = DescribedUser'
  { -- | Specifies the public key portion of the Secure Shell (SSH) keys stored
    -- for the described user.
    sshPublicKeys :: Prelude.Maybe [SshPublicKey],
    -- | The type of landing directory (folder) you want your users\' home
    -- directory to be when they log into the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    -- | Specifies the name of the user that was requested to be described. User
    -- names are used for authentication purposes. This is the string that will
    -- be used by your user when they log in to your server.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
    -- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
    -- your users\' access to your Amazon Elastic File System (Amazon EFS) file
    -- systems. The POSIX permissions that are set on files and directories in
    -- your file system determine the level of access your users get when
    -- transferring files into and out of your Amazon EFS file systems.
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
    -- paths and keys should be visible to your user and how you want to make
    -- them visible. You must specify the @Entry@ and @Target@ pair, where
    -- @Entry@ shows how the path is made visible and @Target@ is the actual
    -- Amazon S3 or Amazon EFS path. If you only specify a target, it is
    -- displayed as is. You also must ensure that your Amazon Web Services
    -- Identity and Access Management (IAM) role provides access to paths in
    -- @Target@. This value can only be set when @HomeDirectoryType@ is set to
    -- /LOGICAL/.
    --
    -- In most cases, you can use this value instead of the session policy to
    -- lock your user down to the designated home directory (\"@chroot@\"). To
    -- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
    -- HomeDirectory parameter value.
    homeDirectoryMappings :: Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry),
    -- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
    -- your users\' access to your Amazon S3 bucket or EFS file system. The
    -- policies attached to this role determine the level of access that you
    -- want to provide your users when transferring files into and out of your
    -- Amazon S3 bucket or EFS file system. The IAM role should also contain a
    -- trust relationship that allows the server to access your resources when
    -- servicing your users\' transfer requests.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A session policy for your user so that you can use the same IAM role
    -- across multiple users. This policy scopes down user access to portions
    -- of their Amazon S3 bucket. Variables that you can use inside this policy
    -- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
    -- @${Transfer:HomeBucket}@.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
    -- | Specifies the key-value pairs for the user requested. Tag can be used to
    -- search for and group users for a variety of purposes.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the unique Amazon Resource Name (ARN) for the user that was
    -- requested to be described.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKeys', 'describedUser_sshPublicKeys' - Specifies the public key portion of the Secure Shell (SSH) keys stored
-- for the described user.
--
-- 'homeDirectoryType', 'describedUser_homeDirectoryType' - The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
--
-- 'userName', 'describedUser_userName' - Specifies the name of the user that was requested to be described. User
-- names are used for authentication purposes. This is the string that will
-- be used by your user when they log in to your server.
--
-- 'posixProfile', 'describedUser_posixProfile' - Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon Elastic File System (Amazon EFS) file
-- systems. The POSIX permissions that are set on files and directories in
-- your file system determine the level of access your users get when
-- transferring files into and out of your Amazon EFS file systems.
--
-- 'homeDirectoryMappings', 'describedUser_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Amazon Web Services
-- Identity and Access Management (IAM) role provides access to paths in
-- @Target@. This value can only be set when @HomeDirectoryType@ is set to
-- /LOGICAL/.
--
-- In most cases, you can use this value instead of the session policy to
-- lock your user down to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
-- HomeDirectory parameter value.
--
-- 'role'', 'describedUser_role' - Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
--
-- 'policy', 'describedUser_policy' - A session policy for your user so that you can use the same IAM role
-- across multiple users. This policy scopes down user access to portions
-- of their Amazon S3 bucket. Variables that you can use inside this policy
-- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
-- @${Transfer:HomeBucket}@.
--
-- 'homeDirectory', 'describedUser_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'tags', 'describedUser_tags' - Specifies the key-value pairs for the user requested. Tag can be used to
-- search for and group users for a variety of purposes.
--
-- 'arn', 'describedUser_arn' - Specifies the unique Amazon Resource Name (ARN) for the user that was
-- requested to be described.
newDescribedUser ::
  -- | 'arn'
  Prelude.Text ->
  DescribedUser
newDescribedUser pArn_ =
  DescribedUser'
    { sshPublicKeys = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      userName = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      role' = Prelude.Nothing,
      policy = Prelude.Nothing,
      homeDirectory = Prelude.Nothing,
      tags = Prelude.Nothing,
      arn = pArn_
    }

-- | Specifies the public key portion of the Secure Shell (SSH) keys stored
-- for the described user.
describedUser_sshPublicKeys :: Lens.Lens' DescribedUser (Prelude.Maybe [SshPublicKey])
describedUser_sshPublicKeys = Lens.lens (\DescribedUser' {sshPublicKeys} -> sshPublicKeys) (\s@DescribedUser' {} a -> s {sshPublicKeys = a} :: DescribedUser) Prelude.. Lens.mapping Lens.coerced

-- | The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
describedUser_homeDirectoryType :: Lens.Lens' DescribedUser (Prelude.Maybe HomeDirectoryType)
describedUser_homeDirectoryType = Lens.lens (\DescribedUser' {homeDirectoryType} -> homeDirectoryType) (\s@DescribedUser' {} a -> s {homeDirectoryType = a} :: DescribedUser)

-- | Specifies the name of the user that was requested to be described. User
-- names are used for authentication purposes. This is the string that will
-- be used by your user when they log in to your server.
describedUser_userName :: Lens.Lens' DescribedUser (Prelude.Maybe Prelude.Text)
describedUser_userName = Lens.lens (\DescribedUser' {userName} -> userName) (\s@DescribedUser' {} a -> s {userName = a} :: DescribedUser)

-- | Specifies the full POSIX identity, including user ID (@Uid@), group ID
-- (@Gid@), and any secondary groups IDs (@SecondaryGids@), that controls
-- your users\' access to your Amazon Elastic File System (Amazon EFS) file
-- systems. The POSIX permissions that are set on files and directories in
-- your file system determine the level of access your users get when
-- transferring files into and out of your Amazon EFS file systems.
describedUser_posixProfile :: Lens.Lens' DescribedUser (Prelude.Maybe PosixProfile)
describedUser_posixProfile = Lens.lens (\DescribedUser' {posixProfile} -> posixProfile) (\s@DescribedUser' {} a -> s {posixProfile = a} :: DescribedUser)

-- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Amazon Web Services
-- Identity and Access Management (IAM) role provides access to paths in
-- @Target@. This value can only be set when @HomeDirectoryType@ is set to
-- /LOGICAL/.
--
-- In most cases, you can use this value instead of the session policy to
-- lock your user down to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to \'\/\' and set @Target@ to the
-- HomeDirectory parameter value.
describedUser_homeDirectoryMappings :: Lens.Lens' DescribedUser (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
describedUser_homeDirectoryMappings = Lens.lens (\DescribedUser' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@DescribedUser' {} a -> s {homeDirectoryMappings = a} :: DescribedUser) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
describedUser_role :: Lens.Lens' DescribedUser (Prelude.Maybe Prelude.Text)
describedUser_role = Lens.lens (\DescribedUser' {role'} -> role') (\s@DescribedUser' {} a -> s {role' = a} :: DescribedUser)

-- | A session policy for your user so that you can use the same IAM role
-- across multiple users. This policy scopes down user access to portions
-- of their Amazon S3 bucket. Variables that you can use inside this policy
-- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
-- @${Transfer:HomeBucket}@.
describedUser_policy :: Lens.Lens' DescribedUser (Prelude.Maybe Prelude.Text)
describedUser_policy = Lens.lens (\DescribedUser' {policy} -> policy) (\s@DescribedUser' {} a -> s {policy = a} :: DescribedUser)

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
describedUser_homeDirectory :: Lens.Lens' DescribedUser (Prelude.Maybe Prelude.Text)
describedUser_homeDirectory = Lens.lens (\DescribedUser' {homeDirectory} -> homeDirectory) (\s@DescribedUser' {} a -> s {homeDirectory = a} :: DescribedUser)

-- | Specifies the key-value pairs for the user requested. Tag can be used to
-- search for and group users for a variety of purposes.
describedUser_tags :: Lens.Lens' DescribedUser (Prelude.Maybe (Prelude.NonEmpty Tag))
describedUser_tags = Lens.lens (\DescribedUser' {tags} -> tags) (\s@DescribedUser' {} a -> s {tags = a} :: DescribedUser) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the unique Amazon Resource Name (ARN) for the user that was
-- requested to be described.
describedUser_arn :: Lens.Lens' DescribedUser Prelude.Text
describedUser_arn = Lens.lens (\DescribedUser' {arn} -> arn) (\s@DescribedUser' {} a -> s {arn = a} :: DescribedUser)

instance Core.FromJSON DescribedUser where
  parseJSON =
    Core.withObject
      "DescribedUser"
      ( \x ->
          DescribedUser'
            Prelude.<$> (x Core..:? "SshPublicKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "HomeDirectoryType")
            Prelude.<*> (x Core..:? "UserName")
            Prelude.<*> (x Core..:? "PosixProfile")
            Prelude.<*> (x Core..:? "HomeDirectoryMappings")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "Policy")
            Prelude.<*> (x Core..:? "HomeDirectory")
            Prelude.<*> (x Core..:? "Tags")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedUser where
  hashWithSalt salt' DescribedUser' {..} =
    salt' `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` homeDirectoryMappings
      `Prelude.hashWithSalt` posixProfile
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` sshPublicKeys

instance Prelude.NFData DescribedUser where
  rnf DescribedUser' {..} =
    Prelude.rnf sshPublicKeys
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf homeDirectory
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf homeDirectoryMappings
      `Prelude.seq` Prelude.rnf posixProfile
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf homeDirectoryType
