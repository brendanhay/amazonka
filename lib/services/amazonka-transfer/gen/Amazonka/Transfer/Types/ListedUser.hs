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
-- Module      : Amazonka.Transfer.Types.ListedUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.HomeDirectoryType

-- | Returns properties of the user that you specify.
--
-- /See:/ 'newListedUser' smart constructor.
data ListedUser = ListedUser'
  { -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
    -- | The type of landing directory (folder) that you want your users\' home
    -- directory to be when they log in to the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or Amazon EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that controls your users\' access to your Amazon S3 bucket or
    -- Amazon EFS file system. The policies attached to this role determine the
    -- level of access that you want to provide your users when transferring
    -- files into and out of your Amazon S3 bucket or Amazon EFS file system.
    -- The IAM role should also contain a trust relationship that allows the
    -- server to access your resources when servicing your users\' transfer
    -- requests.
    --
    -- The IAM role that controls your users\' access to your Amazon S3 bucket
    -- for servers with @Domain=S3@, or your EFS file system for servers with
    -- @Domain=EFS@.
    --
    -- The policies attached to this role determine the level of access you
    -- want to provide your users when transferring files into and out of your
    -- S3 buckets or EFS file systems.
    role' :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of SSH public keys stored for the user you
    -- specified.
    sshPublicKeyCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the name of the user whose ARN was specified. User names are
    -- used for authentication purposes.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Provides the unique Amazon Resource Name (ARN) for the user that you
    -- want to learn about.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeDirectory', 'listedUser_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'homeDirectoryType', 'listedUser_homeDirectoryType' - The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
--
-- 'role'', 'listedUser_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
--
-- The IAM role that controls your users\' access to your Amazon S3 bucket
-- for servers with @Domain=S3@, or your EFS file system for servers with
-- @Domain=EFS@.
--
-- The policies attached to this role determine the level of access you
-- want to provide your users when transferring files into and out of your
-- S3 buckets or EFS file systems.
--
-- 'sshPublicKeyCount', 'listedUser_sshPublicKeyCount' - Specifies the number of SSH public keys stored for the user you
-- specified.
--
-- 'userName', 'listedUser_userName' - Specifies the name of the user whose ARN was specified. User names are
-- used for authentication purposes.
--
-- 'arn', 'listedUser_arn' - Provides the unique Amazon Resource Name (ARN) for the user that you
-- want to learn about.
newListedUser ::
  -- | 'arn'
  Prelude.Text ->
  ListedUser
newListedUser pArn_ =
  ListedUser'
    { homeDirectory = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      role' = Prelude.Nothing,
      sshPublicKeyCount = Prelude.Nothing,
      userName = Prelude.Nothing,
      arn = pArn_
    }

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
listedUser_homeDirectory :: Lens.Lens' ListedUser (Prelude.Maybe Prelude.Text)
listedUser_homeDirectory = Lens.lens (\ListedUser' {homeDirectory} -> homeDirectory) (\s@ListedUser' {} a -> s {homeDirectory = a} :: ListedUser)

-- | The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
listedUser_homeDirectoryType :: Lens.Lens' ListedUser (Prelude.Maybe HomeDirectoryType)
listedUser_homeDirectoryType = Lens.lens (\ListedUser' {homeDirectoryType} -> homeDirectoryType) (\s@ListedUser' {} a -> s {homeDirectoryType = a} :: ListedUser)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
--
-- The IAM role that controls your users\' access to your Amazon S3 bucket
-- for servers with @Domain=S3@, or your EFS file system for servers with
-- @Domain=EFS@.
--
-- The policies attached to this role determine the level of access you
-- want to provide your users when transferring files into and out of your
-- S3 buckets or EFS file systems.
listedUser_role :: Lens.Lens' ListedUser (Prelude.Maybe Prelude.Text)
listedUser_role = Lens.lens (\ListedUser' {role'} -> role') (\s@ListedUser' {} a -> s {role' = a} :: ListedUser)

-- | Specifies the number of SSH public keys stored for the user you
-- specified.
listedUser_sshPublicKeyCount :: Lens.Lens' ListedUser (Prelude.Maybe Prelude.Int)
listedUser_sshPublicKeyCount = Lens.lens (\ListedUser' {sshPublicKeyCount} -> sshPublicKeyCount) (\s@ListedUser' {} a -> s {sshPublicKeyCount = a} :: ListedUser)

-- | Specifies the name of the user whose ARN was specified. User names are
-- used for authentication purposes.
listedUser_userName :: Lens.Lens' ListedUser (Prelude.Maybe Prelude.Text)
listedUser_userName = Lens.lens (\ListedUser' {userName} -> userName) (\s@ListedUser' {} a -> s {userName = a} :: ListedUser)

-- | Provides the unique Amazon Resource Name (ARN) for the user that you
-- want to learn about.
listedUser_arn :: Lens.Lens' ListedUser Prelude.Text
listedUser_arn = Lens.lens (\ListedUser' {arn} -> arn) (\s@ListedUser' {} a -> s {arn = a} :: ListedUser)

instance Data.FromJSON ListedUser where
  parseJSON =
    Data.withObject
      "ListedUser"
      ( \x ->
          ListedUser'
            Prelude.<$> (x Data..:? "HomeDirectory")
            Prelude.<*> (x Data..:? "HomeDirectoryType")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "SshPublicKeyCount")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable ListedUser where
  hashWithSalt _salt ListedUser' {..} =
    _salt
      `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` sshPublicKeyCount
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListedUser where
  rnf ListedUser' {..} =
    Prelude.rnf homeDirectory
      `Prelude.seq` Prelude.rnf homeDirectoryType
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf sshPublicKeyCount
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf arn
