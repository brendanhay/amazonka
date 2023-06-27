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
-- Module      : Amazonka.Transfer.Types.DescribedAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.HomeDirectoryMapEntry
import Amazonka.Transfer.Types.HomeDirectoryType
import Amazonka.Transfer.Types.PosixProfile

-- | Describes the properties of the access that was specified.
--
-- /See:/ 'newDescribedAccess' smart constructor.
data DescribedAccess = DescribedAccess'
  { -- | A unique identifier that is required to identify specific groups within
    -- your directory. The users of the group that you associate have access to
    -- your Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Transfer Family. If you know the group name, you can view the SID values
    -- by running the following command using Windows PowerShell.
    --
    -- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
    --
    -- In that command, replace /YourGroupName/ with the name of your Active
    -- Directory group.
    --
    -- The regular expression used to validate this parameter is a string of
    -- characters consisting of uppercase and lowercase alphanumeric characters
    -- with no spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The landing directory (folder) for a user when they log in to the server
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
    -- In most cases, you can use this value instead of the session policy to
    -- lock down the associated access to the designated home directory
    -- (\"@chroot@\"). To do this, you can set @Entry@ to \'\/\' and set
    -- @Target@ to the @HomeDirectory@ parameter value.
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
    policy :: Prelude.Maybe Prelude.Text,
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that controls your users\' access to your Amazon S3 bucket or
    -- Amazon EFS file system. The policies attached to this role determine the
    -- level of access that you want to provide your users when transferring
    -- files into and out of your Amazon S3 bucket or Amazon EFS file system.
    -- The IAM role should also contain a trust relationship that allows the
    -- server to access your resources when servicing your users\' transfer
    -- requests.
    role' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'describedAccess_externalId' - A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
--
-- 'homeDirectory', 'describedAccess_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'homeDirectoryMappings', 'describedAccess_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
--
-- In most cases, you can use this value instead of the session policy to
-- lock down the associated access to the designated home directory
-- (\"@chroot@\"). To do this, you can set @Entry@ to \'\/\' and set
-- @Target@ to the @HomeDirectory@ parameter value.
--
-- 'homeDirectoryType', 'describedAccess_homeDirectoryType' - The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
--
-- 'policy', 'describedAccess_policy' - A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
--
-- 'posixProfile', 'describedAccess_posixProfile' - Undocumented member.
--
-- 'role'', 'describedAccess_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
newDescribedAccess ::
  DescribedAccess
newDescribedAccess =
  DescribedAccess'
    { externalId = Prelude.Nothing,
      homeDirectory = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      policy = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
describedAccess_externalId :: Lens.Lens' DescribedAccess (Prelude.Maybe Prelude.Text)
describedAccess_externalId = Lens.lens (\DescribedAccess' {externalId} -> externalId) (\s@DescribedAccess' {} a -> s {externalId = a} :: DescribedAccess)

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
describedAccess_homeDirectory :: Lens.Lens' DescribedAccess (Prelude.Maybe Prelude.Text)
describedAccess_homeDirectory = Lens.lens (\DescribedAccess' {homeDirectory} -> homeDirectory) (\s@DescribedAccess' {} a -> s {homeDirectory = a} :: DescribedAccess)

-- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
--
-- In most cases, you can use this value instead of the session policy to
-- lock down the associated access to the designated home directory
-- (\"@chroot@\"). To do this, you can set @Entry@ to \'\/\' and set
-- @Target@ to the @HomeDirectory@ parameter value.
describedAccess_homeDirectoryMappings :: Lens.Lens' DescribedAccess (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
describedAccess_homeDirectoryMappings = Lens.lens (\DescribedAccess' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@DescribedAccess' {} a -> s {homeDirectoryMappings = a} :: DescribedAccess) Prelude.. Lens.mapping Lens.coerced

-- | The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
describedAccess_homeDirectoryType :: Lens.Lens' DescribedAccess (Prelude.Maybe HomeDirectoryType)
describedAccess_homeDirectoryType = Lens.lens (\DescribedAccess' {homeDirectoryType} -> homeDirectoryType) (\s@DescribedAccess' {} a -> s {homeDirectoryType = a} :: DescribedAccess)

-- | A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
describedAccess_policy :: Lens.Lens' DescribedAccess (Prelude.Maybe Prelude.Text)
describedAccess_policy = Lens.lens (\DescribedAccess' {policy} -> policy) (\s@DescribedAccess' {} a -> s {policy = a} :: DescribedAccess)

-- | Undocumented member.
describedAccess_posixProfile :: Lens.Lens' DescribedAccess (Prelude.Maybe PosixProfile)
describedAccess_posixProfile = Lens.lens (\DescribedAccess' {posixProfile} -> posixProfile) (\s@DescribedAccess' {} a -> s {posixProfile = a} :: DescribedAccess)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
describedAccess_role :: Lens.Lens' DescribedAccess (Prelude.Maybe Prelude.Text)
describedAccess_role = Lens.lens (\DescribedAccess' {role'} -> role') (\s@DescribedAccess' {} a -> s {role' = a} :: DescribedAccess)

instance Data.FromJSON DescribedAccess where
  parseJSON =
    Data.withObject
      "DescribedAccess"
      ( \x ->
          DescribedAccess'
            Prelude.<$> (x Data..:? "ExternalId")
            Prelude.<*> (x Data..:? "HomeDirectory")
            Prelude.<*> (x Data..:? "HomeDirectoryMappings")
            Prelude.<*> (x Data..:? "HomeDirectoryType")
            Prelude.<*> (x Data..:? "Policy")
            Prelude.<*> (x Data..:? "PosixProfile")
            Prelude.<*> (x Data..:? "Role")
      )

instance Prelude.Hashable DescribedAccess where
  hashWithSalt _salt DescribedAccess' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` homeDirectoryMappings
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` posixProfile
      `Prelude.hashWithSalt` role'

instance Prelude.NFData DescribedAccess where
  rnf DescribedAccess' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf homeDirectory
      `Prelude.seq` Prelude.rnf homeDirectoryMappings
      `Prelude.seq` Prelude.rnf homeDirectoryType
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf posixProfile
      `Prelude.seq` Prelude.rnf role'
