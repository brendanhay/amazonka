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
-- Module      : Amazonka.Transfer.Types.ListedAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.HomeDirectoryType

-- | Lists the properties for one or more specified associated accesses.
--
-- /See:/ 'newListedAccess' smart constructor.
data ListedAccess = ListedAccess'
  { -- | The type of landing directory (folder) you want your users\' home
    -- directory to be when they log into the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    -- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
    -- your users\' access to your Amazon S3 bucket or EFS file system. The
    -- policies attached to this role determine the level of access that you
    -- want to provide your users when transferring files into and out of your
    -- Amazon S3 bucket or EFS file system. The IAM role should also contain a
    -- trust relationship that allows the server to access your resources when
    -- servicing your users\' transfer requests.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that is required to identify specific groups within
    -- your directory. The users of the group that you associate have access to
    -- your Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Amazon Web Services Transfer Family. If you know the group name, you can
    -- view the SID values by running the following command using Windows
    -- PowerShell.
    --
    -- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
    --
    -- In that command, replace /YourGroupName/ with the name of your Active
    -- Directory group.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of uppercase and lowercase alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeDirectoryType', 'listedAccess_homeDirectoryType' - The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
--
-- 'role'', 'listedAccess_role' - Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
--
-- 'externalId', 'listedAccess_externalId' - A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Amazon Web Services Transfer Family. If you know the group name, you can
-- view the SID values by running the following command using Windows
-- PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of uppercase and lowercase alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
--
-- 'homeDirectory', 'listedAccess_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
newListedAccess ::
  ListedAccess
newListedAccess =
  ListedAccess'
    { homeDirectoryType = Prelude.Nothing,
      role' = Prelude.Nothing,
      externalId = Prelude.Nothing,
      homeDirectory = Prelude.Nothing
    }

-- | The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
listedAccess_homeDirectoryType :: Lens.Lens' ListedAccess (Prelude.Maybe HomeDirectoryType)
listedAccess_homeDirectoryType = Lens.lens (\ListedAccess' {homeDirectoryType} -> homeDirectoryType) (\s@ListedAccess' {} a -> s {homeDirectoryType = a} :: ListedAccess)

-- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
listedAccess_role :: Lens.Lens' ListedAccess (Prelude.Maybe Prelude.Text)
listedAccess_role = Lens.lens (\ListedAccess' {role'} -> role') (\s@ListedAccess' {} a -> s {role' = a} :: ListedAccess)

-- | A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Amazon Web Services Transfer Family. If you know the group name, you can
-- view the SID values by running the following command using Windows
-- PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of uppercase and lowercase alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
listedAccess_externalId :: Lens.Lens' ListedAccess (Prelude.Maybe Prelude.Text)
listedAccess_externalId = Lens.lens (\ListedAccess' {externalId} -> externalId) (\s@ListedAccess' {} a -> s {externalId = a} :: ListedAccess)

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
listedAccess_homeDirectory :: Lens.Lens' ListedAccess (Prelude.Maybe Prelude.Text)
listedAccess_homeDirectory = Lens.lens (\ListedAccess' {homeDirectory} -> homeDirectory) (\s@ListedAccess' {} a -> s {homeDirectory = a} :: ListedAccess)

instance Core.FromJSON ListedAccess where
  parseJSON =
    Core.withObject
      "ListedAccess"
      ( \x ->
          ListedAccess'
            Prelude.<$> (x Core..:? "HomeDirectoryType")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "ExternalId")
            Prelude.<*> (x Core..:? "HomeDirectory")
      )

instance Prelude.Hashable ListedAccess where
  hashWithSalt _salt ListedAccess' {..} =
    _salt `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` homeDirectory

instance Prelude.NFData ListedAccess where
  rnf ListedAccess' {..} =
    Prelude.rnf homeDirectoryType
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf homeDirectory
