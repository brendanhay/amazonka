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
-- Module      : Amazonka.DataSync.Types.FsxProtocolSmb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.FsxProtocolSmb where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.SmbMountOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Server Message Block (SMB) protocol configuration that
-- DataSync uses to access your Amazon FSx for NetApp ONTAP file system.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-ontap-location.html#create-ontap-location-access Accessing FSx for ONTAP file systems>.
--
-- /See:/ 'newFsxProtocolSmb' smart constructor.
data FsxProtocolSmb = FsxProtocolSmb'
  { -- | Specifies the fully qualified domain name (FQDN) of the Microsoft Active
    -- Directory that your storage virtual machine (SVM) belongs to.
    domain :: Prelude.Maybe Prelude.Text,
    mountOptions :: Prelude.Maybe SmbMountOptions,
    -- | Specifies the password of a user who has permission to access your SVM.
    password :: Data.Sensitive Prelude.Text,
    -- | Specifies a user name that can mount the location and access the files,
    -- folders, and metadata that you need in the SVM.
    --
    -- If you provide a user in your Active Directory, note the following:
    --
    -- -   If you\'re using Directory Service for Microsoft Active Directory,
    --     the user must be a member of the Amazon Web Services Delegated FSx
    --     Administrators group.
    --
    -- -   If you\'re using a self-managed Active Directory, the user must be a
    --     member of either the Domain Admins group or a custom group that you
    --     specified for file system administration when you created your file
    --     system.
    --
    -- Make sure that the user has the permissions it needs to copy the data
    -- you want:
    --
    -- -   @SE_TCB_NAME@: Required to set object ownership and file metadata.
    --     With this privilege, you also can copy NTFS discretionary access
    --     lists (DACLs).
    --
    -- -   @SE_SECURITY_NAME@: May be needed to copy NTFS system access control
    --     lists (SACLs). This operation specifically requires the Windows
    --     privilege, which is granted to members of the Domain Admins group.
    --     If you configure your task to copy SACLs, make sure that the user
    --     has the required privileges. For information about copying SACLs,
    --     see
    --     <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html#configure-ownership-and-permissions Ownership and permissions-related options>.
    user :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FsxProtocolSmb' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'fsxProtocolSmb_domain' - Specifies the fully qualified domain name (FQDN) of the Microsoft Active
-- Directory that your storage virtual machine (SVM) belongs to.
--
-- 'mountOptions', 'fsxProtocolSmb_mountOptions' - Undocumented member.
--
-- 'password', 'fsxProtocolSmb_password' - Specifies the password of a user who has permission to access your SVM.
--
-- 'user', 'fsxProtocolSmb_user' - Specifies a user name that can mount the location and access the files,
-- folders, and metadata that you need in the SVM.
--
-- If you provide a user in your Active Directory, note the following:
--
-- -   If you\'re using Directory Service for Microsoft Active Directory,
--     the user must be a member of the Amazon Web Services Delegated FSx
--     Administrators group.
--
-- -   If you\'re using a self-managed Active Directory, the user must be a
--     member of either the Domain Admins group or a custom group that you
--     specified for file system administration when you created your file
--     system.
--
-- Make sure that the user has the permissions it needs to copy the data
-- you want:
--
-- -   @SE_TCB_NAME@: Required to set object ownership and file metadata.
--     With this privilege, you also can copy NTFS discretionary access
--     lists (DACLs).
--
-- -   @SE_SECURITY_NAME@: May be needed to copy NTFS system access control
--     lists (SACLs). This operation specifically requires the Windows
--     privilege, which is granted to members of the Domain Admins group.
--     If you configure your task to copy SACLs, make sure that the user
--     has the required privileges. For information about copying SACLs,
--     see
--     <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html#configure-ownership-and-permissions Ownership and permissions-related options>.
newFsxProtocolSmb ::
  -- | 'password'
  Prelude.Text ->
  -- | 'user'
  Prelude.Text ->
  FsxProtocolSmb
newFsxProtocolSmb pPassword_ pUser_ =
  FsxProtocolSmb'
    { domain = Prelude.Nothing,
      mountOptions = Prelude.Nothing,
      password = Data._Sensitive Lens.# pPassword_,
      user = pUser_
    }

-- | Specifies the fully qualified domain name (FQDN) of the Microsoft Active
-- Directory that your storage virtual machine (SVM) belongs to.
fsxProtocolSmb_domain :: Lens.Lens' FsxProtocolSmb (Prelude.Maybe Prelude.Text)
fsxProtocolSmb_domain = Lens.lens (\FsxProtocolSmb' {domain} -> domain) (\s@FsxProtocolSmb' {} a -> s {domain = a} :: FsxProtocolSmb)

-- | Undocumented member.
fsxProtocolSmb_mountOptions :: Lens.Lens' FsxProtocolSmb (Prelude.Maybe SmbMountOptions)
fsxProtocolSmb_mountOptions = Lens.lens (\FsxProtocolSmb' {mountOptions} -> mountOptions) (\s@FsxProtocolSmb' {} a -> s {mountOptions = a} :: FsxProtocolSmb)

-- | Specifies the password of a user who has permission to access your SVM.
fsxProtocolSmb_password :: Lens.Lens' FsxProtocolSmb Prelude.Text
fsxProtocolSmb_password = Lens.lens (\FsxProtocolSmb' {password} -> password) (\s@FsxProtocolSmb' {} a -> s {password = a} :: FsxProtocolSmb) Prelude.. Data._Sensitive

-- | Specifies a user name that can mount the location and access the files,
-- folders, and metadata that you need in the SVM.
--
-- If you provide a user in your Active Directory, note the following:
--
-- -   If you\'re using Directory Service for Microsoft Active Directory,
--     the user must be a member of the Amazon Web Services Delegated FSx
--     Administrators group.
--
-- -   If you\'re using a self-managed Active Directory, the user must be a
--     member of either the Domain Admins group or a custom group that you
--     specified for file system administration when you created your file
--     system.
--
-- Make sure that the user has the permissions it needs to copy the data
-- you want:
--
-- -   @SE_TCB_NAME@: Required to set object ownership and file metadata.
--     With this privilege, you also can copy NTFS discretionary access
--     lists (DACLs).
--
-- -   @SE_SECURITY_NAME@: May be needed to copy NTFS system access control
--     lists (SACLs). This operation specifically requires the Windows
--     privilege, which is granted to members of the Domain Admins group.
--     If you configure your task to copy SACLs, make sure that the user
--     has the required privileges. For information about copying SACLs,
--     see
--     <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html#configure-ownership-and-permissions Ownership and permissions-related options>.
fsxProtocolSmb_user :: Lens.Lens' FsxProtocolSmb Prelude.Text
fsxProtocolSmb_user = Lens.lens (\FsxProtocolSmb' {user} -> user) (\s@FsxProtocolSmb' {} a -> s {user = a} :: FsxProtocolSmb)

instance Data.FromJSON FsxProtocolSmb where
  parseJSON =
    Data.withObject
      "FsxProtocolSmb"
      ( \x ->
          FsxProtocolSmb'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "MountOptions")
            Prelude.<*> (x Data..: "Password")
            Prelude.<*> (x Data..: "User")
      )

instance Prelude.Hashable FsxProtocolSmb where
  hashWithSalt _salt FsxProtocolSmb' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` mountOptions
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` user

instance Prelude.NFData FsxProtocolSmb where
  rnf FsxProtocolSmb' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf mountOptions
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf user

instance Data.ToJSON FsxProtocolSmb where
  toJSON FsxProtocolSmb' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            ("MountOptions" Data..=) Prelude.<$> mountOptions,
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("User" Data..= user)
          ]
      )
