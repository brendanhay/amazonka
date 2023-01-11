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
-- Module      : Amazonka.FSx.CreateStorageVirtualMachine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a storage virtual machine (SVM) for an Amazon FSx for ONTAP file
-- system.
module Amazonka.FSx.CreateStorageVirtualMachine
  ( -- * Creating a Request
    CreateStorageVirtualMachine (..),
    newCreateStorageVirtualMachine,

    -- * Request Lenses
    createStorageVirtualMachine_activeDirectoryConfiguration,
    createStorageVirtualMachine_clientRequestToken,
    createStorageVirtualMachine_rootVolumeSecurityStyle,
    createStorageVirtualMachine_svmAdminPassword,
    createStorageVirtualMachine_tags,
    createStorageVirtualMachine_fileSystemId,
    createStorageVirtualMachine_name,

    -- * Destructuring the Response
    CreateStorageVirtualMachineResponse (..),
    newCreateStorageVirtualMachineResponse,

    -- * Response Lenses
    createStorageVirtualMachineResponse_storageVirtualMachine,
    createStorageVirtualMachineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStorageVirtualMachine' smart constructor.
data CreateStorageVirtualMachine = CreateStorageVirtualMachine'
  { -- | Describes the self-managed Microsoft Active Directory to which you want
    -- to join the SVM. Joining an Active Directory provides user
    -- authentication and access control for SMB clients, including Microsoft
    -- Windows and macOS client accessing the file system.
    activeDirectoryConfiguration :: Prelude.Maybe CreateSvmActiveDirectoryConfiguration,
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The security style of the root volume of the SVM. Specify one of the
    -- following values:
    --
    -- -   @UNIX@ if the file system is managed by a UNIX administrator, the
    --     majority of users are NFS clients, and an application accessing the
    --     data uses a UNIX user as the service account.
    --
    -- -   @NTFS@ if the file system is managed by a Windows administrator, the
    --     majority of users are SMB clients, and an application accessing the
    --     data uses a Windows user as the service account.
    --
    -- -   @MIXED@ if the file system is managed by both UNIX and Windows
    --     administrators and users consist of both NFS and SMB clients.
    rootVolumeSecurityStyle :: Prelude.Maybe StorageVirtualMachineRootVolumeSecurityStyle,
    -- | The password to use when managing the SVM using the NetApp ONTAP CLI or
    -- REST API. If you do not specify a password, you can still use the file
    -- system\'s @fsxadmin@ user to manage the SVM.
    svmAdminPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    fileSystemId :: Prelude.Text,
    -- | The name of the SVM.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorageVirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryConfiguration', 'createStorageVirtualMachine_activeDirectoryConfiguration' - Describes the self-managed Microsoft Active Directory to which you want
-- to join the SVM. Joining an Active Directory provides user
-- authentication and access control for SMB clients, including Microsoft
-- Windows and macOS client accessing the file system.
--
-- 'clientRequestToken', 'createStorageVirtualMachine_clientRequestToken' - Undocumented member.
--
-- 'rootVolumeSecurityStyle', 'createStorageVirtualMachine_rootVolumeSecurityStyle' - The security style of the root volume of the SVM. Specify one of the
-- following values:
--
-- -   @UNIX@ if the file system is managed by a UNIX administrator, the
--     majority of users are NFS clients, and an application accessing the
--     data uses a UNIX user as the service account.
--
-- -   @NTFS@ if the file system is managed by a Windows administrator, the
--     majority of users are SMB clients, and an application accessing the
--     data uses a Windows user as the service account.
--
-- -   @MIXED@ if the file system is managed by both UNIX and Windows
--     administrators and users consist of both NFS and SMB clients.
--
-- 'svmAdminPassword', 'createStorageVirtualMachine_svmAdminPassword' - The password to use when managing the SVM using the NetApp ONTAP CLI or
-- REST API. If you do not specify a password, you can still use the file
-- system\'s @fsxadmin@ user to manage the SVM.
--
-- 'tags', 'createStorageVirtualMachine_tags' - Undocumented member.
--
-- 'fileSystemId', 'createStorageVirtualMachine_fileSystemId' - Undocumented member.
--
-- 'name', 'createStorageVirtualMachine_name' - The name of the SVM.
newCreateStorageVirtualMachine ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateStorageVirtualMachine
newCreateStorageVirtualMachine pFileSystemId_ pName_ =
  CreateStorageVirtualMachine'
    { activeDirectoryConfiguration =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      rootVolumeSecurityStyle = Prelude.Nothing,
      svmAdminPassword = Prelude.Nothing,
      tags = Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      name = pName_
    }

-- | Describes the self-managed Microsoft Active Directory to which you want
-- to join the SVM. Joining an Active Directory provides user
-- authentication and access control for SMB clients, including Microsoft
-- Windows and macOS client accessing the file system.
createStorageVirtualMachine_activeDirectoryConfiguration :: Lens.Lens' CreateStorageVirtualMachine (Prelude.Maybe CreateSvmActiveDirectoryConfiguration)
createStorageVirtualMachine_activeDirectoryConfiguration = Lens.lens (\CreateStorageVirtualMachine' {activeDirectoryConfiguration} -> activeDirectoryConfiguration) (\s@CreateStorageVirtualMachine' {} a -> s {activeDirectoryConfiguration = a} :: CreateStorageVirtualMachine)

-- | Undocumented member.
createStorageVirtualMachine_clientRequestToken :: Lens.Lens' CreateStorageVirtualMachine (Prelude.Maybe Prelude.Text)
createStorageVirtualMachine_clientRequestToken = Lens.lens (\CreateStorageVirtualMachine' {clientRequestToken} -> clientRequestToken) (\s@CreateStorageVirtualMachine' {} a -> s {clientRequestToken = a} :: CreateStorageVirtualMachine)

-- | The security style of the root volume of the SVM. Specify one of the
-- following values:
--
-- -   @UNIX@ if the file system is managed by a UNIX administrator, the
--     majority of users are NFS clients, and an application accessing the
--     data uses a UNIX user as the service account.
--
-- -   @NTFS@ if the file system is managed by a Windows administrator, the
--     majority of users are SMB clients, and an application accessing the
--     data uses a Windows user as the service account.
--
-- -   @MIXED@ if the file system is managed by both UNIX and Windows
--     administrators and users consist of both NFS and SMB clients.
createStorageVirtualMachine_rootVolumeSecurityStyle :: Lens.Lens' CreateStorageVirtualMachine (Prelude.Maybe StorageVirtualMachineRootVolumeSecurityStyle)
createStorageVirtualMachine_rootVolumeSecurityStyle = Lens.lens (\CreateStorageVirtualMachine' {rootVolumeSecurityStyle} -> rootVolumeSecurityStyle) (\s@CreateStorageVirtualMachine' {} a -> s {rootVolumeSecurityStyle = a} :: CreateStorageVirtualMachine)

-- | The password to use when managing the SVM using the NetApp ONTAP CLI or
-- REST API. If you do not specify a password, you can still use the file
-- system\'s @fsxadmin@ user to manage the SVM.
createStorageVirtualMachine_svmAdminPassword :: Lens.Lens' CreateStorageVirtualMachine (Prelude.Maybe Prelude.Text)
createStorageVirtualMachine_svmAdminPassword = Lens.lens (\CreateStorageVirtualMachine' {svmAdminPassword} -> svmAdminPassword) (\s@CreateStorageVirtualMachine' {} a -> s {svmAdminPassword = a} :: CreateStorageVirtualMachine) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
createStorageVirtualMachine_tags :: Lens.Lens' CreateStorageVirtualMachine (Prelude.Maybe (Prelude.NonEmpty Tag))
createStorageVirtualMachine_tags = Lens.lens (\CreateStorageVirtualMachine' {tags} -> tags) (\s@CreateStorageVirtualMachine' {} a -> s {tags = a} :: CreateStorageVirtualMachine) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createStorageVirtualMachine_fileSystemId :: Lens.Lens' CreateStorageVirtualMachine Prelude.Text
createStorageVirtualMachine_fileSystemId = Lens.lens (\CreateStorageVirtualMachine' {fileSystemId} -> fileSystemId) (\s@CreateStorageVirtualMachine' {} a -> s {fileSystemId = a} :: CreateStorageVirtualMachine)

-- | The name of the SVM.
createStorageVirtualMachine_name :: Lens.Lens' CreateStorageVirtualMachine Prelude.Text
createStorageVirtualMachine_name = Lens.lens (\CreateStorageVirtualMachine' {name} -> name) (\s@CreateStorageVirtualMachine' {} a -> s {name = a} :: CreateStorageVirtualMachine)

instance Core.AWSRequest CreateStorageVirtualMachine where
  type
    AWSResponse CreateStorageVirtualMachine =
      CreateStorageVirtualMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStorageVirtualMachineResponse'
            Prelude.<$> (x Data..?> "StorageVirtualMachine")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStorageVirtualMachine where
  hashWithSalt _salt CreateStorageVirtualMachine' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectoryConfiguration
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` rootVolumeSecurityStyle
      `Prelude.hashWithSalt` svmAdminPassword
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateStorageVirtualMachine where
  rnf CreateStorageVirtualMachine' {..} =
    Prelude.rnf activeDirectoryConfiguration
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf rootVolumeSecurityStyle
      `Prelude.seq` Prelude.rnf svmAdminPassword
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateStorageVirtualMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateStorageVirtualMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStorageVirtualMachine where
  toJSON CreateStorageVirtualMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveDirectoryConfiguration" Data..=)
              Prelude.<$> activeDirectoryConfiguration,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("RootVolumeSecurityStyle" Data..=)
              Prelude.<$> rootVolumeSecurityStyle,
            ("SvmAdminPassword" Data..=)
              Prelude.<$> svmAdminPassword,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateStorageVirtualMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStorageVirtualMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStorageVirtualMachineResponse' smart constructor.
data CreateStorageVirtualMachineResponse = CreateStorageVirtualMachineResponse'
  { -- | Returned after a successful @CreateStorageVirtualMachine@ operation;
    -- describes the SVM just created.
    storageVirtualMachine :: Prelude.Maybe StorageVirtualMachine,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorageVirtualMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageVirtualMachine', 'createStorageVirtualMachineResponse_storageVirtualMachine' - Returned after a successful @CreateStorageVirtualMachine@ operation;
-- describes the SVM just created.
--
-- 'httpStatus', 'createStorageVirtualMachineResponse_httpStatus' - The response's http status code.
newCreateStorageVirtualMachineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStorageVirtualMachineResponse
newCreateStorageVirtualMachineResponse pHttpStatus_ =
  CreateStorageVirtualMachineResponse'
    { storageVirtualMachine =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returned after a successful @CreateStorageVirtualMachine@ operation;
-- describes the SVM just created.
createStorageVirtualMachineResponse_storageVirtualMachine :: Lens.Lens' CreateStorageVirtualMachineResponse (Prelude.Maybe StorageVirtualMachine)
createStorageVirtualMachineResponse_storageVirtualMachine = Lens.lens (\CreateStorageVirtualMachineResponse' {storageVirtualMachine} -> storageVirtualMachine) (\s@CreateStorageVirtualMachineResponse' {} a -> s {storageVirtualMachine = a} :: CreateStorageVirtualMachineResponse)

-- | The response's http status code.
createStorageVirtualMachineResponse_httpStatus :: Lens.Lens' CreateStorageVirtualMachineResponse Prelude.Int
createStorageVirtualMachineResponse_httpStatus = Lens.lens (\CreateStorageVirtualMachineResponse' {httpStatus} -> httpStatus) (\s@CreateStorageVirtualMachineResponse' {} a -> s {httpStatus = a} :: CreateStorageVirtualMachineResponse)

instance
  Prelude.NFData
    CreateStorageVirtualMachineResponse
  where
  rnf CreateStorageVirtualMachineResponse' {..} =
    Prelude.rnf storageVirtualMachine
      `Prelude.seq` Prelude.rnf httpStatus
