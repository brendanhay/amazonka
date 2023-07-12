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
-- Module      : Amazonka.DirectoryService.CreateDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Simple AD directory. For more information, see
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_simple_ad.html Simple Active Directory>
-- in the /Directory Service Admin Guide/.
--
-- Before you call @CreateDirectory@, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the @CreateDirectory@
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Amazonka.DirectoryService.CreateDirectory
  ( -- * Creating a Request
    CreateDirectory (..),
    newCreateDirectory,

    -- * Request Lenses
    createDirectory_description,
    createDirectory_shortName,
    createDirectory_tags,
    createDirectory_vpcSettings,
    createDirectory_name,
    createDirectory_password,
    createDirectory_size,

    -- * Destructuring the Response
    CreateDirectoryResponse (..),
    newCreateDirectoryResponse,

    -- * Response Lenses
    createDirectoryResponse_directoryId,
    createDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the CreateDirectory operation.
--
-- /See:/ 'newCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { -- | A description for the directory.
    description :: Prelude.Maybe Prelude.Text,
    -- | The NetBIOS name of the directory, such as @CORP@.
    shortName :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to the Simple AD directory.
    tags :: Prelude.Maybe [Tag],
    -- | A DirectoryVpcSettings object that contains additional information for
    -- the operation.
    vpcSettings :: Prelude.Maybe DirectoryVpcSettings,
    -- | The fully qualified name for the directory, such as @corp.example.com@.
    name :: Prelude.Text,
    -- | The password for the directory administrator. The directory creation
    -- process creates a directory administrator account with the user name
    -- @Administrator@ and this password.
    --
    -- If you need to change the password for the administrator account, you
    -- can use the ResetUserPassword API call.
    --
    -- The regex pattern for this string is made up of the following
    -- conditions:
    --
    -- -   Length (?=^.{8,64}$) – Must be between 8 and 64 characters
    --
    -- AND any 3 of the following password complexity rules required by Active
    -- Directory:
    --
    -- -   Numbers and upper case and lowercase (?=.*\\d)(?=.*[A-Z])(?=.*[a-z])
    --
    -- -   Numbers and special characters and lower case
    --     (?=.*\\d)(?=.*[^A-Za-z0-9\\s])(?=.*[a-z])
    --
    -- -   Special characters and upper case and lower case
    --     (?=.*[^A-Za-z0-9\\s])(?=.*[A-Z])(?=.*[a-z])
    --
    -- -   Numbers and upper case and special characters
    --     (?=.*\\d)(?=.*[A-Z])(?=.*[^A-Za-z0-9\\s])
    --
    -- For additional information about how Active Directory passwords are
    -- enforced, see
    -- <https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/password-must-meet-complexity-requirements Password must meet complexity requirements>
    -- on the Microsoft website.
    password :: Data.Sensitive Prelude.Text,
    -- | The size of the directory.
    size :: DirectorySize
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDirectory_description' - A description for the directory.
--
-- 'shortName', 'createDirectory_shortName' - The NetBIOS name of the directory, such as @CORP@.
--
-- 'tags', 'createDirectory_tags' - The tags to be assigned to the Simple AD directory.
--
-- 'vpcSettings', 'createDirectory_vpcSettings' - A DirectoryVpcSettings object that contains additional information for
-- the operation.
--
-- 'name', 'createDirectory_name' - The fully qualified name for the directory, such as @corp.example.com@.
--
-- 'password', 'createDirectory_password' - The password for the directory administrator. The directory creation
-- process creates a directory administrator account with the user name
-- @Administrator@ and this password.
--
-- If you need to change the password for the administrator account, you
-- can use the ResetUserPassword API call.
--
-- The regex pattern for this string is made up of the following
-- conditions:
--
-- -   Length (?=^.{8,64}$) – Must be between 8 and 64 characters
--
-- AND any 3 of the following password complexity rules required by Active
-- Directory:
--
-- -   Numbers and upper case and lowercase (?=.*\\d)(?=.*[A-Z])(?=.*[a-z])
--
-- -   Numbers and special characters and lower case
--     (?=.*\\d)(?=.*[^A-Za-z0-9\\s])(?=.*[a-z])
--
-- -   Special characters and upper case and lower case
--     (?=.*[^A-Za-z0-9\\s])(?=.*[A-Z])(?=.*[a-z])
--
-- -   Numbers and upper case and special characters
--     (?=.*\\d)(?=.*[A-Z])(?=.*[^A-Za-z0-9\\s])
--
-- For additional information about how Active Directory passwords are
-- enforced, see
-- <https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/password-must-meet-complexity-requirements Password must meet complexity requirements>
-- on the Microsoft website.
--
-- 'size', 'createDirectory_size' - The size of the directory.
newCreateDirectory ::
  -- | 'name'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'size'
  DirectorySize ->
  CreateDirectory
newCreateDirectory pName_ pPassword_ pSize_ =
  CreateDirectory'
    { description = Prelude.Nothing,
      shortName = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcSettings = Prelude.Nothing,
      name = pName_,
      password = Data._Sensitive Lens.# pPassword_,
      size = pSize_
    }

-- | A description for the directory.
createDirectory_description :: Lens.Lens' CreateDirectory (Prelude.Maybe Prelude.Text)
createDirectory_description = Lens.lens (\CreateDirectory' {description} -> description) (\s@CreateDirectory' {} a -> s {description = a} :: CreateDirectory)

-- | The NetBIOS name of the directory, such as @CORP@.
createDirectory_shortName :: Lens.Lens' CreateDirectory (Prelude.Maybe Prelude.Text)
createDirectory_shortName = Lens.lens (\CreateDirectory' {shortName} -> shortName) (\s@CreateDirectory' {} a -> s {shortName = a} :: CreateDirectory)

-- | The tags to be assigned to the Simple AD directory.
createDirectory_tags :: Lens.Lens' CreateDirectory (Prelude.Maybe [Tag])
createDirectory_tags = Lens.lens (\CreateDirectory' {tags} -> tags) (\s@CreateDirectory' {} a -> s {tags = a} :: CreateDirectory) Prelude.. Lens.mapping Lens.coerced

-- | A DirectoryVpcSettings object that contains additional information for
-- the operation.
createDirectory_vpcSettings :: Lens.Lens' CreateDirectory (Prelude.Maybe DirectoryVpcSettings)
createDirectory_vpcSettings = Lens.lens (\CreateDirectory' {vpcSettings} -> vpcSettings) (\s@CreateDirectory' {} a -> s {vpcSettings = a} :: CreateDirectory)

-- | The fully qualified name for the directory, such as @corp.example.com@.
createDirectory_name :: Lens.Lens' CreateDirectory Prelude.Text
createDirectory_name = Lens.lens (\CreateDirectory' {name} -> name) (\s@CreateDirectory' {} a -> s {name = a} :: CreateDirectory)

-- | The password for the directory administrator. The directory creation
-- process creates a directory administrator account with the user name
-- @Administrator@ and this password.
--
-- If you need to change the password for the administrator account, you
-- can use the ResetUserPassword API call.
--
-- The regex pattern for this string is made up of the following
-- conditions:
--
-- -   Length (?=^.{8,64}$) – Must be between 8 and 64 characters
--
-- AND any 3 of the following password complexity rules required by Active
-- Directory:
--
-- -   Numbers and upper case and lowercase (?=.*\\d)(?=.*[A-Z])(?=.*[a-z])
--
-- -   Numbers and special characters and lower case
--     (?=.*\\d)(?=.*[^A-Za-z0-9\\s])(?=.*[a-z])
--
-- -   Special characters and upper case and lower case
--     (?=.*[^A-Za-z0-9\\s])(?=.*[A-Z])(?=.*[a-z])
--
-- -   Numbers and upper case and special characters
--     (?=.*\\d)(?=.*[A-Z])(?=.*[^A-Za-z0-9\\s])
--
-- For additional information about how Active Directory passwords are
-- enforced, see
-- <https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/password-must-meet-complexity-requirements Password must meet complexity requirements>
-- on the Microsoft website.
createDirectory_password :: Lens.Lens' CreateDirectory Prelude.Text
createDirectory_password = Lens.lens (\CreateDirectory' {password} -> password) (\s@CreateDirectory' {} a -> s {password = a} :: CreateDirectory) Prelude.. Data._Sensitive

-- | The size of the directory.
createDirectory_size :: Lens.Lens' CreateDirectory DirectorySize
createDirectory_size = Lens.lens (\CreateDirectory' {size} -> size) (\s@CreateDirectory' {} a -> s {size = a} :: CreateDirectory)

instance Core.AWSRequest CreateDirectory where
  type
    AWSResponse CreateDirectory =
      CreateDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryResponse'
            Prelude.<$> (x Data..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDirectory where
  hashWithSalt _salt CreateDirectory' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` shortName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSettings
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` size

instance Prelude.NFData CreateDirectory where
  rnf CreateDirectory' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf shortName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSettings
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf size

instance Data.ToHeaders CreateDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDirectory where
  toJSON CreateDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ShortName" Data..=) Prelude.<$> shortName,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcSettings" Data..=) Prelude.<$> vpcSettings,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("Size" Data..= size)
          ]
      )

instance Data.ToPath CreateDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the CreateDirectory operation.
--
-- /See:/ 'newCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { -- | The identifier of the directory that was created.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createDirectoryResponse_directoryId' - The identifier of the directory that was created.
--
-- 'httpStatus', 'createDirectoryResponse_httpStatus' - The response's http status code.
newCreateDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectoryResponse
newCreateDirectoryResponse pHttpStatus_ =
  CreateDirectoryResponse'
    { directoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory that was created.
createDirectoryResponse_directoryId :: Lens.Lens' CreateDirectoryResponse (Prelude.Maybe Prelude.Text)
createDirectoryResponse_directoryId = Lens.lens (\CreateDirectoryResponse' {directoryId} -> directoryId) (\s@CreateDirectoryResponse' {} a -> s {directoryId = a} :: CreateDirectoryResponse)

-- | The response's http status code.
createDirectoryResponse_httpStatus :: Lens.Lens' CreateDirectoryResponse Prelude.Int
createDirectoryResponse_httpStatus = Lens.lens (\CreateDirectoryResponse' {httpStatus} -> httpStatus) (\s@CreateDirectoryResponse' {} a -> s {httpStatus = a} :: CreateDirectoryResponse)

instance Prelude.NFData CreateDirectoryResponse where
  rnf CreateDirectoryResponse' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf httpStatus
