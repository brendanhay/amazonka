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
-- Module      : Amazonka.WorkSpaces.RegisterWorkspaceDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified directory. This operation is asynchronous and
-- returns before the WorkSpace directory is registered. If this is the
-- first time you are registering a directory, you will need to create the
-- workspaces_DefaultRole role before you can register a directory. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role>.
module Amazonka.WorkSpaces.RegisterWorkspaceDirectory
  ( -- * Creating a Request
    RegisterWorkspaceDirectory (..),
    newRegisterWorkspaceDirectory,

    -- * Request Lenses
    registerWorkspaceDirectory_enableSelfService,
    registerWorkspaceDirectory_subnetIds,
    registerWorkspaceDirectory_tags,
    registerWorkspaceDirectory_tenancy,
    registerWorkspaceDirectory_directoryId,
    registerWorkspaceDirectory_enableWorkDocs,

    -- * Destructuring the Response
    RegisterWorkspaceDirectoryResponse (..),
    newRegisterWorkspaceDirectoryResponse,

    -- * Response Lenses
    registerWorkspaceDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newRegisterWorkspaceDirectory' smart constructor.
data RegisterWorkspaceDirectory = RegisterWorkspaceDirectory'
  { -- | Indicates whether self-service capabilities are enabled or disabled.
    enableSelfService :: Prelude.Maybe Prelude.Bool,
    -- | The identifiers of the subnets for your virtual private cloud (VPC).
    -- Make sure that the subnets are in supported Availability Zones. The
    -- subnets must also be in separate Availability Zones. If these conditions
    -- are not met, you will receive an OperationNotSupportedException error.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags associated with the directory.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether your WorkSpace directory is dedicated or shared. To
    -- use Bring Your Own License (BYOL) images, this value must be set to
    -- @DEDICATED@ and your Amazon Web Services account must be enabled for
    -- BYOL. If your account has not been enabled for BYOL, you will receive an
    -- InvalidParameterValuesException error. For more information about BYOL
    -- images, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The identifier of the directory. You cannot register a directory if it
    -- does not have a status of Active. If the directory does not have a
    -- status of Active, you will receive an InvalidResourceStateException
    -- error. If you have already registered the maximum number of directories
    -- that you can register with Amazon WorkSpaces, you will receive a
    -- ResourceLimitExceededException error. Deregister directories that you
    -- are not using for WorkSpaces, and try again.
    directoryId :: Prelude.Text,
    -- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have
    -- enabled this parameter and WorkDocs is not available in the Region, you
    -- will receive an OperationNotSupportedException error. Set
    -- @EnableWorkDocs@ to disabled, and try again.
    enableWorkDocs :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterWorkspaceDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableSelfService', 'registerWorkspaceDirectory_enableSelfService' - Indicates whether self-service capabilities are enabled or disabled.
--
-- 'subnetIds', 'registerWorkspaceDirectory_subnetIds' - The identifiers of the subnets for your virtual private cloud (VPC).
-- Make sure that the subnets are in supported Availability Zones. The
-- subnets must also be in separate Availability Zones. If these conditions
-- are not met, you will receive an OperationNotSupportedException error.
--
-- 'tags', 'registerWorkspaceDirectory_tags' - The tags associated with the directory.
--
-- 'tenancy', 'registerWorkspaceDirectory_tenancy' - Indicates whether your WorkSpace directory is dedicated or shared. To
-- use Bring Your Own License (BYOL) images, this value must be set to
-- @DEDICATED@ and your Amazon Web Services account must be enabled for
-- BYOL. If your account has not been enabled for BYOL, you will receive an
-- InvalidParameterValuesException error. For more information about BYOL
-- images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
--
-- 'directoryId', 'registerWorkspaceDirectory_directoryId' - The identifier of the directory. You cannot register a directory if it
-- does not have a status of Active. If the directory does not have a
-- status of Active, you will receive an InvalidResourceStateException
-- error. If you have already registered the maximum number of directories
-- that you can register with Amazon WorkSpaces, you will receive a
-- ResourceLimitExceededException error. Deregister directories that you
-- are not using for WorkSpaces, and try again.
--
-- 'enableWorkDocs', 'registerWorkspaceDirectory_enableWorkDocs' - Indicates whether Amazon WorkDocs is enabled or disabled. If you have
-- enabled this parameter and WorkDocs is not available in the Region, you
-- will receive an OperationNotSupportedException error. Set
-- @EnableWorkDocs@ to disabled, and try again.
newRegisterWorkspaceDirectory ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'enableWorkDocs'
  Prelude.Bool ->
  RegisterWorkspaceDirectory
newRegisterWorkspaceDirectory
  pDirectoryId_
  pEnableWorkDocs_ =
    RegisterWorkspaceDirectory'
      { enableSelfService =
          Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        tenancy = Prelude.Nothing,
        directoryId = pDirectoryId_,
        enableWorkDocs = pEnableWorkDocs_
      }

-- | Indicates whether self-service capabilities are enabled or disabled.
registerWorkspaceDirectory_enableSelfService :: Lens.Lens' RegisterWorkspaceDirectory (Prelude.Maybe Prelude.Bool)
registerWorkspaceDirectory_enableSelfService = Lens.lens (\RegisterWorkspaceDirectory' {enableSelfService} -> enableSelfService) (\s@RegisterWorkspaceDirectory' {} a -> s {enableSelfService = a} :: RegisterWorkspaceDirectory)

-- | The identifiers of the subnets for your virtual private cloud (VPC).
-- Make sure that the subnets are in supported Availability Zones. The
-- subnets must also be in separate Availability Zones. If these conditions
-- are not met, you will receive an OperationNotSupportedException error.
registerWorkspaceDirectory_subnetIds :: Lens.Lens' RegisterWorkspaceDirectory (Prelude.Maybe [Prelude.Text])
registerWorkspaceDirectory_subnetIds = Lens.lens (\RegisterWorkspaceDirectory' {subnetIds} -> subnetIds) (\s@RegisterWorkspaceDirectory' {} a -> s {subnetIds = a} :: RegisterWorkspaceDirectory) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the directory.
registerWorkspaceDirectory_tags :: Lens.Lens' RegisterWorkspaceDirectory (Prelude.Maybe [Tag])
registerWorkspaceDirectory_tags = Lens.lens (\RegisterWorkspaceDirectory' {tags} -> tags) (\s@RegisterWorkspaceDirectory' {} a -> s {tags = a} :: RegisterWorkspaceDirectory) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether your WorkSpace directory is dedicated or shared. To
-- use Bring Your Own License (BYOL) images, this value must be set to
-- @DEDICATED@ and your Amazon Web Services account must be enabled for
-- BYOL. If your account has not been enabled for BYOL, you will receive an
-- InvalidParameterValuesException error. For more information about BYOL
-- images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
registerWorkspaceDirectory_tenancy :: Lens.Lens' RegisterWorkspaceDirectory (Prelude.Maybe Tenancy)
registerWorkspaceDirectory_tenancy = Lens.lens (\RegisterWorkspaceDirectory' {tenancy} -> tenancy) (\s@RegisterWorkspaceDirectory' {} a -> s {tenancy = a} :: RegisterWorkspaceDirectory)

-- | The identifier of the directory. You cannot register a directory if it
-- does not have a status of Active. If the directory does not have a
-- status of Active, you will receive an InvalidResourceStateException
-- error. If you have already registered the maximum number of directories
-- that you can register with Amazon WorkSpaces, you will receive a
-- ResourceLimitExceededException error. Deregister directories that you
-- are not using for WorkSpaces, and try again.
registerWorkspaceDirectory_directoryId :: Lens.Lens' RegisterWorkspaceDirectory Prelude.Text
registerWorkspaceDirectory_directoryId = Lens.lens (\RegisterWorkspaceDirectory' {directoryId} -> directoryId) (\s@RegisterWorkspaceDirectory' {} a -> s {directoryId = a} :: RegisterWorkspaceDirectory)

-- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have
-- enabled this parameter and WorkDocs is not available in the Region, you
-- will receive an OperationNotSupportedException error. Set
-- @EnableWorkDocs@ to disabled, and try again.
registerWorkspaceDirectory_enableWorkDocs :: Lens.Lens' RegisterWorkspaceDirectory Prelude.Bool
registerWorkspaceDirectory_enableWorkDocs = Lens.lens (\RegisterWorkspaceDirectory' {enableWorkDocs} -> enableWorkDocs) (\s@RegisterWorkspaceDirectory' {} a -> s {enableWorkDocs = a} :: RegisterWorkspaceDirectory)

instance Core.AWSRequest RegisterWorkspaceDirectory where
  type
    AWSResponse RegisterWorkspaceDirectory =
      RegisterWorkspaceDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterWorkspaceDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterWorkspaceDirectory where
  hashWithSalt _salt RegisterWorkspaceDirectory' {..} =
    _salt
      `Prelude.hashWithSalt` enableSelfService
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` enableWorkDocs

instance Prelude.NFData RegisterWorkspaceDirectory where
  rnf RegisterWorkspaceDirectory' {..} =
    Prelude.rnf enableSelfService
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf enableWorkDocs

instance Data.ToHeaders RegisterWorkspaceDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.RegisterWorkspaceDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterWorkspaceDirectory where
  toJSON RegisterWorkspaceDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableSelfService" Data..=)
              Prelude.<$> enableSelfService,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Tenancy" Data..=) Prelude.<$> tenancy,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("EnableWorkDocs" Data..= enableWorkDocs)
          ]
      )

instance Data.ToPath RegisterWorkspaceDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterWorkspaceDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterWorkspaceDirectoryResponse' smart constructor.
data RegisterWorkspaceDirectoryResponse = RegisterWorkspaceDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterWorkspaceDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerWorkspaceDirectoryResponse_httpStatus' - The response's http status code.
newRegisterWorkspaceDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterWorkspaceDirectoryResponse
newRegisterWorkspaceDirectoryResponse pHttpStatus_ =
  RegisterWorkspaceDirectoryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerWorkspaceDirectoryResponse_httpStatus :: Lens.Lens' RegisterWorkspaceDirectoryResponse Prelude.Int
registerWorkspaceDirectoryResponse_httpStatus = Lens.lens (\RegisterWorkspaceDirectoryResponse' {httpStatus} -> httpStatus) (\s@RegisterWorkspaceDirectoryResponse' {} a -> s {httpStatus = a} :: RegisterWorkspaceDirectoryResponse)

instance
  Prelude.NFData
    RegisterWorkspaceDirectoryResponse
  where
  rnf RegisterWorkspaceDirectoryResponse' {..} =
    Prelude.rnf httpStatus
