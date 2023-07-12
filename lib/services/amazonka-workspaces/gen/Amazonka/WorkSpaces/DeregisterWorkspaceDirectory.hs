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
-- Module      : Amazonka.WorkSpaces.DeregisterWorkspaceDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified directory. This operation is asynchronous and
-- returns before the WorkSpace directory is deregistered. If any
-- WorkSpaces are registered to this directory, you must remove them before
-- you can deregister the directory.
--
-- Simple AD and AD Connector are made available to you free of charge to
-- use with WorkSpaces. If there are no WorkSpaces being used with your
-- Simple AD or AD Connector directory for 30 consecutive days, this
-- directory will be automatically deregistered for use with Amazon
-- WorkSpaces, and you will be charged for this directory as per the
-- <http://aws.amazon.com/directoryservice/pricing/ Directory Service pricing terms>.
--
-- To delete empty directories, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/delete-workspaces-directory.html Delete the Directory for Your WorkSpaces>.
-- If you delete your Simple AD or AD Connector directory, you can always
-- create a new one when you want to start using WorkSpaces again.
module Amazonka.WorkSpaces.DeregisterWorkspaceDirectory
  ( -- * Creating a Request
    DeregisterWorkspaceDirectory (..),
    newDeregisterWorkspaceDirectory,

    -- * Request Lenses
    deregisterWorkspaceDirectory_directoryId,

    -- * Destructuring the Response
    DeregisterWorkspaceDirectoryResponse (..),
    newDeregisterWorkspaceDirectoryResponse,

    -- * Response Lenses
    deregisterWorkspaceDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDeregisterWorkspaceDirectory' smart constructor.
data DeregisterWorkspaceDirectory = DeregisterWorkspaceDirectory'
  { -- | The identifier of the directory. If any WorkSpaces are registered to
    -- this directory, you must remove them before you deregister the
    -- directory, or you will receive an OperationNotSupportedException error.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterWorkspaceDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deregisterWorkspaceDirectory_directoryId' - The identifier of the directory. If any WorkSpaces are registered to
-- this directory, you must remove them before you deregister the
-- directory, or you will receive an OperationNotSupportedException error.
newDeregisterWorkspaceDirectory ::
  -- | 'directoryId'
  Prelude.Text ->
  DeregisterWorkspaceDirectory
newDeregisterWorkspaceDirectory pDirectoryId_ =
  DeregisterWorkspaceDirectory'
    { directoryId =
        pDirectoryId_
    }

-- | The identifier of the directory. If any WorkSpaces are registered to
-- this directory, you must remove them before you deregister the
-- directory, or you will receive an OperationNotSupportedException error.
deregisterWorkspaceDirectory_directoryId :: Lens.Lens' DeregisterWorkspaceDirectory Prelude.Text
deregisterWorkspaceDirectory_directoryId = Lens.lens (\DeregisterWorkspaceDirectory' {directoryId} -> directoryId) (\s@DeregisterWorkspaceDirectory' {} a -> s {directoryId = a} :: DeregisterWorkspaceDirectory)

instance Core.AWSRequest DeregisterWorkspaceDirectory where
  type
    AWSResponse DeregisterWorkspaceDirectory =
      DeregisterWorkspaceDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWorkspaceDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterWorkspaceDirectory
  where
  hashWithSalt _salt DeregisterWorkspaceDirectory' {..} =
    _salt `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DeregisterWorkspaceDirectory where
  rnf DeregisterWorkspaceDirectory' {..} =
    Prelude.rnf directoryId

instance Data.ToHeaders DeregisterWorkspaceDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DeregisterWorkspaceDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterWorkspaceDirectory where
  toJSON DeregisterWorkspaceDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DirectoryId" Data..= directoryId)]
      )

instance Data.ToPath DeregisterWorkspaceDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterWorkspaceDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterWorkspaceDirectoryResponse' smart constructor.
data DeregisterWorkspaceDirectoryResponse = DeregisterWorkspaceDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterWorkspaceDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterWorkspaceDirectoryResponse_httpStatus' - The response's http status code.
newDeregisterWorkspaceDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterWorkspaceDirectoryResponse
newDeregisterWorkspaceDirectoryResponse pHttpStatus_ =
  DeregisterWorkspaceDirectoryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterWorkspaceDirectoryResponse_httpStatus :: Lens.Lens' DeregisterWorkspaceDirectoryResponse Prelude.Int
deregisterWorkspaceDirectoryResponse_httpStatus = Lens.lens (\DeregisterWorkspaceDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeregisterWorkspaceDirectoryResponse' {} a -> s {httpStatus = a} :: DeregisterWorkspaceDirectoryResponse)

instance
  Prelude.NFData
    DeregisterWorkspaceDirectoryResponse
  where
  rnf DeregisterWorkspaceDirectoryResponse' {..} =
    Prelude.rnf httpStatus
