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
-- Module      : Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- <http://aws.amazon.com/directoryservice/pricing/ AWS Directory Services pricing terms>.
--
-- To delete empty directories, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/delete-workspaces-directory.html Delete the Directory for Your WorkSpaces>.
-- If you delete your Simple AD or AD Connector directory, you can always
-- create a new one when you want to start using WorkSpaces again.
module Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDeregisterWorkspaceDirectory' smart constructor.
data DeregisterWorkspaceDirectory = DeregisterWorkspaceDirectory'
  { -- | The identifier of the directory. If any WorkSpaces are registered to
    -- this directory, you must remove them before you deregister the
    -- directory, or you will receive an OperationNotSupportedException error.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeregisterWorkspaceDirectory
newDeregisterWorkspaceDirectory pDirectoryId_ =
  DeregisterWorkspaceDirectory'
    { directoryId =
        pDirectoryId_
    }

-- | The identifier of the directory. If any WorkSpaces are registered to
-- this directory, you must remove them before you deregister the
-- directory, or you will receive an OperationNotSupportedException error.
deregisterWorkspaceDirectory_directoryId :: Lens.Lens' DeregisterWorkspaceDirectory Core.Text
deregisterWorkspaceDirectory_directoryId = Lens.lens (\DeregisterWorkspaceDirectory' {directoryId} -> directoryId) (\s@DeregisterWorkspaceDirectory' {} a -> s {directoryId = a} :: DeregisterWorkspaceDirectory)

instance Core.AWSRequest DeregisterWorkspaceDirectory where
  type
    AWSResponse DeregisterWorkspaceDirectory =
      DeregisterWorkspaceDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWorkspaceDirectoryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterWorkspaceDirectory

instance Core.NFData DeregisterWorkspaceDirectory

instance Core.ToHeaders DeregisterWorkspaceDirectory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DeregisterWorkspaceDirectory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterWorkspaceDirectory where
  toJSON DeregisterWorkspaceDirectory' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath DeregisterWorkspaceDirectory where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterWorkspaceDirectory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterWorkspaceDirectoryResponse' smart constructor.
data DeregisterWorkspaceDirectoryResponse = DeregisterWorkspaceDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeregisterWorkspaceDirectoryResponse
newDeregisterWorkspaceDirectoryResponse pHttpStatus_ =
  DeregisterWorkspaceDirectoryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterWorkspaceDirectoryResponse_httpStatus :: Lens.Lens' DeregisterWorkspaceDirectoryResponse Core.Int
deregisterWorkspaceDirectoryResponse_httpStatus = Lens.lens (\DeregisterWorkspaceDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeregisterWorkspaceDirectoryResponse' {} a -> s {httpStatus = a} :: DeregisterWorkspaceDirectoryResponse)

instance
  Core.NFData
    DeregisterWorkspaceDirectoryResponse
