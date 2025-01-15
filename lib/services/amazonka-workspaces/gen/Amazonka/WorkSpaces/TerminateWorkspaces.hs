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
-- Module      : Amazonka.WorkSpaces.TerminateWorkspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified WorkSpaces.
--
-- Terminating a WorkSpace is a permanent action and cannot be undone. The
-- user\'s data is destroyed. If you need to archive any user data, contact
-- Amazon Web Services Support before terminating the WorkSpace.
--
-- You can terminate a WorkSpace that is in any state except @SUSPENDED@.
--
-- This operation is asynchronous and returns before the WorkSpaces have
-- been completely terminated. After a WorkSpace is terminated, the
-- @TERMINATED@ state is returned only briefly before the WorkSpace
-- directory metadata is cleaned up, so this state is rarely returned. To
-- confirm that a WorkSpace is terminated, check for the WorkSpace ID by
-- using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces>.
-- If the WorkSpace ID isn\'t returned, then the WorkSpace has been
-- successfully terminated.
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
module Amazonka.WorkSpaces.TerminateWorkspaces
  ( -- * Creating a Request
    TerminateWorkspaces (..),
    newTerminateWorkspaces,

    -- * Request Lenses
    terminateWorkspaces_terminateWorkspaceRequests,

    -- * Destructuring the Response
    TerminateWorkspacesResponse (..),
    newTerminateWorkspacesResponse,

    -- * Response Lenses
    terminateWorkspacesResponse_failedRequests,
    terminateWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newTerminateWorkspaces' smart constructor.
data TerminateWorkspaces = TerminateWorkspaces'
  { -- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
    terminateWorkspaceRequests :: Prelude.NonEmpty TerminateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminateWorkspaceRequests', 'terminateWorkspaces_terminateWorkspaceRequests' - The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
newTerminateWorkspaces ::
  -- | 'terminateWorkspaceRequests'
  Prelude.NonEmpty TerminateRequest ->
  TerminateWorkspaces
newTerminateWorkspaces pTerminateWorkspaceRequests_ =
  TerminateWorkspaces'
    { terminateWorkspaceRequests =
        Lens.coerced Lens.# pTerminateWorkspaceRequests_
    }

-- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
terminateWorkspaces_terminateWorkspaceRequests :: Lens.Lens' TerminateWorkspaces (Prelude.NonEmpty TerminateRequest)
terminateWorkspaces_terminateWorkspaceRequests = Lens.lens (\TerminateWorkspaces' {terminateWorkspaceRequests} -> terminateWorkspaceRequests) (\s@TerminateWorkspaces' {} a -> s {terminateWorkspaceRequests = a} :: TerminateWorkspaces) Prelude.. Lens.coerced

instance Core.AWSRequest TerminateWorkspaces where
  type
    AWSResponse TerminateWorkspaces =
      TerminateWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateWorkspacesResponse'
            Prelude.<$> (x Data..?> "FailedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateWorkspaces where
  hashWithSalt _salt TerminateWorkspaces' {..} =
    _salt
      `Prelude.hashWithSalt` terminateWorkspaceRequests

instance Prelude.NFData TerminateWorkspaces where
  rnf TerminateWorkspaces' {..} =
    Prelude.rnf terminateWorkspaceRequests

instance Data.ToHeaders TerminateWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.TerminateWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateWorkspaces where
  toJSON TerminateWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TerminateWorkspaceRequests"
                  Data..= terminateWorkspaceRequests
              )
          ]
      )

instance Data.ToPath TerminateWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateWorkspacesResponse' smart constructor.
data TerminateWorkspacesResponse = TerminateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be terminated.
    failedRequests :: Prelude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'terminateWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be terminated.
--
-- 'httpStatus', 'terminateWorkspacesResponse_httpStatus' - The response's http status code.
newTerminateWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateWorkspacesResponse
newTerminateWorkspacesResponse pHttpStatus_ =
  TerminateWorkspacesResponse'
    { failedRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be terminated.
terminateWorkspacesResponse_failedRequests :: Lens.Lens' TerminateWorkspacesResponse (Prelude.Maybe [FailedWorkspaceChangeRequest])
terminateWorkspacesResponse_failedRequests = Lens.lens (\TerminateWorkspacesResponse' {failedRequests} -> failedRequests) (\s@TerminateWorkspacesResponse' {} a -> s {failedRequests = a} :: TerminateWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
terminateWorkspacesResponse_httpStatus :: Lens.Lens' TerminateWorkspacesResponse Prelude.Int
terminateWorkspacesResponse_httpStatus = Lens.lens (\TerminateWorkspacesResponse' {httpStatus} -> httpStatus) (\s@TerminateWorkspacesResponse' {} a -> s {httpStatus = a} :: TerminateWorkspacesResponse)

instance Prelude.NFData TerminateWorkspacesResponse where
  rnf TerminateWorkspacesResponse' {..} =
    Prelude.rnf failedRequests `Prelude.seq`
      Prelude.rnf httpStatus
