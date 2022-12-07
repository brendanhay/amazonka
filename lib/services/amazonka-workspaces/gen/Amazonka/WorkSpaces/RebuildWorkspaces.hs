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
-- Module      : Amazonka.WorkSpaces.RebuildWorkspaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rebuilds the specified WorkSpace.
--
-- You cannot rebuild a WorkSpace unless its state is @AVAILABLE@, @ERROR@,
-- @UNHEALTHY@, @STOPPED@, or @REBOOTING@.
--
-- Rebuilding a WorkSpace is a potentially destructive action that can
-- result in the loss of data. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html Rebuild a WorkSpace>.
--
-- This operation is asynchronous and returns before the WorkSpaces have
-- been completely rebuilt.
module Amazonka.WorkSpaces.RebuildWorkspaces
  ( -- * Creating a Request
    RebuildWorkspaces (..),
    newRebuildWorkspaces,

    -- * Request Lenses
    rebuildWorkspaces_rebuildWorkspaceRequests,

    -- * Destructuring the Response
    RebuildWorkspacesResponse (..),
    newRebuildWorkspacesResponse,

    -- * Response Lenses
    rebuildWorkspacesResponse_failedRequests,
    rebuildWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newRebuildWorkspaces' smart constructor.
data RebuildWorkspaces = RebuildWorkspaces'
  { -- | The WorkSpace to rebuild. You can specify a single WorkSpace.
    rebuildWorkspaceRequests :: Prelude.NonEmpty RebuildRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebuildWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rebuildWorkspaceRequests', 'rebuildWorkspaces_rebuildWorkspaceRequests' - The WorkSpace to rebuild. You can specify a single WorkSpace.
newRebuildWorkspaces ::
  -- | 'rebuildWorkspaceRequests'
  Prelude.NonEmpty RebuildRequest ->
  RebuildWorkspaces
newRebuildWorkspaces pRebuildWorkspaceRequests_ =
  RebuildWorkspaces'
    { rebuildWorkspaceRequests =
        Lens.coerced Lens.# pRebuildWorkspaceRequests_
    }

-- | The WorkSpace to rebuild. You can specify a single WorkSpace.
rebuildWorkspaces_rebuildWorkspaceRequests :: Lens.Lens' RebuildWorkspaces (Prelude.NonEmpty RebuildRequest)
rebuildWorkspaces_rebuildWorkspaceRequests = Lens.lens (\RebuildWorkspaces' {rebuildWorkspaceRequests} -> rebuildWorkspaceRequests) (\s@RebuildWorkspaces' {} a -> s {rebuildWorkspaceRequests = a} :: RebuildWorkspaces) Prelude.. Lens.coerced

instance Core.AWSRequest RebuildWorkspaces where
  type
    AWSResponse RebuildWorkspaces =
      RebuildWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RebuildWorkspacesResponse'
            Prelude.<$> (x Data..?> "FailedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebuildWorkspaces where
  hashWithSalt _salt RebuildWorkspaces' {..} =
    _salt
      `Prelude.hashWithSalt` rebuildWorkspaceRequests

instance Prelude.NFData RebuildWorkspaces where
  rnf RebuildWorkspaces' {..} =
    Prelude.rnf rebuildWorkspaceRequests

instance Data.ToHeaders RebuildWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.RebuildWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebuildWorkspaces where
  toJSON RebuildWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RebuildWorkspaceRequests"
                  Data..= rebuildWorkspaceRequests
              )
          ]
      )

instance Data.ToPath RebuildWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery RebuildWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebuildWorkspacesResponse' smart constructor.
data RebuildWorkspacesResponse = RebuildWorkspacesResponse'
  { -- | Information about the WorkSpace that could not be rebuilt.
    failedRequests :: Prelude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebuildWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'rebuildWorkspacesResponse_failedRequests' - Information about the WorkSpace that could not be rebuilt.
--
-- 'httpStatus', 'rebuildWorkspacesResponse_httpStatus' - The response's http status code.
newRebuildWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebuildWorkspacesResponse
newRebuildWorkspacesResponse pHttpStatus_ =
  RebuildWorkspacesResponse'
    { failedRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpace that could not be rebuilt.
rebuildWorkspacesResponse_failedRequests :: Lens.Lens' RebuildWorkspacesResponse (Prelude.Maybe [FailedWorkspaceChangeRequest])
rebuildWorkspacesResponse_failedRequests = Lens.lens (\RebuildWorkspacesResponse' {failedRequests} -> failedRequests) (\s@RebuildWorkspacesResponse' {} a -> s {failedRequests = a} :: RebuildWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rebuildWorkspacesResponse_httpStatus :: Lens.Lens' RebuildWorkspacesResponse Prelude.Int
rebuildWorkspacesResponse_httpStatus = Lens.lens (\RebuildWorkspacesResponse' {httpStatus} -> httpStatus) (\s@RebuildWorkspacesResponse' {} a -> s {httpStatus = a} :: RebuildWorkspacesResponse)

instance Prelude.NFData RebuildWorkspacesResponse where
  rnf RebuildWorkspacesResponse' {..} =
    Prelude.rnf failedRequests
      `Prelude.seq` Prelude.rnf httpStatus
