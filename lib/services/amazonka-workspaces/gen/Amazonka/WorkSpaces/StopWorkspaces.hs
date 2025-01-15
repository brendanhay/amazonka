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
-- Module      : Amazonka.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@
-- and a state of @AVAILABLE@, @IMPAIRED@, @UNHEALTHY@, or @ERROR@.
module Amazonka.WorkSpaces.StopWorkspaces
  ( -- * Creating a Request
    StopWorkspaces (..),
    newStopWorkspaces,

    -- * Request Lenses
    stopWorkspaces_stopWorkspaceRequests,

    -- * Destructuring the Response
    StopWorkspacesResponse (..),
    newStopWorkspacesResponse,

    -- * Response Lenses
    stopWorkspacesResponse_failedRequests,
    stopWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newStopWorkspaces' smart constructor.
data StopWorkspaces = StopWorkspaces'
  { -- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
    stopWorkspaceRequests :: Prelude.NonEmpty StopRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopWorkspaceRequests', 'stopWorkspaces_stopWorkspaceRequests' - The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
newStopWorkspaces ::
  -- | 'stopWorkspaceRequests'
  Prelude.NonEmpty StopRequest ->
  StopWorkspaces
newStopWorkspaces pStopWorkspaceRequests_ =
  StopWorkspaces'
    { stopWorkspaceRequests =
        Lens.coerced Lens.# pStopWorkspaceRequests_
    }

-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
stopWorkspaces_stopWorkspaceRequests :: Lens.Lens' StopWorkspaces (Prelude.NonEmpty StopRequest)
stopWorkspaces_stopWorkspaceRequests = Lens.lens (\StopWorkspaces' {stopWorkspaceRequests} -> stopWorkspaceRequests) (\s@StopWorkspaces' {} a -> s {stopWorkspaceRequests = a} :: StopWorkspaces) Prelude.. Lens.coerced

instance Core.AWSRequest StopWorkspaces where
  type
    AWSResponse StopWorkspaces =
      StopWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopWorkspacesResponse'
            Prelude.<$> (x Data..?> "FailedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopWorkspaces where
  hashWithSalt _salt StopWorkspaces' {..} =
    _salt `Prelude.hashWithSalt` stopWorkspaceRequests

instance Prelude.NFData StopWorkspaces where
  rnf StopWorkspaces' {..} =
    Prelude.rnf stopWorkspaceRequests

instance Data.ToHeaders StopWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.StopWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopWorkspaces where
  toJSON StopWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StopWorkspaceRequests"
                  Data..= stopWorkspaceRequests
              )
          ]
      )

instance Data.ToPath StopWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery StopWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be stopped.
    failedRequests :: Prelude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'stopWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be stopped.
--
-- 'httpStatus', 'stopWorkspacesResponse_httpStatus' - The response's http status code.
newStopWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopWorkspacesResponse
newStopWorkspacesResponse pHttpStatus_ =
  StopWorkspacesResponse'
    { failedRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be stopped.
stopWorkspacesResponse_failedRequests :: Lens.Lens' StopWorkspacesResponse (Prelude.Maybe [FailedWorkspaceChangeRequest])
stopWorkspacesResponse_failedRequests = Lens.lens (\StopWorkspacesResponse' {failedRequests} -> failedRequests) (\s@StopWorkspacesResponse' {} a -> s {failedRequests = a} :: StopWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopWorkspacesResponse_httpStatus :: Lens.Lens' StopWorkspacesResponse Prelude.Int
stopWorkspacesResponse_httpStatus = Lens.lens (\StopWorkspacesResponse' {httpStatus} -> httpStatus) (\s@StopWorkspacesResponse' {} a -> s {httpStatus = a} :: StopWorkspacesResponse)

instance Prelude.NFData StopWorkspacesResponse where
  rnf StopWorkspacesResponse' {..} =
    Prelude.rnf failedRequests `Prelude.seq`
      Prelude.rnf httpStatus
