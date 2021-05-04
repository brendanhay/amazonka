{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@
-- and a state of @AVAILABLE@, @IMPAIRED@, @UNHEALTHY@, or @ERROR@.
module Network.AWS.WorkSpaces.StopWorkspaces
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newStopWorkspaces' smart constructor.
data StopWorkspaces = StopWorkspaces'
  { -- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
    stopWorkspaceRequests :: Prelude.NonEmpty StopRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude._Coerce Lens.# pStopWorkspaceRequests_
    }

-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
stopWorkspaces_stopWorkspaceRequests :: Lens.Lens' StopWorkspaces (Prelude.NonEmpty StopRequest)
stopWorkspaces_stopWorkspaceRequests = Lens.lens (\StopWorkspaces' {stopWorkspaceRequests} -> stopWorkspaceRequests) (\s@StopWorkspaces' {} a -> s {stopWorkspaceRequests = a} :: StopWorkspaces) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest StopWorkspaces where
  type Rs StopWorkspaces = StopWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopWorkspacesResponse'
            Prelude.<$> ( x Prelude..?> "FailedRequests"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopWorkspaces

instance Prelude.NFData StopWorkspaces

instance Prelude.ToHeaders StopWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.StopWorkspaces" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopWorkspaces where
  toJSON StopWorkspaces' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StopWorkspaceRequests"
                  Prelude..= stopWorkspaceRequests
              )
          ]
      )

instance Prelude.ToPath StopWorkspaces where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be stopped.
    failedRequests :: Prelude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
stopWorkspacesResponse_failedRequests = Lens.lens (\StopWorkspacesResponse' {failedRequests} -> failedRequests) (\s@StopWorkspacesResponse' {} a -> s {failedRequests = a} :: StopWorkspacesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
stopWorkspacesResponse_httpStatus :: Lens.Lens' StopWorkspacesResponse Prelude.Int
stopWorkspacesResponse_httpStatus = Lens.lens (\StopWorkspacesResponse' {httpStatus} -> httpStatus) (\s@StopWorkspacesResponse' {} a -> s {httpStatus = a} :: StopWorkspacesResponse)

instance Prelude.NFData StopWorkspacesResponse
