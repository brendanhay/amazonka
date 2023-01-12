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
-- Module      : Amazonka.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- You cannot reboot a WorkSpace unless its state is @AVAILABLE@ or
-- @UNHEALTHY@.
--
-- This operation is asynchronous and returns before the WorkSpaces have
-- rebooted.
module Amazonka.WorkSpaces.RebootWorkspaces
  ( -- * Creating a Request
    RebootWorkspaces (..),
    newRebootWorkspaces,

    -- * Request Lenses
    rebootWorkspaces_rebootWorkspaceRequests,

    -- * Destructuring the Response
    RebootWorkspacesResponse (..),
    newRebootWorkspacesResponse,

    -- * Response Lenses
    rebootWorkspacesResponse_failedRequests,
    rebootWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newRebootWorkspaces' smart constructor.
data RebootWorkspaces = RebootWorkspaces'
  { -- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
    rebootWorkspaceRequests :: Prelude.NonEmpty RebootRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rebootWorkspaceRequests', 'rebootWorkspaces_rebootWorkspaceRequests' - The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
newRebootWorkspaces ::
  -- | 'rebootWorkspaceRequests'
  Prelude.NonEmpty RebootRequest ->
  RebootWorkspaces
newRebootWorkspaces pRebootWorkspaceRequests_ =
  RebootWorkspaces'
    { rebootWorkspaceRequests =
        Lens.coerced Lens.# pRebootWorkspaceRequests_
    }

-- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
rebootWorkspaces_rebootWorkspaceRequests :: Lens.Lens' RebootWorkspaces (Prelude.NonEmpty RebootRequest)
rebootWorkspaces_rebootWorkspaceRequests = Lens.lens (\RebootWorkspaces' {rebootWorkspaceRequests} -> rebootWorkspaceRequests) (\s@RebootWorkspaces' {} a -> s {rebootWorkspaceRequests = a} :: RebootWorkspaces) Prelude.. Lens.coerced

instance Core.AWSRequest RebootWorkspaces where
  type
    AWSResponse RebootWorkspaces =
      RebootWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootWorkspacesResponse'
            Prelude.<$> (x Data..?> "FailedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootWorkspaces where
  hashWithSalt _salt RebootWorkspaces' {..} =
    _salt
      `Prelude.hashWithSalt` rebootWorkspaceRequests

instance Prelude.NFData RebootWorkspaces where
  rnf RebootWorkspaces' {..} =
    Prelude.rnf rebootWorkspaceRequests

instance Data.ToHeaders RebootWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.RebootWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebootWorkspaces where
  toJSON RebootWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RebootWorkspaceRequests"
                  Data..= rebootWorkspaceRequests
              )
          ]
      )

instance Data.ToPath RebootWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be rebooted.
    failedRequests :: Prelude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'rebootWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be rebooted.
--
-- 'httpStatus', 'rebootWorkspacesResponse_httpStatus' - The response's http status code.
newRebootWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootWorkspacesResponse
newRebootWorkspacesResponse pHttpStatus_ =
  RebootWorkspacesResponse'
    { failedRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be rebooted.
rebootWorkspacesResponse_failedRequests :: Lens.Lens' RebootWorkspacesResponse (Prelude.Maybe [FailedWorkspaceChangeRequest])
rebootWorkspacesResponse_failedRequests = Lens.lens (\RebootWorkspacesResponse' {failedRequests} -> failedRequests) (\s@RebootWorkspacesResponse' {} a -> s {failedRequests = a} :: RebootWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rebootWorkspacesResponse_httpStatus :: Lens.Lens' RebootWorkspacesResponse Prelude.Int
rebootWorkspacesResponse_httpStatus = Lens.lens (\RebootWorkspacesResponse' {httpStatus} -> httpStatus) (\s@RebootWorkspacesResponse' {} a -> s {httpStatus = a} :: RebootWorkspacesResponse)

instance Prelude.NFData RebootWorkspacesResponse where
  rnf RebootWorkspacesResponse' {..} =
    Prelude.rnf failedRequests
      `Prelude.seq` Prelude.rnf httpStatus
