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
-- Module      : Amazonka.CodeCommit.UpdatePullRequestStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a pull request.
module Amazonka.CodeCommit.UpdatePullRequestStatus
  ( -- * Creating a Request
    UpdatePullRequestStatus (..),
    newUpdatePullRequestStatus,

    -- * Request Lenses
    updatePullRequestStatus_pullRequestId,
    updatePullRequestStatus_pullRequestStatus,

    -- * Destructuring the Response
    UpdatePullRequestStatusResponse (..),
    newUpdatePullRequestStatusResponse,

    -- * Response Lenses
    updatePullRequestStatusResponse_httpStatus,
    updatePullRequestStatusResponse_pullRequest,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The status of the pull request. The only valid operations are to update
    -- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
    -- @CLOSED@.
    pullRequestStatus :: PullRequestStatusEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestStatus_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'pullRequestStatus', 'updatePullRequestStatus_pullRequestStatus' - The status of the pull request. The only valid operations are to update
-- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
-- @CLOSED@.
newUpdatePullRequestStatus ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'pullRequestStatus'
  PullRequestStatusEnum ->
  UpdatePullRequestStatus
newUpdatePullRequestStatus
  pPullRequestId_
  pPullRequestStatus_ =
    UpdatePullRequestStatus'
      { pullRequestId =
          pPullRequestId_,
        pullRequestStatus = pPullRequestStatus_
      }

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
updatePullRequestStatus_pullRequestId :: Lens.Lens' UpdatePullRequestStatus Prelude.Text
updatePullRequestStatus_pullRequestId = Lens.lens (\UpdatePullRequestStatus' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestStatus' {} a -> s {pullRequestId = a} :: UpdatePullRequestStatus)

-- | The status of the pull request. The only valid operations are to update
-- the status from @OPEN@ to @OPEN@, @OPEN@ to @CLOSED@ or from @CLOSED@ to
-- @CLOSED@.
updatePullRequestStatus_pullRequestStatus :: Lens.Lens' UpdatePullRequestStatus PullRequestStatusEnum
updatePullRequestStatus_pullRequestStatus = Lens.lens (\UpdatePullRequestStatus' {pullRequestStatus} -> pullRequestStatus) (\s@UpdatePullRequestStatus' {} a -> s {pullRequestStatus = a} :: UpdatePullRequestStatus)

instance Core.AWSRequest UpdatePullRequestStatus where
  type
    AWSResponse UpdatePullRequestStatus =
      UpdatePullRequestStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "pullRequest")
      )

instance Prelude.Hashable UpdatePullRequestStatus where
  hashWithSalt _salt UpdatePullRequestStatus' {..} =
    _salt
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` pullRequestStatus

instance Prelude.NFData UpdatePullRequestStatus where
  rnf UpdatePullRequestStatus' {..} =
    Prelude.rnf pullRequestId `Prelude.seq`
      Prelude.rnf pullRequestStatus

instance Data.ToHeaders UpdatePullRequestStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdatePullRequestStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePullRequestStatus where
  toJSON UpdatePullRequestStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Data..= pullRequestId),
            Prelude.Just
              ("pullRequestStatus" Data..= pullRequestStatus)
          ]
      )

instance Data.ToPath UpdatePullRequestStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePullRequestStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the pull request.
    pullRequest :: PullRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePullRequestStatusResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'updatePullRequestStatusResponse_pullRequest' - Information about the pull request.
newUpdatePullRequestStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  UpdatePullRequestStatusResponse
newUpdatePullRequestStatusResponse
  pHttpStatus_
  pPullRequest_ =
    UpdatePullRequestStatusResponse'
      { httpStatus =
          pHttpStatus_,
        pullRequest = pPullRequest_
      }

-- | The response's http status code.
updatePullRequestStatusResponse_httpStatus :: Lens.Lens' UpdatePullRequestStatusResponse Prelude.Int
updatePullRequestStatusResponse_httpStatus = Lens.lens (\UpdatePullRequestStatusResponse' {httpStatus} -> httpStatus) (\s@UpdatePullRequestStatusResponse' {} a -> s {httpStatus = a} :: UpdatePullRequestStatusResponse)

-- | Information about the pull request.
updatePullRequestStatusResponse_pullRequest :: Lens.Lens' UpdatePullRequestStatusResponse PullRequest
updatePullRequestStatusResponse_pullRequest = Lens.lens (\UpdatePullRequestStatusResponse' {pullRequest} -> pullRequest) (\s@UpdatePullRequestStatusResponse' {} a -> s {pullRequest = a} :: UpdatePullRequestStatusResponse)

instance
  Prelude.NFData
    UpdatePullRequestStatusResponse
  where
  rnf UpdatePullRequestStatusResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf pullRequest
