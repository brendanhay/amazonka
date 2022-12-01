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
-- Module      : Amazonka.CodeCommit.UpdatePullRequestTitle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the title of a pull request.
module Amazonka.CodeCommit.UpdatePullRequestTitle
  ( -- * Creating a Request
    UpdatePullRequestTitle (..),
    newUpdatePullRequestTitle,

    -- * Request Lenses
    updatePullRequestTitle_pullRequestId,
    updatePullRequestTitle_title,

    -- * Destructuring the Response
    UpdatePullRequestTitleResponse (..),
    newUpdatePullRequestTitleResponse,

    -- * Response Lenses
    updatePullRequestTitleResponse_httpStatus,
    updatePullRequestTitleResponse_pullRequest,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePullRequestTitle' smart constructor.
data UpdatePullRequestTitle = UpdatePullRequestTitle'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The updated title of the pull request. This replaces the existing title.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestTitle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestTitle_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'title', 'updatePullRequestTitle_title' - The updated title of the pull request. This replaces the existing title.
newUpdatePullRequestTitle ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  UpdatePullRequestTitle
newUpdatePullRequestTitle pPullRequestId_ pTitle_ =
  UpdatePullRequestTitle'
    { pullRequestId =
        pPullRequestId_,
      title = pTitle_
    }

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
updatePullRequestTitle_pullRequestId :: Lens.Lens' UpdatePullRequestTitle Prelude.Text
updatePullRequestTitle_pullRequestId = Lens.lens (\UpdatePullRequestTitle' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestTitle' {} a -> s {pullRequestId = a} :: UpdatePullRequestTitle)

-- | The updated title of the pull request. This replaces the existing title.
updatePullRequestTitle_title :: Lens.Lens' UpdatePullRequestTitle Prelude.Text
updatePullRequestTitle_title = Lens.lens (\UpdatePullRequestTitle' {title} -> title) (\s@UpdatePullRequestTitle' {} a -> s {title = a} :: UpdatePullRequestTitle)

instance Core.AWSRequest UpdatePullRequestTitle where
  type
    AWSResponse UpdatePullRequestTitle =
      UpdatePullRequestTitleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestTitleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "pullRequest")
      )

instance Prelude.Hashable UpdatePullRequestTitle where
  hashWithSalt _salt UpdatePullRequestTitle' {..} =
    _salt `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` title

instance Prelude.NFData UpdatePullRequestTitle where
  rnf UpdatePullRequestTitle' {..} =
    Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf title

instance Core.ToHeaders UpdatePullRequestTitle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdatePullRequestTitle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePullRequestTitle where
  toJSON UpdatePullRequestTitle' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Core..= pullRequestId),
            Prelude.Just ("title" Core..= title)
          ]
      )

instance Core.ToPath UpdatePullRequestTitle where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePullRequestTitle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestTitleResponse' smart constructor.
data UpdatePullRequestTitleResponse = UpdatePullRequestTitleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the updated pull request.
    pullRequest :: PullRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestTitleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePullRequestTitleResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'updatePullRequestTitleResponse_pullRequest' - Information about the updated pull request.
newUpdatePullRequestTitleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  UpdatePullRequestTitleResponse
newUpdatePullRequestTitleResponse
  pHttpStatus_
  pPullRequest_ =
    UpdatePullRequestTitleResponse'
      { httpStatus =
          pHttpStatus_,
        pullRequest = pPullRequest_
      }

-- | The response's http status code.
updatePullRequestTitleResponse_httpStatus :: Lens.Lens' UpdatePullRequestTitleResponse Prelude.Int
updatePullRequestTitleResponse_httpStatus = Lens.lens (\UpdatePullRequestTitleResponse' {httpStatus} -> httpStatus) (\s@UpdatePullRequestTitleResponse' {} a -> s {httpStatus = a} :: UpdatePullRequestTitleResponse)

-- | Information about the updated pull request.
updatePullRequestTitleResponse_pullRequest :: Lens.Lens' UpdatePullRequestTitleResponse PullRequest
updatePullRequestTitleResponse_pullRequest = Lens.lens (\UpdatePullRequestTitleResponse' {pullRequest} -> pullRequest) (\s@UpdatePullRequestTitleResponse' {} a -> s {pullRequest = a} :: UpdatePullRequestTitleResponse)

instance
  Prelude.NFData
    UpdatePullRequestTitleResponse
  where
  rnf UpdatePullRequestTitleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pullRequest
