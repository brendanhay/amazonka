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
-- Module      : Network.AWS.CodeCommit.GetPullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a pull request in a specified repository.
module Network.AWS.CodeCommit.GetPullRequest
  ( -- * Creating a Request
    GetPullRequest (..),
    newGetPullRequest,

    -- * Request Lenses
    getPullRequest_pullRequestId,

    -- * Destructuring the Response
    GetPullRequestResponse (..),
    newGetPullRequestResponse,

    -- * Response Lenses
    getPullRequestResponse_httpStatus,
    getPullRequestResponse_pullRequest,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPullRequest' smart constructor.
data GetPullRequest = GetPullRequest'
  { -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'getPullRequest_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
newGetPullRequest ::
  -- | 'pullRequestId'
  Core.Text ->
  GetPullRequest
newGetPullRequest pPullRequestId_ =
  GetPullRequest' {pullRequestId = pPullRequestId_}

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
getPullRequest_pullRequestId :: Lens.Lens' GetPullRequest Core.Text
getPullRequest_pullRequestId = Lens.lens (\GetPullRequest' {pullRequestId} -> pullRequestId) (\s@GetPullRequest' {} a -> s {pullRequestId = a} :: GetPullRequest)

instance Core.AWSRequest GetPullRequest where
  type
    AWSResponse GetPullRequest =
      GetPullRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPullRequestResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "pullRequest")
      )

instance Core.Hashable GetPullRequest

instance Core.NFData GetPullRequest

instance Core.ToHeaders GetPullRequest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetPullRequest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPullRequest where
  toJSON GetPullRequest' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("pullRequestId" Core..= pullRequestId)]
      )

instance Core.ToPath GetPullRequest where
  toPath = Core.const "/"

instance Core.ToQuery GetPullRequest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPullRequestResponse' smart constructor.
data GetPullRequestResponse = GetPullRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about the specified pull request.
    pullRequest :: PullRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPullRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPullRequestResponse_httpStatus' - The response's http status code.
--
-- 'pullRequest', 'getPullRequestResponse_pullRequest' - Information about the specified pull request.
newGetPullRequestResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'pullRequest'
  PullRequest ->
  GetPullRequestResponse
newGetPullRequestResponse pHttpStatus_ pPullRequest_ =
  GetPullRequestResponse'
    { httpStatus = pHttpStatus_,
      pullRequest = pPullRequest_
    }

-- | The response's http status code.
getPullRequestResponse_httpStatus :: Lens.Lens' GetPullRequestResponse Core.Int
getPullRequestResponse_httpStatus = Lens.lens (\GetPullRequestResponse' {httpStatus} -> httpStatus) (\s@GetPullRequestResponse' {} a -> s {httpStatus = a} :: GetPullRequestResponse)

-- | Information about the specified pull request.
getPullRequestResponse_pullRequest :: Lens.Lens' GetPullRequestResponse PullRequest
getPullRequestResponse_pullRequest = Lens.lens (\GetPullRequestResponse' {pullRequest} -> pullRequest) (\s@GetPullRequestResponse' {} a -> s {pullRequest = a} :: GetPullRequestResponse)

instance Core.NFData GetPullRequestResponse
