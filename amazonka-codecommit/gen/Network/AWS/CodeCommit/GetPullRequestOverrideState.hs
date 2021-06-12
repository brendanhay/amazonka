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
-- Module      : Network.AWS.CodeCommit.GetPullRequestOverrideState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether approval rules have been set aside
-- (overridden) for a pull request, and if so, the Amazon Resource Name
-- (ARN) of the user or identity that overrode the rules and their
-- requirements for the pull request.
module Network.AWS.CodeCommit.GetPullRequestOverrideState
  ( -- * Creating a Request
    GetPullRequestOverrideState (..),
    newGetPullRequestOverrideState,

    -- * Request Lenses
    getPullRequestOverrideState_pullRequestId,
    getPullRequestOverrideState_revisionId,

    -- * Destructuring the Response
    GetPullRequestOverrideStateResponse (..),
    newGetPullRequestOverrideStateResponse,

    -- * Response Lenses
    getPullRequestOverrideStateResponse_overridden,
    getPullRequestOverrideStateResponse_overrider,
    getPullRequestOverrideStateResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPullRequestOverrideState' smart constructor.
data GetPullRequestOverrideState = GetPullRequestOverrideState'
  { -- | The ID of the pull request for which you want to get information about
    -- whether approval rules have been set aside (overridden).
    pullRequestId :: Core.Text,
    -- | The system-generated ID of the revision for the pull request. To
    -- retrieve the most recent revision ID, use GetPullRequest.
    revisionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPullRequestOverrideState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'getPullRequestOverrideState_pullRequestId' - The ID of the pull request for which you want to get information about
-- whether approval rules have been set aside (overridden).
--
-- 'revisionId', 'getPullRequestOverrideState_revisionId' - The system-generated ID of the revision for the pull request. To
-- retrieve the most recent revision ID, use GetPullRequest.
newGetPullRequestOverrideState ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'revisionId'
  Core.Text ->
  GetPullRequestOverrideState
newGetPullRequestOverrideState
  pPullRequestId_
  pRevisionId_ =
    GetPullRequestOverrideState'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_
      }

-- | The ID of the pull request for which you want to get information about
-- whether approval rules have been set aside (overridden).
getPullRequestOverrideState_pullRequestId :: Lens.Lens' GetPullRequestOverrideState Core.Text
getPullRequestOverrideState_pullRequestId = Lens.lens (\GetPullRequestOverrideState' {pullRequestId} -> pullRequestId) (\s@GetPullRequestOverrideState' {} a -> s {pullRequestId = a} :: GetPullRequestOverrideState)

-- | The system-generated ID of the revision for the pull request. To
-- retrieve the most recent revision ID, use GetPullRequest.
getPullRequestOverrideState_revisionId :: Lens.Lens' GetPullRequestOverrideState Core.Text
getPullRequestOverrideState_revisionId = Lens.lens (\GetPullRequestOverrideState' {revisionId} -> revisionId) (\s@GetPullRequestOverrideState' {} a -> s {revisionId = a} :: GetPullRequestOverrideState)

instance Core.AWSRequest GetPullRequestOverrideState where
  type
    AWSResponse GetPullRequestOverrideState =
      GetPullRequestOverrideStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPullRequestOverrideStateResponse'
            Core.<$> (x Core..?> "overridden")
            Core.<*> (x Core..?> "overrider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPullRequestOverrideState

instance Core.NFData GetPullRequestOverrideState

instance Core.ToHeaders GetPullRequestOverrideState where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetPullRequestOverrideState" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPullRequestOverrideState where
  toJSON GetPullRequestOverrideState' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("revisionId" Core..= revisionId)
          ]
      )

instance Core.ToPath GetPullRequestOverrideState where
  toPath = Core.const "/"

instance Core.ToQuery GetPullRequestOverrideState where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPullRequestOverrideStateResponse' smart constructor.
data GetPullRequestOverrideStateResponse = GetPullRequestOverrideStateResponse'
  { -- | A Boolean value that indicates whether a pull request has had its rules
    -- set aside (TRUE) or whether all approval rules still apply (FALSE).
    overridden :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the user or identity that overrode the
    -- rules and their requirements for the pull request.
    overrider :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPullRequestOverrideStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overridden', 'getPullRequestOverrideStateResponse_overridden' - A Boolean value that indicates whether a pull request has had its rules
-- set aside (TRUE) or whether all approval rules still apply (FALSE).
--
-- 'overrider', 'getPullRequestOverrideStateResponse_overrider' - The Amazon Resource Name (ARN) of the user or identity that overrode the
-- rules and their requirements for the pull request.
--
-- 'httpStatus', 'getPullRequestOverrideStateResponse_httpStatus' - The response's http status code.
newGetPullRequestOverrideStateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPullRequestOverrideStateResponse
newGetPullRequestOverrideStateResponse pHttpStatus_ =
  GetPullRequestOverrideStateResponse'
    { overridden =
        Core.Nothing,
      overrider = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Boolean value that indicates whether a pull request has had its rules
-- set aside (TRUE) or whether all approval rules still apply (FALSE).
getPullRequestOverrideStateResponse_overridden :: Lens.Lens' GetPullRequestOverrideStateResponse (Core.Maybe Core.Bool)
getPullRequestOverrideStateResponse_overridden = Lens.lens (\GetPullRequestOverrideStateResponse' {overridden} -> overridden) (\s@GetPullRequestOverrideStateResponse' {} a -> s {overridden = a} :: GetPullRequestOverrideStateResponse)

-- | The Amazon Resource Name (ARN) of the user or identity that overrode the
-- rules and their requirements for the pull request.
getPullRequestOverrideStateResponse_overrider :: Lens.Lens' GetPullRequestOverrideStateResponse (Core.Maybe Core.Text)
getPullRequestOverrideStateResponse_overrider = Lens.lens (\GetPullRequestOverrideStateResponse' {overrider} -> overrider) (\s@GetPullRequestOverrideStateResponse' {} a -> s {overrider = a} :: GetPullRequestOverrideStateResponse)

-- | The response's http status code.
getPullRequestOverrideStateResponse_httpStatus :: Lens.Lens' GetPullRequestOverrideStateResponse Core.Int
getPullRequestOverrideStateResponse_httpStatus = Lens.lens (\GetPullRequestOverrideStateResponse' {httpStatus} -> httpStatus) (\s@GetPullRequestOverrideStateResponse' {} a -> s {httpStatus = a} :: GetPullRequestOverrideStateResponse)

instance
  Core.NFData
    GetPullRequestOverrideStateResponse
