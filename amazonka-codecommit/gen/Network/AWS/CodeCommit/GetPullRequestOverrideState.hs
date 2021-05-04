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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPullRequestOverrideState' smart constructor.
data GetPullRequestOverrideState = GetPullRequestOverrideState'
  { -- | The ID of the pull request for which you want to get information about
    -- whether approval rules have been set aside (overridden).
    pullRequestId :: Prelude.Text,
    -- | The system-generated ID of the revision for the pull request. To
    -- retrieve the most recent revision ID, use GetPullRequest.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
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
getPullRequestOverrideState_pullRequestId :: Lens.Lens' GetPullRequestOverrideState Prelude.Text
getPullRequestOverrideState_pullRequestId = Lens.lens (\GetPullRequestOverrideState' {pullRequestId} -> pullRequestId) (\s@GetPullRequestOverrideState' {} a -> s {pullRequestId = a} :: GetPullRequestOverrideState)

-- | The system-generated ID of the revision for the pull request. To
-- retrieve the most recent revision ID, use GetPullRequest.
getPullRequestOverrideState_revisionId :: Lens.Lens' GetPullRequestOverrideState Prelude.Text
getPullRequestOverrideState_revisionId = Lens.lens (\GetPullRequestOverrideState' {revisionId} -> revisionId) (\s@GetPullRequestOverrideState' {} a -> s {revisionId = a} :: GetPullRequestOverrideState)

instance
  Prelude.AWSRequest
    GetPullRequestOverrideState
  where
  type
    Rs GetPullRequestOverrideState =
      GetPullRequestOverrideStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPullRequestOverrideStateResponse'
            Prelude.<$> (x Prelude..?> "overridden")
            Prelude.<*> (x Prelude..?> "overrider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPullRequestOverrideState

instance Prelude.NFData GetPullRequestOverrideState

instance
  Prelude.ToHeaders
    GetPullRequestOverrideState
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.GetPullRequestOverrideState" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetPullRequestOverrideState where
  toJSON GetPullRequestOverrideState' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Prelude..= pullRequestId),
            Prelude.Just ("revisionId" Prelude..= revisionId)
          ]
      )

instance Prelude.ToPath GetPullRequestOverrideState where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetPullRequestOverrideState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPullRequestOverrideStateResponse' smart constructor.
data GetPullRequestOverrideStateResponse = GetPullRequestOverrideStateResponse'
  { -- | A Boolean value that indicates whether a pull request has had its rules
    -- set aside (TRUE) or whether all approval rules still apply (FALSE).
    overridden :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the user or identity that overrode the
    -- rules and their requirements for the pull request.
    overrider :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetPullRequestOverrideStateResponse
newGetPullRequestOverrideStateResponse pHttpStatus_ =
  GetPullRequestOverrideStateResponse'
    { overridden =
        Prelude.Nothing,
      overrider = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Boolean value that indicates whether a pull request has had its rules
-- set aside (TRUE) or whether all approval rules still apply (FALSE).
getPullRequestOverrideStateResponse_overridden :: Lens.Lens' GetPullRequestOverrideStateResponse (Prelude.Maybe Prelude.Bool)
getPullRequestOverrideStateResponse_overridden = Lens.lens (\GetPullRequestOverrideStateResponse' {overridden} -> overridden) (\s@GetPullRequestOverrideStateResponse' {} a -> s {overridden = a} :: GetPullRequestOverrideStateResponse)

-- | The Amazon Resource Name (ARN) of the user or identity that overrode the
-- rules and their requirements for the pull request.
getPullRequestOverrideStateResponse_overrider :: Lens.Lens' GetPullRequestOverrideStateResponse (Prelude.Maybe Prelude.Text)
getPullRequestOverrideStateResponse_overrider = Lens.lens (\GetPullRequestOverrideStateResponse' {overrider} -> overrider) (\s@GetPullRequestOverrideStateResponse' {} a -> s {overrider = a} :: GetPullRequestOverrideStateResponse)

-- | The response's http status code.
getPullRequestOverrideStateResponse_httpStatus :: Lens.Lens' GetPullRequestOverrideStateResponse Prelude.Int
getPullRequestOverrideStateResponse_httpStatus = Lens.lens (\GetPullRequestOverrideStateResponse' {httpStatus} -> httpStatus) (\s@GetPullRequestOverrideStateResponse' {} a -> s {httpStatus = a} :: GetPullRequestOverrideStateResponse)

instance
  Prelude.NFData
    GetPullRequestOverrideStateResponse
