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
-- Module      : Network.AWS.CodeCommit.OverridePullRequestApprovalRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets aside (overrides) all approval rule requirements for a specified
-- pull request.
module Network.AWS.CodeCommit.OverridePullRequestApprovalRules
  ( -- * Creating a Request
    OverridePullRequestApprovalRules (..),
    newOverridePullRequestApprovalRules,

    -- * Request Lenses
    overridePullRequestApprovalRules_pullRequestId,
    overridePullRequestApprovalRules_revisionId,
    overridePullRequestApprovalRules_overrideStatus,

    -- * Destructuring the Response
    OverridePullRequestApprovalRulesResponse (..),
    newOverridePullRequestApprovalRulesResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newOverridePullRequestApprovalRules' smart constructor.
data OverridePullRequestApprovalRules = OverridePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request for which you want to
    -- override all approval rule requirements. To get this information, use
    -- GetPullRequest.
    pullRequestId :: Core.Text,
    -- | The system-generated ID of the most recent revision of the pull request.
    -- You cannot override approval rules for anything but the most recent
    -- revision of a pull request. To get the revision ID, use GetPullRequest.
    revisionId :: Core.Text,
    -- | Whether you want to set aside approval rule requirements for the pull
    -- request (OVERRIDE) or revoke a previous override and apply approval rule
    -- requirements (REVOKE). REVOKE status is not stored.
    overrideStatus :: OverrideStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OverridePullRequestApprovalRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'overridePullRequestApprovalRules_pullRequestId' - The system-generated ID of the pull request for which you want to
-- override all approval rule requirements. To get this information, use
-- GetPullRequest.
--
-- 'revisionId', 'overridePullRequestApprovalRules_revisionId' - The system-generated ID of the most recent revision of the pull request.
-- You cannot override approval rules for anything but the most recent
-- revision of a pull request. To get the revision ID, use GetPullRequest.
--
-- 'overrideStatus', 'overridePullRequestApprovalRules_overrideStatus' - Whether you want to set aside approval rule requirements for the pull
-- request (OVERRIDE) or revoke a previous override and apply approval rule
-- requirements (REVOKE). REVOKE status is not stored.
newOverridePullRequestApprovalRules ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'revisionId'
  Core.Text ->
  -- | 'overrideStatus'
  OverrideStatus ->
  OverridePullRequestApprovalRules
newOverridePullRequestApprovalRules
  pPullRequestId_
  pRevisionId_
  pOverrideStatus_ =
    OverridePullRequestApprovalRules'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_,
        overrideStatus = pOverrideStatus_
      }

-- | The system-generated ID of the pull request for which you want to
-- override all approval rule requirements. To get this information, use
-- GetPullRequest.
overridePullRequestApprovalRules_pullRequestId :: Lens.Lens' OverridePullRequestApprovalRules Core.Text
overridePullRequestApprovalRules_pullRequestId = Lens.lens (\OverridePullRequestApprovalRules' {pullRequestId} -> pullRequestId) (\s@OverridePullRequestApprovalRules' {} a -> s {pullRequestId = a} :: OverridePullRequestApprovalRules)

-- | The system-generated ID of the most recent revision of the pull request.
-- You cannot override approval rules for anything but the most recent
-- revision of a pull request. To get the revision ID, use GetPullRequest.
overridePullRequestApprovalRules_revisionId :: Lens.Lens' OverridePullRequestApprovalRules Core.Text
overridePullRequestApprovalRules_revisionId = Lens.lens (\OverridePullRequestApprovalRules' {revisionId} -> revisionId) (\s@OverridePullRequestApprovalRules' {} a -> s {revisionId = a} :: OverridePullRequestApprovalRules)

-- | Whether you want to set aside approval rule requirements for the pull
-- request (OVERRIDE) or revoke a previous override and apply approval rule
-- requirements (REVOKE). REVOKE status is not stored.
overridePullRequestApprovalRules_overrideStatus :: Lens.Lens' OverridePullRequestApprovalRules OverrideStatus
overridePullRequestApprovalRules_overrideStatus = Lens.lens (\OverridePullRequestApprovalRules' {overrideStatus} -> overrideStatus) (\s@OverridePullRequestApprovalRules' {} a -> s {overrideStatus = a} :: OverridePullRequestApprovalRules)

instance
  Core.AWSRequest
    OverridePullRequestApprovalRules
  where
  type
    AWSResponse OverridePullRequestApprovalRules =
      OverridePullRequestApprovalRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      OverridePullRequestApprovalRulesResponse'

instance
  Core.Hashable
    OverridePullRequestApprovalRules

instance Core.NFData OverridePullRequestApprovalRules

instance
  Core.ToHeaders
    OverridePullRequestApprovalRules
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.OverridePullRequestApprovalRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON OverridePullRequestApprovalRules where
  toJSON OverridePullRequestApprovalRules' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("revisionId" Core..= revisionId),
            Core.Just ("overrideStatus" Core..= overrideStatus)
          ]
      )

instance Core.ToPath OverridePullRequestApprovalRules where
  toPath = Core.const "/"

instance
  Core.ToQuery
    OverridePullRequestApprovalRules
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newOverridePullRequestApprovalRulesResponse' smart constructor.
data OverridePullRequestApprovalRulesResponse = OverridePullRequestApprovalRulesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OverridePullRequestApprovalRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newOverridePullRequestApprovalRulesResponse ::
  OverridePullRequestApprovalRulesResponse
newOverridePullRequestApprovalRulesResponse =
  OverridePullRequestApprovalRulesResponse'

instance
  Core.NFData
    OverridePullRequestApprovalRulesResponse
