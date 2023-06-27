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
-- Module      : Amazonka.CodeCommit.OverridePullRequestApprovalRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets aside (overrides) all approval rule requirements for a specified
-- pull request.
module Amazonka.CodeCommit.OverridePullRequestApprovalRules
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newOverridePullRequestApprovalRules' smart constructor.
data OverridePullRequestApprovalRules = OverridePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request for which you want to
    -- override all approval rule requirements. To get this information, use
    -- GetPullRequest.
    pullRequestId :: Prelude.Text,
    -- | The system-generated ID of the most recent revision of the pull request.
    -- You cannot override approval rules for anything but the most recent
    -- revision of a pull request. To get the revision ID, use GetPullRequest.
    revisionId :: Prelude.Text,
    -- | Whether you want to set aside approval rule requirements for the pull
    -- request (OVERRIDE) or revoke a previous override and apply approval rule
    -- requirements (REVOKE). REVOKE status is not stored.
    overrideStatus :: OverrideStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
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
overridePullRequestApprovalRules_pullRequestId :: Lens.Lens' OverridePullRequestApprovalRules Prelude.Text
overridePullRequestApprovalRules_pullRequestId = Lens.lens (\OverridePullRequestApprovalRules' {pullRequestId} -> pullRequestId) (\s@OverridePullRequestApprovalRules' {} a -> s {pullRequestId = a} :: OverridePullRequestApprovalRules)

-- | The system-generated ID of the most recent revision of the pull request.
-- You cannot override approval rules for anything but the most recent
-- revision of a pull request. To get the revision ID, use GetPullRequest.
overridePullRequestApprovalRules_revisionId :: Lens.Lens' OverridePullRequestApprovalRules Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      OverridePullRequestApprovalRulesResponse'

instance
  Prelude.Hashable
    OverridePullRequestApprovalRules
  where
  hashWithSalt
    _salt
    OverridePullRequestApprovalRules' {..} =
      _salt
        `Prelude.hashWithSalt` pullRequestId
        `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` overrideStatus

instance
  Prelude.NFData
    OverridePullRequestApprovalRules
  where
  rnf OverridePullRequestApprovalRules' {..} =
    Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf overrideStatus

instance
  Data.ToHeaders
    OverridePullRequestApprovalRules
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.OverridePullRequestApprovalRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON OverridePullRequestApprovalRules where
  toJSON OverridePullRequestApprovalRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Data..= pullRequestId),
            Prelude.Just ("revisionId" Data..= revisionId),
            Prelude.Just
              ("overrideStatus" Data..= overrideStatus)
          ]
      )

instance Data.ToPath OverridePullRequestApprovalRules where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    OverridePullRequestApprovalRules
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newOverridePullRequestApprovalRulesResponse' smart constructor.
data OverridePullRequestApprovalRulesResponse = OverridePullRequestApprovalRulesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverridePullRequestApprovalRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newOverridePullRequestApprovalRulesResponse ::
  OverridePullRequestApprovalRulesResponse
newOverridePullRequestApprovalRulesResponse =
  OverridePullRequestApprovalRulesResponse'

instance
  Prelude.NFData
    OverridePullRequestApprovalRulesResponse
  where
  rnf _ = ()
