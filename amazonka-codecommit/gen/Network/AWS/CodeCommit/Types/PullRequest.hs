{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequest where

import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.CodeCommit.Types.PullRequestTarget
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a pull request.
--
-- /See:/ 'newPullRequest' smart constructor.
data PullRequest = PullRequest'
  { -- | The system-generated revision ID for the pull request.
    revisionId :: Core.Maybe Core.Text,
    -- | The targets of the pull request, including the source branch and
    -- destination branch for the pull request.
    pullRequestTargets :: Core.Maybe [PullRequestTarget],
    -- | The user-defined title of the pull request. This title is displayed in
    -- the list of pull requests to other repository users.
    title :: Core.Maybe Core.Text,
    -- | The status of the pull request. Pull request status can only change from
    -- @OPEN@ to @CLOSED@.
    pullRequestStatus :: Core.Maybe PullRequestStatusEnum,
    -- | The date and time the pull request was originally created, in timestamp
    -- format.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Core.Maybe Core.Text,
    -- | The user-defined description of the pull request. This description can
    -- be used to clarify what should be reviewed and other details of the
    -- request.
    description :: Core.Maybe Core.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The day and time of the last user or system activity on the pull
    -- request, in timestamp format.
    lastActivityDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who created the pull request.
    authorArn :: Core.Maybe Core.Text,
    -- | The approval rules applied to the pull request.
    approvalRules :: Core.Maybe [ApprovalRule]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'pullRequest_revisionId' - The system-generated revision ID for the pull request.
--
-- 'pullRequestTargets', 'pullRequest_pullRequestTargets' - The targets of the pull request, including the source branch and
-- destination branch for the pull request.
--
-- 'title', 'pullRequest_title' - The user-defined title of the pull request. This title is displayed in
-- the list of pull requests to other repository users.
--
-- 'pullRequestStatus', 'pullRequest_pullRequestStatus' - The status of the pull request. Pull request status can only change from
-- @OPEN@ to @CLOSED@.
--
-- 'creationDate', 'pullRequest_creationDate' - The date and time the pull request was originally created, in timestamp
-- format.
--
-- 'pullRequestId', 'pullRequest_pullRequestId' - The system-generated ID of the pull request.
--
-- 'description', 'pullRequest_description' - The user-defined description of the pull request. This description can
-- be used to clarify what should be reviewed and other details of the
-- request.
--
-- 'clientRequestToken', 'pullRequest_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'lastActivityDate', 'pullRequest_lastActivityDate' - The day and time of the last user or system activity on the pull
-- request, in timestamp format.
--
-- 'authorArn', 'pullRequest_authorArn' - The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- 'approvalRules', 'pullRequest_approvalRules' - The approval rules applied to the pull request.
newPullRequest ::
  PullRequest
newPullRequest =
  PullRequest'
    { revisionId = Core.Nothing,
      pullRequestTargets = Core.Nothing,
      title = Core.Nothing,
      pullRequestStatus = Core.Nothing,
      creationDate = Core.Nothing,
      pullRequestId = Core.Nothing,
      description = Core.Nothing,
      clientRequestToken = Core.Nothing,
      lastActivityDate = Core.Nothing,
      authorArn = Core.Nothing,
      approvalRules = Core.Nothing
    }

-- | The system-generated revision ID for the pull request.
pullRequest_revisionId :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_revisionId = Lens.lens (\PullRequest' {revisionId} -> revisionId) (\s@PullRequest' {} a -> s {revisionId = a} :: PullRequest)

-- | The targets of the pull request, including the source branch and
-- destination branch for the pull request.
pullRequest_pullRequestTargets :: Lens.Lens' PullRequest (Core.Maybe [PullRequestTarget])
pullRequest_pullRequestTargets = Lens.lens (\PullRequest' {pullRequestTargets} -> pullRequestTargets) (\s@PullRequest' {} a -> s {pullRequestTargets = a} :: PullRequest) Core.. Lens.mapping Lens._Coerce

-- | The user-defined title of the pull request. This title is displayed in
-- the list of pull requests to other repository users.
pullRequest_title :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_title = Lens.lens (\PullRequest' {title} -> title) (\s@PullRequest' {} a -> s {title = a} :: PullRequest)

-- | The status of the pull request. Pull request status can only change from
-- @OPEN@ to @CLOSED@.
pullRequest_pullRequestStatus :: Lens.Lens' PullRequest (Core.Maybe PullRequestStatusEnum)
pullRequest_pullRequestStatus = Lens.lens (\PullRequest' {pullRequestStatus} -> pullRequestStatus) (\s@PullRequest' {} a -> s {pullRequestStatus = a} :: PullRequest)

-- | The date and time the pull request was originally created, in timestamp
-- format.
pullRequest_creationDate :: Lens.Lens' PullRequest (Core.Maybe Core.UTCTime)
pullRequest_creationDate = Lens.lens (\PullRequest' {creationDate} -> creationDate) (\s@PullRequest' {} a -> s {creationDate = a} :: PullRequest) Core.. Lens.mapping Core._Time

-- | The system-generated ID of the pull request.
pullRequest_pullRequestId :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_pullRequestId = Lens.lens (\PullRequest' {pullRequestId} -> pullRequestId) (\s@PullRequest' {} a -> s {pullRequestId = a} :: PullRequest)

-- | The user-defined description of the pull request. This description can
-- be used to clarify what should be reviewed and other details of the
-- request.
pullRequest_description :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_description = Lens.lens (\PullRequest' {description} -> description) (\s@PullRequest' {} a -> s {description = a} :: PullRequest)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
pullRequest_clientRequestToken :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_clientRequestToken = Lens.lens (\PullRequest' {clientRequestToken} -> clientRequestToken) (\s@PullRequest' {} a -> s {clientRequestToken = a} :: PullRequest)

-- | The day and time of the last user or system activity on the pull
-- request, in timestamp format.
pullRequest_lastActivityDate :: Lens.Lens' PullRequest (Core.Maybe Core.UTCTime)
pullRequest_lastActivityDate = Lens.lens (\PullRequest' {lastActivityDate} -> lastActivityDate) (\s@PullRequest' {} a -> s {lastActivityDate = a} :: PullRequest) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the user who created the pull request.
pullRequest_authorArn :: Lens.Lens' PullRequest (Core.Maybe Core.Text)
pullRequest_authorArn = Lens.lens (\PullRequest' {authorArn} -> authorArn) (\s@PullRequest' {} a -> s {authorArn = a} :: PullRequest)

-- | The approval rules applied to the pull request.
pullRequest_approvalRules :: Lens.Lens' PullRequest (Core.Maybe [ApprovalRule])
pullRequest_approvalRules = Lens.lens (\PullRequest' {approvalRules} -> approvalRules) (\s@PullRequest' {} a -> s {approvalRules = a} :: PullRequest) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON PullRequest where
  parseJSON =
    Core.withObject
      "PullRequest"
      ( \x ->
          PullRequest'
            Core.<$> (x Core..:? "revisionId")
            Core.<*> ( x Core..:? "pullRequestTargets"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "title")
            Core.<*> (x Core..:? "pullRequestStatus")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "pullRequestId")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "clientRequestToken")
            Core.<*> (x Core..:? "lastActivityDate")
            Core.<*> (x Core..:? "authorArn")
            Core.<*> (x Core..:? "approvalRules" Core..!= Core.mempty)
      )

instance Core.Hashable PullRequest

instance Core.NFData PullRequest
