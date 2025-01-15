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
-- Module      : Amazonka.CodeCommit.Types.PullRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PullRequest where

import Amazonka.CodeCommit.Types.ApprovalRule
import Amazonka.CodeCommit.Types.PullRequestStatusEnum
import Amazonka.CodeCommit.Types.PullRequestTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a pull request.
--
-- /See:/ 'newPullRequest' smart constructor.
data PullRequest = PullRequest'
  { -- | The approval rules applied to the pull request.
    approvalRules :: Prelude.Maybe [ApprovalRule],
    -- | The Amazon Resource Name (ARN) of the user who created the pull request.
    authorArn :: Prelude.Maybe Prelude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time the pull request was originally created, in timestamp
    -- format.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The user-defined description of the pull request. This description can
    -- be used to clarify what should be reviewed and other details of the
    -- request.
    description :: Prelude.Maybe Prelude.Text,
    -- | The day and time of the last user or system activity on the pull
    -- request, in timestamp format.
    lastActivityDate :: Prelude.Maybe Data.POSIX,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | The status of the pull request. Pull request status can only change from
    -- @OPEN@ to @CLOSED@.
    pullRequestStatus :: Prelude.Maybe PullRequestStatusEnum,
    -- | The targets of the pull request, including the source branch and
    -- destination branch for the pull request.
    pullRequestTargets :: Prelude.Maybe [PullRequestTarget],
    -- | The system-generated revision ID for the pull request.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The user-defined title of the pull request. This title is displayed in
    -- the list of pull requests to other repository users.
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRules', 'pullRequest_approvalRules' - The approval rules applied to the pull request.
--
-- 'authorArn', 'pullRequest_authorArn' - The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- 'clientRequestToken', 'pullRequest_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'creationDate', 'pullRequest_creationDate' - The date and time the pull request was originally created, in timestamp
-- format.
--
-- 'description', 'pullRequest_description' - The user-defined description of the pull request. This description can
-- be used to clarify what should be reviewed and other details of the
-- request.
--
-- 'lastActivityDate', 'pullRequest_lastActivityDate' - The day and time of the last user or system activity on the pull
-- request, in timestamp format.
--
-- 'pullRequestId', 'pullRequest_pullRequestId' - The system-generated ID of the pull request.
--
-- 'pullRequestStatus', 'pullRequest_pullRequestStatus' - The status of the pull request. Pull request status can only change from
-- @OPEN@ to @CLOSED@.
--
-- 'pullRequestTargets', 'pullRequest_pullRequestTargets' - The targets of the pull request, including the source branch and
-- destination branch for the pull request.
--
-- 'revisionId', 'pullRequest_revisionId' - The system-generated revision ID for the pull request.
--
-- 'title', 'pullRequest_title' - The user-defined title of the pull request. This title is displayed in
-- the list of pull requests to other repository users.
newPullRequest ::
  PullRequest
newPullRequest =
  PullRequest'
    { approvalRules = Prelude.Nothing,
      authorArn = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastActivityDate = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      pullRequestStatus = Prelude.Nothing,
      pullRequestTargets = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The approval rules applied to the pull request.
pullRequest_approvalRules :: Lens.Lens' PullRequest (Prelude.Maybe [ApprovalRule])
pullRequest_approvalRules = Lens.lens (\PullRequest' {approvalRules} -> approvalRules) (\s@PullRequest' {} a -> s {approvalRules = a} :: PullRequest) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the user who created the pull request.
pullRequest_authorArn :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_authorArn = Lens.lens (\PullRequest' {authorArn} -> authorArn) (\s@PullRequest' {} a -> s {authorArn = a} :: PullRequest)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
pullRequest_clientRequestToken :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_clientRequestToken = Lens.lens (\PullRequest' {clientRequestToken} -> clientRequestToken) (\s@PullRequest' {} a -> s {clientRequestToken = a} :: PullRequest)

-- | The date and time the pull request was originally created, in timestamp
-- format.
pullRequest_creationDate :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.UTCTime)
pullRequest_creationDate = Lens.lens (\PullRequest' {creationDate} -> creationDate) (\s@PullRequest' {} a -> s {creationDate = a} :: PullRequest) Prelude.. Lens.mapping Data._Time

-- | The user-defined description of the pull request. This description can
-- be used to clarify what should be reviewed and other details of the
-- request.
pullRequest_description :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_description = Lens.lens (\PullRequest' {description} -> description) (\s@PullRequest' {} a -> s {description = a} :: PullRequest)

-- | The day and time of the last user or system activity on the pull
-- request, in timestamp format.
pullRequest_lastActivityDate :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.UTCTime)
pullRequest_lastActivityDate = Lens.lens (\PullRequest' {lastActivityDate} -> lastActivityDate) (\s@PullRequest' {} a -> s {lastActivityDate = a} :: PullRequest) Prelude.. Lens.mapping Data._Time

-- | The system-generated ID of the pull request.
pullRequest_pullRequestId :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_pullRequestId = Lens.lens (\PullRequest' {pullRequestId} -> pullRequestId) (\s@PullRequest' {} a -> s {pullRequestId = a} :: PullRequest)

-- | The status of the pull request. Pull request status can only change from
-- @OPEN@ to @CLOSED@.
pullRequest_pullRequestStatus :: Lens.Lens' PullRequest (Prelude.Maybe PullRequestStatusEnum)
pullRequest_pullRequestStatus = Lens.lens (\PullRequest' {pullRequestStatus} -> pullRequestStatus) (\s@PullRequest' {} a -> s {pullRequestStatus = a} :: PullRequest)

-- | The targets of the pull request, including the source branch and
-- destination branch for the pull request.
pullRequest_pullRequestTargets :: Lens.Lens' PullRequest (Prelude.Maybe [PullRequestTarget])
pullRequest_pullRequestTargets = Lens.lens (\PullRequest' {pullRequestTargets} -> pullRequestTargets) (\s@PullRequest' {} a -> s {pullRequestTargets = a} :: PullRequest) Prelude.. Lens.mapping Lens.coerced

-- | The system-generated revision ID for the pull request.
pullRequest_revisionId :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_revisionId = Lens.lens (\PullRequest' {revisionId} -> revisionId) (\s@PullRequest' {} a -> s {revisionId = a} :: PullRequest)

-- | The user-defined title of the pull request. This title is displayed in
-- the list of pull requests to other repository users.
pullRequest_title :: Lens.Lens' PullRequest (Prelude.Maybe Prelude.Text)
pullRequest_title = Lens.lens (\PullRequest' {title} -> title) (\s@PullRequest' {} a -> s {title = a} :: PullRequest)

instance Data.FromJSON PullRequest where
  parseJSON =
    Data.withObject
      "PullRequest"
      ( \x ->
          PullRequest'
            Prelude.<$> (x Data..:? "approvalRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "authorArn")
            Prelude.<*> (x Data..:? "clientRequestToken")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastActivityDate")
            Prelude.<*> (x Data..:? "pullRequestId")
            Prelude.<*> (x Data..:? "pullRequestStatus")
            Prelude.<*> ( x
                            Data..:? "pullRequestTargets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "title")
      )

instance Prelude.Hashable PullRequest where
  hashWithSalt _salt PullRequest' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRules
      `Prelude.hashWithSalt` authorArn
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastActivityDate
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` pullRequestStatus
      `Prelude.hashWithSalt` pullRequestTargets
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` title

instance Prelude.NFData PullRequest where
  rnf PullRequest' {..} =
    Prelude.rnf approvalRules `Prelude.seq`
      Prelude.rnf authorArn `Prelude.seq`
        Prelude.rnf clientRequestToken `Prelude.seq`
          Prelude.rnf creationDate `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf lastActivityDate `Prelude.seq`
                Prelude.rnf pullRequestId `Prelude.seq`
                  Prelude.rnf pullRequestStatus `Prelude.seq`
                    Prelude.rnf pullRequestTargets `Prelude.seq`
                      Prelude.rnf revisionId `Prelude.seq`
                        Prelude.rnf title
