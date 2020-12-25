{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequest
  ( PullRequest (..),

    -- * Smart constructor
    mkPullRequest,

    -- * Lenses
    prApprovalRules,
    prAuthorArn,
    prClientRequestToken,
    prCreationDate,
    prDescription,
    prLastActivityDate,
    prPullRequestId,
    prPullRequestStatus,
    prPullRequestTargets,
    prRevisionId,
    prTitle,
  )
where

import qualified Network.AWS.CodeCommit.Types.ApprovalRule as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.ClientRequestToken as Types
import qualified Network.AWS.CodeCommit.Types.Description as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestId as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestStatusEnum as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestTarget as Types
import qualified Network.AWS.CodeCommit.Types.RevisionId as Types
import qualified Network.AWS.CodeCommit.Types.Title as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a pull request.
--
-- /See:/ 'mkPullRequest' smart constructor.
data PullRequest = PullRequest'
  { -- | The approval rules applied to the pull request.
    approvalRules :: Core.Maybe [Types.ApprovalRule],
    -- | The Amazon Resource Name (ARN) of the user who created the pull request.
    authorArn :: Core.Maybe Types.Arn,
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The date and time the pull request was originally created, in timestamp format.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
    description :: Core.Maybe Types.Description,
    -- | The day and time of the last user or system activity on the pull request, in timestamp format.
    lastActivityDate :: Core.Maybe Core.NominalDiffTime,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Core.Maybe Types.PullRequestId,
    -- | The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
    pullRequestStatus :: Core.Maybe Types.PullRequestStatusEnum,
    -- | The targets of the pull request, including the source branch and destination branch for the pull request.
    pullRequestTargets :: Core.Maybe [Types.PullRequestTarget],
    -- | The system-generated revision ID for the pull request.
    revisionId :: Core.Maybe Types.RevisionId,
    -- | The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
    title :: Core.Maybe Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PullRequest' value with any optional fields omitted.
mkPullRequest ::
  PullRequest
mkPullRequest =
  PullRequest'
    { approvalRules = Core.Nothing,
      authorArn = Core.Nothing,
      clientRequestToken = Core.Nothing,
      creationDate = Core.Nothing,
      description = Core.Nothing,
      lastActivityDate = Core.Nothing,
      pullRequestId = Core.Nothing,
      pullRequestStatus = Core.Nothing,
      pullRequestTargets = Core.Nothing,
      revisionId = Core.Nothing,
      title = Core.Nothing
    }

-- | The approval rules applied to the pull request.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApprovalRules :: Lens.Lens' PullRequest (Core.Maybe [Types.ApprovalRule])
prApprovalRules = Lens.field @"approvalRules"
{-# DEPRECATED prApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- /Note:/ Consider using 'authorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prAuthorArn :: Lens.Lens' PullRequest (Core.Maybe Types.Arn)
prAuthorArn = Lens.field @"authorArn"
{-# DEPRECATED prAuthorArn "Use generic-lens or generic-optics with 'authorArn' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prClientRequestToken :: Lens.Lens' PullRequest (Core.Maybe Types.ClientRequestToken)
prClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED prClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The date and time the pull request was originally created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCreationDate :: Lens.Lens' PullRequest (Core.Maybe Core.NominalDiffTime)
prCreationDate = Lens.field @"creationDate"
{-# DEPRECATED prCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prDescription :: Lens.Lens' PullRequest (Core.Maybe Types.Description)
prDescription = Lens.field @"description"
{-# DEPRECATED prDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The day and time of the last user or system activity on the pull request, in timestamp format.
--
-- /Note:/ Consider using 'lastActivityDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prLastActivityDate :: Lens.Lens' PullRequest (Core.Maybe Core.NominalDiffTime)
prLastActivityDate = Lens.field @"lastActivityDate"
{-# DEPRECATED prLastActivityDate "Use generic-lens or generic-optics with 'lastActivityDate' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestId :: Lens.Lens' PullRequest (Core.Maybe Types.PullRequestId)
prPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED prPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestStatus :: Lens.Lens' PullRequest (Core.Maybe Types.PullRequestStatusEnum)
prPullRequestStatus = Lens.field @"pullRequestStatus"
{-# DEPRECATED prPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

-- | The targets of the pull request, including the source branch and destination branch for the pull request.
--
-- /Note:/ Consider using 'pullRequestTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestTargets :: Lens.Lens' PullRequest (Core.Maybe [Types.PullRequestTarget])
prPullRequestTargets = Lens.field @"pullRequestTargets"
{-# DEPRECATED prPullRequestTargets "Use generic-lens or generic-optics with 'pullRequestTargets' instead." #-}

-- | The system-generated revision ID for the pull request.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRevisionId :: Lens.Lens' PullRequest (Core.Maybe Types.RevisionId)
prRevisionId = Lens.field @"revisionId"
{-# DEPRECATED prRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prTitle :: Lens.Lens' PullRequest (Core.Maybe Types.Title)
prTitle = Lens.field @"title"
{-# DEPRECATED prTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON PullRequest where
  parseJSON =
    Core.withObject "PullRequest" Core.$
      \x ->
        PullRequest'
          Core.<$> (x Core..:? "approvalRules")
          Core.<*> (x Core..:? "authorArn")
          Core.<*> (x Core..:? "clientRequestToken")
          Core.<*> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "lastActivityDate")
          Core.<*> (x Core..:? "pullRequestId")
          Core.<*> (x Core..:? "pullRequestStatus")
          Core.<*> (x Core..:? "pullRequestTargets")
          Core.<*> (x Core..:? "revisionId")
          Core.<*> (x Core..:? "title")
