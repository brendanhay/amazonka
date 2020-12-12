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
    prAuthorARN,
    prPullRequestId,
    prCreationDate,
    prPullRequestStatus,
    prTitle,
    prClientRequestToken,
    prLastActivityDate,
    prRevisionId,
    prPullRequestTargets,
    prDescription,
  )
where

import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.CodeCommit.Types.PullRequestTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a pull request.
--
-- /See:/ 'mkPullRequest' smart constructor.
data PullRequest = PullRequest'
  { approvalRules ::
      Lude.Maybe [ApprovalRule],
    authorARN :: Lude.Maybe Lude.Text,
    pullRequestId :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    pullRequestStatus :: Lude.Maybe PullRequestStatusEnum,
    title :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    lastActivityDate :: Lude.Maybe Lude.Timestamp,
    revisionId :: Lude.Maybe Lude.Text,
    pullRequestTargets :: Lude.Maybe [PullRequestTarget],
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequest' with the minimum fields required to make a request.
--
-- * 'approvalRules' - The approval rules applied to the pull request.
-- * 'authorARN' - The Amazon Resource Name (ARN) of the user who created the pull request.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
-- * 'creationDate' - The date and time the pull request was originally created, in timestamp format.
-- * 'description' - The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
-- * 'lastActivityDate' - The day and time of the last user or system activity on the pull request, in timestamp format.
-- * 'pullRequestId' - The system-generated ID of the pull request.
-- * 'pullRequestStatus' - The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
-- * 'pullRequestTargets' - The targets of the pull request, including the source branch and destination branch for the pull request.
-- * 'revisionId' - The system-generated revision ID for the pull request.
-- * 'title' - The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
mkPullRequest ::
  PullRequest
mkPullRequest =
  PullRequest'
    { approvalRules = Lude.Nothing,
      authorARN = Lude.Nothing,
      pullRequestId = Lude.Nothing,
      creationDate = Lude.Nothing,
      pullRequestStatus = Lude.Nothing,
      title = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      lastActivityDate = Lude.Nothing,
      revisionId = Lude.Nothing,
      pullRequestTargets = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The approval rules applied to the pull request.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApprovalRules :: Lens.Lens' PullRequest (Lude.Maybe [ApprovalRule])
prApprovalRules = Lens.lens (approvalRules :: PullRequest -> Lude.Maybe [ApprovalRule]) (\s a -> s {approvalRules = a} :: PullRequest)
{-# DEPRECATED prApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- /Note:/ Consider using 'authorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prAuthorARN :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prAuthorARN = Lens.lens (authorARN :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {authorARN = a} :: PullRequest)
{-# DEPRECATED prAuthorARN "Use generic-lens or generic-optics with 'authorARN' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestId :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prPullRequestId = Lens.lens (pullRequestId :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {pullRequestId = a} :: PullRequest)
{-# DEPRECATED prPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The date and time the pull request was originally created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCreationDate :: Lens.Lens' PullRequest (Lude.Maybe Lude.Timestamp)
prCreationDate = Lens.lens (creationDate :: PullRequest -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: PullRequest)
{-# DEPRECATED prCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestStatus :: Lens.Lens' PullRequest (Lude.Maybe PullRequestStatusEnum)
prPullRequestStatus = Lens.lens (pullRequestStatus :: PullRequest -> Lude.Maybe PullRequestStatusEnum) (\s a -> s {pullRequestStatus = a} :: PullRequest)
{-# DEPRECATED prPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

-- | The user-defined title of the pull request. This title is displayed in the list of pull requests to other repository users.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prTitle :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prTitle = Lens.lens (title :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: PullRequest)
{-# DEPRECATED prTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prClientRequestToken :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prClientRequestToken = Lens.lens (clientRequestToken :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: PullRequest)
{-# DEPRECATED prClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The day and time of the last user or system activity on the pull request, in timestamp format.
--
-- /Note:/ Consider using 'lastActivityDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prLastActivityDate :: Lens.Lens' PullRequest (Lude.Maybe Lude.Timestamp)
prLastActivityDate = Lens.lens (lastActivityDate :: PullRequest -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastActivityDate = a} :: PullRequest)
{-# DEPRECATED prLastActivityDate "Use generic-lens or generic-optics with 'lastActivityDate' instead." #-}

-- | The system-generated revision ID for the pull request.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRevisionId :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prRevisionId = Lens.lens (revisionId :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: PullRequest)
{-# DEPRECATED prRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The targets of the pull request, including the source branch and destination branch for the pull request.
--
-- /Note:/ Consider using 'pullRequestTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPullRequestTargets :: Lens.Lens' PullRequest (Lude.Maybe [PullRequestTarget])
prPullRequestTargets = Lens.lens (pullRequestTargets :: PullRequest -> Lude.Maybe [PullRequestTarget]) (\s a -> s {pullRequestTargets = a} :: PullRequest)
{-# DEPRECATED prPullRequestTargets "Use generic-lens or generic-optics with 'pullRequestTargets' instead." #-}

-- | The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prDescription :: Lens.Lens' PullRequest (Lude.Maybe Lude.Text)
prDescription = Lens.lens (description :: PullRequest -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PullRequest)
{-# DEPRECATED prDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON PullRequest where
  parseJSON =
    Lude.withObject
      "PullRequest"
      ( \x ->
          PullRequest'
            Lude.<$> (x Lude..:? "approvalRules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "authorArn")
            Lude.<*> (x Lude..:? "pullRequestId")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "pullRequestStatus")
            Lude.<*> (x Lude..:? "title")
            Lude.<*> (x Lude..:? "clientRequestToken")
            Lude.<*> (x Lude..:? "lastActivityDate")
            Lude.<*> (x Lude..:? "revisionId")
            Lude.<*> (x Lude..:? "pullRequestTargets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "description")
      )
