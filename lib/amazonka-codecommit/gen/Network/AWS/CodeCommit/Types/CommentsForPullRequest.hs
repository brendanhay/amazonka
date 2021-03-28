{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.CommentsForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.CommentsForPullRequest
  ( CommentsForPullRequest (..)
  -- * Smart constructor
  , mkCommentsForPullRequest
  -- * Lenses
  , cfprAfterBlobId
  , cfprAfterCommitId
  , cfprBeforeBlobId
  , cfprBeforeCommitId
  , cfprComments
  , cfprLocation
  , cfprPullRequestId
  , cfprRepositoryName
  ) where

import qualified Network.AWS.CodeCommit.Types.AfterBlobId as Types
import qualified Network.AWS.CodeCommit.Types.BeforeBlobId as Types
import qualified Network.AWS.CodeCommit.Types.Comment as Types
import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.Location as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about comments on a pull request.
--
-- /See:/ 'mkCommentsForPullRequest' smart constructor.
data CommentsForPullRequest = CommentsForPullRequest'
  { afterBlobId :: Core.Maybe Types.AfterBlobId
    -- ^ The full blob ID of the file on which you want to comment on the source commit.
  , afterCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit that was the tip of the source branch at the time the comment was made. 
  , beforeBlobId :: Core.Maybe Types.BeforeBlobId
    -- ^ The full blob ID of the file on which you want to comment on the destination commit.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit is superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
  , comments :: Core.Maybe [Types.Comment]
    -- ^ An array of comment objects. Each comment object contains information about a comment on the pull request.
  , location :: Core.Maybe Types.Location
    -- ^ Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is BEFORE (destination branch) or AFTER (source branch).
  , pullRequestId :: Core.Maybe Types.PullRequestId
    -- ^ The system-generated ID of the pull request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository that contains the pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CommentsForPullRequest' value with any optional fields omitted.
mkCommentsForPullRequest
    :: CommentsForPullRequest
mkCommentsForPullRequest
  = CommentsForPullRequest'{afterBlobId = Core.Nothing,
                            afterCommitId = Core.Nothing, beforeBlobId = Core.Nothing,
                            beforeCommitId = Core.Nothing, comments = Core.Nothing,
                            location = Core.Nothing, pullRequestId = Core.Nothing,
                            repositoryName = Core.Nothing}

-- | The full blob ID of the file on which you want to comment on the source commit.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprAfterBlobId :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.AfterBlobId)
cfprAfterBlobId = Lens.field @"afterBlobId"
{-# INLINEABLE cfprAfterBlobId #-}
{-# DEPRECATED afterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead"  #-}

-- | The full commit ID of the commit that was the tip of the source branch at the time the comment was made. 
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprAfterCommitId :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.CommitId)
cfprAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE cfprAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | The full blob ID of the file on which you want to comment on the destination commit.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprBeforeBlobId :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.BeforeBlobId)
cfprBeforeBlobId = Lens.field @"beforeBlobId"
{-# INLINEABLE cfprBeforeBlobId #-}
{-# DEPRECATED beforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead"  #-}

-- | The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit is superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprBeforeCommitId :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.CommitId)
cfprBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE cfprBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | An array of comment objects. Each comment object contains information about a comment on the pull request.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprComments :: Lens.Lens' CommentsForPullRequest (Core.Maybe [Types.Comment])
cfprComments = Lens.field @"comments"
{-# INLINEABLE cfprComments #-}
{-# DEPRECATED comments "Use generic-lens or generic-optics with 'comments' instead"  #-}

-- | Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is BEFORE (destination branch) or AFTER (source branch).
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprLocation :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.Location)
cfprLocation = Lens.field @"location"
{-# INLINEABLE cfprLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprPullRequestId :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.PullRequestId)
cfprPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE cfprPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprRepositoryName :: Lens.Lens' CommentsForPullRequest (Core.Maybe Types.RepositoryName)
cfprRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE cfprRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.FromJSON CommentsForPullRequest where
        parseJSON
          = Core.withObject "CommentsForPullRequest" Core.$
              \ x ->
                CommentsForPullRequest' Core.<$>
                  (x Core..:? "afterBlobId") Core.<*> x Core..:? "afterCommitId"
                    Core.<*> x Core..:? "beforeBlobId"
                    Core.<*> x Core..:? "beforeCommitId"
                    Core.<*> x Core..:? "comments"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "pullRequestId"
                    Core.<*> x Core..:? "repositoryName"
