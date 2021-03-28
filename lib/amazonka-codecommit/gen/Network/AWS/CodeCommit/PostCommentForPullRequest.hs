{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on a pull request.
module Network.AWS.CodeCommit.PostCommentForPullRequest
    (
    -- * Creating a request
      PostCommentForPullRequest (..)
    , mkPostCommentForPullRequest
    -- ** Request lenses
    , pcfprPullRequestId
    , pcfprRepositoryName
    , pcfprBeforeCommitId
    , pcfprAfterCommitId
    , pcfprContent
    , pcfprClientRequestToken
    , pcfprLocation

    -- * Destructuring the response
    , PostCommentForPullRequestResponse (..)
    , mkPostCommentForPullRequestResponse
    -- ** Response lenses
    , pcfprrrsAfterBlobId
    , pcfprrrsAfterCommitId
    , pcfprrrsBeforeBlobId
    , pcfprrrsBeforeCommitId
    , pcfprrrsComment
    , pcfprrrsLocation
    , pcfprrrsPullRequestId
    , pcfprrrsRepositoryName
    , pcfprrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPostCommentForPullRequest' smart constructor.
data PostCommentForPullRequest = PostCommentForPullRequest'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to post a comment on a pull request.
  , beforeCommitId :: Types.CommitId
    -- ^ The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
  , afterCommitId :: Types.CommitId
    -- ^ The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
  , content :: Types.Content
    -- ^ The content of your comment on the change.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
  , location :: Core.Maybe Types.Location
    -- ^ The location of the change where you want to post your comment. If no location is provided, the comment is posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostCommentForPullRequest' value with any optional fields omitted.
mkPostCommentForPullRequest
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RepositoryName -- ^ 'repositoryName'
    -> Types.CommitId -- ^ 'beforeCommitId'
    -> Types.CommitId -- ^ 'afterCommitId'
    -> Types.Content -- ^ 'content'
    -> PostCommentForPullRequest
mkPostCommentForPullRequest pullRequestId repositoryName
  beforeCommitId afterCommitId content
  = PostCommentForPullRequest'{pullRequestId, repositoryName,
                               beforeCommitId, afterCommitId, content,
                               clientRequestToken = Core.Nothing, location = Core.Nothing}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprPullRequestId :: Lens.Lens' PostCommentForPullRequest Types.PullRequestId
pcfprPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE pcfprPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name of the repository where you want to post a comment on a pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprRepositoryName :: Lens.Lens' PostCommentForPullRequest Types.RepositoryName
pcfprRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pcfprRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprBeforeCommitId :: Lens.Lens' PostCommentForPullRequest Types.CommitId
pcfprBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE pcfprBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprAfterCommitId :: Lens.Lens' PostCommentForPullRequest Types.CommitId
pcfprAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE pcfprAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | The content of your comment on the change.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprContent :: Lens.Lens' PostCommentForPullRequest Types.Content
pcfprContent = Lens.field @"content"
{-# INLINEABLE pcfprContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprClientRequestToken :: Lens.Lens' PostCommentForPullRequest (Core.Maybe Types.ClientRequestToken)
pcfprClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE pcfprClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The location of the change where you want to post your comment. If no location is provided, the comment is posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprLocation :: Lens.Lens' PostCommentForPullRequest (Core.Maybe Types.Location)
pcfprLocation = Lens.field @"location"
{-# INLINEABLE pcfprLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.ToQuery PostCommentForPullRequest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PostCommentForPullRequest where
        toHeaders PostCommentForPullRequest{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.PostCommentForPullRequest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PostCommentForPullRequest where
        toJSON PostCommentForPullRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("beforeCommitId" Core..= beforeCommitId),
                  Core.Just ("afterCommitId" Core..= afterCommitId),
                  Core.Just ("content" Core..= content),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("location" Core..=) Core.<$> location])

instance Core.AWSRequest PostCommentForPullRequest where
        type Rs PostCommentForPullRequest =
             PostCommentForPullRequestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PostCommentForPullRequestResponse' Core.<$>
                   (x Core..:? "afterBlobId") Core.<*> x Core..:? "afterCommitId"
                     Core.<*> x Core..:? "beforeBlobId"
                     Core.<*> x Core..:? "beforeCommitId"
                     Core.<*> x Core..:? "comment"
                     Core.<*> x Core..:? "location"
                     Core.<*> x Core..:? "pullRequestId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPostCommentForPullRequestResponse' smart constructor.
data PostCommentForPullRequestResponse = PostCommentForPullRequestResponse'
  { afterBlobId :: Core.Maybe Types.ObjectId
    -- ^ In the directionality of the pull request, the blob ID of the after blob.
  , afterCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit in the destination branch where the pull request is merged.
  , beforeBlobId :: Core.Maybe Types.ObjectId
    -- ^ In the directionality of the pull request, the blob ID of the before blob.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
  , comment :: Core.Maybe Types.Comment
    -- ^ The content of the comment you posted.
  , location :: Core.Maybe Types.Location
    -- ^ The location of the change where you posted your comment.
  , pullRequestId :: Core.Maybe Types.PullRequestId
    -- ^ The system-generated ID of the pull request. 
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository where you posted a comment on a pull request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PostCommentForPullRequestResponse' value with any optional fields omitted.
mkPostCommentForPullRequestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PostCommentForPullRequestResponse
mkPostCommentForPullRequestResponse responseStatus
  = PostCommentForPullRequestResponse'{afterBlobId = Core.Nothing,
                                       afterCommitId = Core.Nothing, beforeBlobId = Core.Nothing,
                                       beforeCommitId = Core.Nothing, comment = Core.Nothing,
                                       location = Core.Nothing, pullRequestId = Core.Nothing,
                                       repositoryName = Core.Nothing, responseStatus}

-- | In the directionality of the pull request, the blob ID of the after blob.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsAfterBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.ObjectId)
pcfprrrsAfterBlobId = Lens.field @"afterBlobId"
{-# INLINEABLE pcfprrrsAfterBlobId #-}
{-# DEPRECATED afterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead"  #-}

-- | The full commit ID of the commit in the destination branch where the pull request is merged.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsAfterCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.CommitId)
pcfprrrsAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE pcfprrrsAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | In the directionality of the pull request, the blob ID of the before blob.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsBeforeBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.ObjectId)
pcfprrrsBeforeBlobId = Lens.field @"beforeBlobId"
{-# INLINEABLE pcfprrrsBeforeBlobId #-}
{-# DEPRECATED beforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead"  #-}

-- | The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsBeforeCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.CommitId)
pcfprrrsBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE pcfprrrsBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | The content of the comment you posted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsComment :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.Comment)
pcfprrrsComment = Lens.field @"comment"
{-# INLINEABLE pcfprrrsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The location of the change where you posted your comment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsLocation :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.Location)
pcfprrrsLocation = Lens.field @"location"
{-# INLINEABLE pcfprrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The system-generated ID of the pull request. 
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsPullRequestId :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.PullRequestId)
pcfprrrsPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE pcfprrrsPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name of the repository where you posted a comment on a pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsRepositoryName :: Lens.Lens' PostCommentForPullRequestResponse (Core.Maybe Types.RepositoryName)
pcfprrrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pcfprrrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrrsResponseStatus :: Lens.Lens' PostCommentForPullRequestResponse Core.Int
pcfprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcfprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
