{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on the comparison between two commits.
module Network.AWS.CodeCommit.PostCommentForComparedCommit
    (
    -- * Creating a request
      PostCommentForComparedCommit (..)
    , mkPostCommentForComparedCommit
    -- ** Request lenses
    , pcfccRepositoryName
    , pcfccAfterCommitId
    , pcfccContent
    , pcfccBeforeCommitId
    , pcfccClientRequestToken
    , pcfccLocation

    -- * Destructuring the response
    , PostCommentForComparedCommitResponse (..)
    , mkPostCommentForComparedCommitResponse
    -- ** Response lenses
    , pcfccrrsAfterBlobId
    , pcfccrrsAfterCommitId
    , pcfccrrsBeforeBlobId
    , pcfccrrsBeforeCommitId
    , pcfccrrsComment
    , pcfccrrsLocation
    , pcfccrrsRepositoryName
    , pcfccrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPostCommentForComparedCommit' smart constructor.
data PostCommentForComparedCommit = PostCommentForComparedCommit'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to post a comment on the comparison between commits.
  , afterCommitId :: Types.CommitId
    -- ^ To establish the directionality of the comparison, the full commit ID of the after commit.
  , content :: Types.Content
    -- ^ The content of the comment you want to make.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ To establish the directionality of the comparison, the full commit ID of the before commit. Required for commenting on any commit unless that commit is the initial commit.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
  , location :: Core.Maybe Types.Location
    -- ^ The location of the comparison where you want to comment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostCommentForComparedCommit' value with any optional fields omitted.
mkPostCommentForComparedCommit
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.CommitId -- ^ 'afterCommitId'
    -> Types.Content -- ^ 'content'
    -> PostCommentForComparedCommit
mkPostCommentForComparedCommit repositoryName afterCommitId content
  = PostCommentForComparedCommit'{repositoryName, afterCommitId,
                                  content, beforeCommitId = Core.Nothing,
                                  clientRequestToken = Core.Nothing, location = Core.Nothing}

-- | The name of the repository where you want to post a comment on the comparison between commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccRepositoryName :: Lens.Lens' PostCommentForComparedCommit Types.RepositoryName
pcfccRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pcfccRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | To establish the directionality of the comparison, the full commit ID of the after commit.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccAfterCommitId :: Lens.Lens' PostCommentForComparedCommit Types.CommitId
pcfccAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE pcfccAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | The content of the comment you want to make.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccContent :: Lens.Lens' PostCommentForComparedCommit Types.Content
pcfccContent = Lens.field @"content"
{-# INLINEABLE pcfccContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | To establish the directionality of the comparison, the full commit ID of the before commit. Required for commenting on any commit unless that commit is the initial commit.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccBeforeCommitId :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Types.CommitId)
pcfccBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE pcfccBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccClientRequestToken :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Types.ClientRequestToken)
pcfccClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE pcfccClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The location of the comparison where you want to comment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccLocation :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Types.Location)
pcfccLocation = Lens.field @"location"
{-# INLINEABLE pcfccLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.ToQuery PostCommentForComparedCommit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PostCommentForComparedCommit where
        toHeaders PostCommentForComparedCommit{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.PostCommentForComparedCommit")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PostCommentForComparedCommit where
        toJSON PostCommentForComparedCommit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("afterCommitId" Core..= afterCommitId),
                  Core.Just ("content" Core..= content),
                  ("beforeCommitId" Core..=) Core.<$> beforeCommitId,
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("location" Core..=) Core.<$> location])

instance Core.AWSRequest PostCommentForComparedCommit where
        type Rs PostCommentForComparedCommit =
             PostCommentForComparedCommitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PostCommentForComparedCommitResponse' Core.<$>
                   (x Core..:? "afterBlobId") Core.<*> x Core..:? "afterCommitId"
                     Core.<*> x Core..:? "beforeBlobId"
                     Core.<*> x Core..:? "beforeCommitId"
                     Core.<*> x Core..:? "comment"
                     Core.<*> x Core..:? "location"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPostCommentForComparedCommitResponse' smart constructor.
data PostCommentForComparedCommitResponse = PostCommentForComparedCommitResponse'
  { afterBlobId :: Core.Maybe Types.AfterBlobId
    -- ^ In the directionality you established, the blob ID of the after blob.
  , afterCommitId :: Core.Maybe Types.CommitId
    -- ^ In the directionality you established, the full commit ID of the after commit.
  , beforeBlobId :: Core.Maybe Types.BeforeBlobId
    -- ^ In the directionality you established, the blob ID of the before blob.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ In the directionality you established, the full commit ID of the before commit.
  , comment :: Core.Maybe Types.Comment
    -- ^ The content of the comment you posted.
  , location :: Core.Maybe Types.Location
    -- ^ The location of the comment in the comparison between the two commits.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository where you posted a comment on the comparison between commits.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PostCommentForComparedCommitResponse' value with any optional fields omitted.
mkPostCommentForComparedCommitResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PostCommentForComparedCommitResponse
mkPostCommentForComparedCommitResponse responseStatus
  = PostCommentForComparedCommitResponse'{afterBlobId = Core.Nothing,
                                          afterCommitId = Core.Nothing, beforeBlobId = Core.Nothing,
                                          beforeCommitId = Core.Nothing, comment = Core.Nothing,
                                          location = Core.Nothing, repositoryName = Core.Nothing,
                                          responseStatus}

-- | In the directionality you established, the blob ID of the after blob.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsAfterBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.AfterBlobId)
pcfccrrsAfterBlobId = Lens.field @"afterBlobId"
{-# INLINEABLE pcfccrrsAfterBlobId #-}
{-# DEPRECATED afterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead"  #-}

-- | In the directionality you established, the full commit ID of the after commit.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsAfterCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.CommitId)
pcfccrrsAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE pcfccrrsAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | In the directionality you established, the blob ID of the before blob.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsBeforeBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.BeforeBlobId)
pcfccrrsBeforeBlobId = Lens.field @"beforeBlobId"
{-# INLINEABLE pcfccrrsBeforeBlobId #-}
{-# DEPRECATED beforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead"  #-}

-- | In the directionality you established, the full commit ID of the before commit.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsBeforeCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.CommitId)
pcfccrrsBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE pcfccrrsBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | The content of the comment you posted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsComment :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.Comment)
pcfccrrsComment = Lens.field @"comment"
{-# INLINEABLE pcfccrrsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The location of the comment in the comparison between the two commits.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsLocation :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.Location)
pcfccrrsLocation = Lens.field @"location"
{-# INLINEABLE pcfccrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the repository where you posted a comment on the comparison between commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsRepositoryName :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Types.RepositoryName)
pcfccrrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pcfccrrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrrsResponseStatus :: Lens.Lens' PostCommentForComparedCommitResponse Core.Int
pcfccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcfccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
