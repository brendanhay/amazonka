{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentReactions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reactions to a specified comment ID. Reactions from users who have been deleted will not be included in the count.
module Network.AWS.CodeCommit.GetCommentReactions
    (
    -- * Creating a request
      GetCommentReactions (..)
    , mkGetCommentReactions
    -- ** Request lenses
    , gcrCommentId
    , gcrMaxResults
    , gcrNextToken
    , gcrReactionUserArn

    -- * Destructuring the response
    , GetCommentReactionsResponse (..)
    , mkGetCommentReactionsResponse
    -- ** Response lenses
    , gcrrrsReactionsForComment
    , gcrrrsNextToken
    , gcrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCommentReactions' smart constructor.
data GetCommentReactions = GetCommentReactions'
  { commentId :: Types.CommentId
    -- ^ The ID of the comment for which you want to get reactions information.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results. 
  , reactionUserArn :: Core.Maybe Types.Arn
    -- ^ Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommentReactions' value with any optional fields omitted.
mkGetCommentReactions
    :: Types.CommentId -- ^ 'commentId'
    -> GetCommentReactions
mkGetCommentReactions commentId
  = GetCommentReactions'{commentId, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, reactionUserArn = Core.Nothing}

-- | The ID of the comment for which you want to get reactions information.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrCommentId :: Lens.Lens' GetCommentReactions Types.CommentId
gcrCommentId = Lens.field @"commentId"
{-# INLINEABLE gcrCommentId #-}
{-# DEPRECATED commentId "Use generic-lens or generic-optics with 'commentId' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrMaxResults :: Lens.Lens' GetCommentReactions (Core.Maybe Core.Int)
gcrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrNextToken :: Lens.Lens' GetCommentReactions (Core.Maybe Types.NextToken)
gcrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
--
-- /Note:/ Consider using 'reactionUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrReactionUserArn :: Lens.Lens' GetCommentReactions (Core.Maybe Types.Arn)
gcrReactionUserArn = Lens.field @"reactionUserArn"
{-# INLINEABLE gcrReactionUserArn #-}
{-# DEPRECATED reactionUserArn "Use generic-lens or generic-optics with 'reactionUserArn' instead"  #-}

instance Core.ToQuery GetCommentReactions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCommentReactions where
        toHeaders GetCommentReactions{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.GetCommentReactions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCommentReactions where
        toJSON GetCommentReactions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("commentId" Core..= commentId),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("reactionUserArn" Core..=) Core.<$> reactionUserArn])

instance Core.AWSRequest GetCommentReactions where
        type Rs GetCommentReactions = GetCommentReactionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCommentReactionsResponse' Core.<$>
                   (x Core..:? "reactionsForComment" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCommentReactionsResponse' smart constructor.
data GetCommentReactionsResponse = GetCommentReactionsResponse'
  { reactionsForComment :: [Types.ReactionForComment]
    -- ^ An array of reactions to the specified comment.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that can be used in a request to return the next batch of the results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommentReactionsResponse' value with any optional fields omitted.
mkGetCommentReactionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCommentReactionsResponse
mkGetCommentReactionsResponse responseStatus
  = GetCommentReactionsResponse'{reactionsForComment = Core.mempty,
                                 nextToken = Core.Nothing, responseStatus}

-- | An array of reactions to the specified comment.
--
-- /Note:/ Consider using 'reactionsForComment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsReactionsForComment :: Lens.Lens' GetCommentReactionsResponse [Types.ReactionForComment]
gcrrrsReactionsForComment = Lens.field @"reactionsForComment"
{-# INLINEABLE gcrrrsReactionsForComment #-}
{-# DEPRECATED reactionsForComment "Use generic-lens or generic-optics with 'reactionsForComment' instead"  #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsNextToken :: Lens.Lens' GetCommentReactionsResponse (Core.Maybe Types.NextToken)
gcrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsResponseStatus :: Lens.Lens' GetCommentReactionsResponse Core.Int
gcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
