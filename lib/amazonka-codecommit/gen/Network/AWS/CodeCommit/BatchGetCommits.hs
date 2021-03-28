{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchGetCommits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the contents of one or more commits in a repository.
module Network.AWS.CodeCommit.BatchGetCommits
    (
    -- * Creating a request
      BatchGetCommits (..)
    , mkBatchGetCommits
    -- ** Request lenses
    , bgcCommitIds
    , bgcRepositoryName

    -- * Destructuring the response
    , BatchGetCommitsResponse (..)
    , mkBatchGetCommitsResponse
    -- ** Response lenses
    , bgcrrsCommits
    , bgcrrsErrors
    , bgcrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetCommits' smart constructor.
data BatchGetCommits = BatchGetCommits'
  { commitIds :: [Types.ObjectId]
    -- ^ The full commit IDs of the commits to get information about.
  , repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the commits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetCommits' value with any optional fields omitted.
mkBatchGetCommits
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> BatchGetCommits
mkBatchGetCommits repositoryName
  = BatchGetCommits'{commitIds = Core.mempty, repositoryName}

-- | The full commit IDs of the commits to get information about.
--
-- /Note:/ Consider using 'commitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcCommitIds :: Lens.Lens' BatchGetCommits [Types.ObjectId]
bgcCommitIds = Lens.field @"commitIds"
{-# INLINEABLE bgcCommitIds #-}
{-# DEPRECATED commitIds "Use generic-lens or generic-optics with 'commitIds' instead"  #-}

-- | The name of the repository that contains the commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcRepositoryName :: Lens.Lens' BatchGetCommits Types.RepositoryName
bgcRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE bgcRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.ToQuery BatchGetCommits where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetCommits where
        toHeaders BatchGetCommits{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.BatchGetCommits")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetCommits where
        toJSON BatchGetCommits{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("commitIds" Core..= commitIds),
                  Core.Just ("repositoryName" Core..= repositoryName)])

instance Core.AWSRequest BatchGetCommits where
        type Rs BatchGetCommits = BatchGetCommitsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetCommitsResponse' Core.<$>
                   (x Core..:? "commits") Core.<*> x Core..:? "errors" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetCommitsResponse' smart constructor.
data BatchGetCommitsResponse = BatchGetCommitsResponse'
  { commits :: Core.Maybe [Types.Commit]
    -- ^ An array of commit data type objects, each of which contains information about a specified commit.
  , errors :: Core.Maybe [Types.BatchGetCommitsError]
    -- ^ Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetCommitsResponse' value with any optional fields omitted.
mkBatchGetCommitsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetCommitsResponse
mkBatchGetCommitsResponse responseStatus
  = BatchGetCommitsResponse'{commits = Core.Nothing,
                             errors = Core.Nothing, responseStatus}

-- | An array of commit data type objects, each of which contains information about a specified commit.
--
-- /Note:/ Consider using 'commits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsCommits :: Lens.Lens' BatchGetCommitsResponse (Core.Maybe [Types.Commit])
bgcrrsCommits = Lens.field @"commits"
{-# INLINEABLE bgcrrsCommits #-}
{-# DEPRECATED commits "Use generic-lens or generic-optics with 'commits' instead"  #-}

-- | Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsErrors :: Lens.Lens' BatchGetCommitsResponse (Core.Maybe [Types.BatchGetCommitsError])
bgcrrsErrors = Lens.field @"errors"
{-# INLINEABLE bgcrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsResponseStatus :: Lens.Lens' BatchGetCommitsResponse Core.Int
bgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
