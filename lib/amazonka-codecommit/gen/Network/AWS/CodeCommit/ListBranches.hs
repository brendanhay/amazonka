{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListBranches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more branches in a repository.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListBranches
    (
    -- * Creating a request
      ListBranches (..)
    , mkListBranches
    -- ** Request lenses
    , lbRepositoryName
    , lbNextToken

    -- * Destructuring the response
    , ListBranchesResponse (..)
    , mkListBranchesResponse
    -- ** Response lenses
    , lbrrsBranches
    , lbrrsNextToken
    , lbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a list branches operation.
--
-- /See:/ 'mkListBranches' smart constructor.
data ListBranches = ListBranches'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the branches.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that allows the operation to batch the results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBranches' value with any optional fields omitted.
mkListBranches
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> ListBranches
mkListBranches repositoryName
  = ListBranches'{repositoryName, nextToken = Core.Nothing}

-- | The name of the repository that contains the branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbRepositoryName :: Lens.Lens' ListBranches Types.RepositoryName
lbRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE lbRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | An enumeration token that allows the operation to batch the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBranches (Core.Maybe Types.NextToken)
lbNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBranches where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBranches where
        toHeaders ListBranches{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.ListBranches")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBranches where
        toJSON ListBranches{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListBranches where
        type Rs ListBranches = ListBranchesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBranchesResponse' Core.<$>
                   (x Core..:? "branches") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBranches where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"branches" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a list branches operation.
--
-- /See:/ 'mkListBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
  { branches :: Core.Maybe [Types.BranchName]
    -- ^ The list of branch names.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that returns the batch of the results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBranchesResponse' value with any optional fields omitted.
mkListBranchesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBranchesResponse
mkListBranchesResponse responseStatus
  = ListBranchesResponse'{branches = Core.Nothing,
                          nextToken = Core.Nothing, responseStatus}

-- | The list of branch names.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsBranches :: Lens.Lens' ListBranchesResponse (Core.Maybe [Types.BranchName])
lbrrsBranches = Lens.field @"branches"
{-# INLINEABLE lbrrsBranches #-}
{-# DEPRECATED branches "Use generic-lens or generic-optics with 'branches' instead"  #-}

-- | An enumeration token that returns the batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsNextToken :: Lens.Lens' ListBranchesResponse (Core.Maybe Types.NextToken)
lbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsResponseStatus :: Lens.Lens' ListBranchesResponse Core.Int
lbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
