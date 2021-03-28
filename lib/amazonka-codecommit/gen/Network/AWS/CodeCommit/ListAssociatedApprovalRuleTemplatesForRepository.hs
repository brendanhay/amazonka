{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates that are associated with a specified repository.
module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
    (
    -- * Creating a request
      ListAssociatedApprovalRuleTemplatesForRepository (..)
    , mkListAssociatedApprovalRuleTemplatesForRepository
    -- ** Request lenses
    , laartfrRepositoryName
    , laartfrMaxResults
    , laartfrNextToken

    -- * Destructuring the response
    , ListAssociatedApprovalRuleTemplatesForRepositoryResponse (..)
    , mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
    -- ** Response lenses
    , laartfrrrsApprovalRuleTemplateNames
    , laartfrrrsNextToken
    , laartfrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository for which you want to list all associated approval rule templates.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedApprovalRuleTemplatesForRepository' value with any optional fields omitted.
mkListAssociatedApprovalRuleTemplatesForRepository
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> ListAssociatedApprovalRuleTemplatesForRepository
mkListAssociatedApprovalRuleTemplatesForRepository repositoryName
  = ListAssociatedApprovalRuleTemplatesForRepository'{repositoryName,
                                                      maxResults = Core.Nothing,
                                                      nextToken = Core.Nothing}

-- | The name of the repository for which you want to list all associated approval rule templates.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrRepositoryName :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository Types.RepositoryName
laartfrRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE laartfrRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrMaxResults :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Core.Int)
laartfrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laartfrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Types.NextToken)
laartfrNextToken = Lens.field @"nextToken"
{-# INLINEABLE laartfrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery
           ListAssociatedApprovalRuleTemplatesForRepository
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           ListAssociatedApprovalRuleTemplatesForRepository
         where
        toHeaders ListAssociatedApprovalRuleTemplatesForRepository{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           ListAssociatedApprovalRuleTemplatesForRepository
         where
        toJSON ListAssociatedApprovalRuleTemplatesForRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest
           ListAssociatedApprovalRuleTemplatesForRepository
         where
        type Rs ListAssociatedApprovalRuleTemplatesForRepository =
             ListAssociatedApprovalRuleTemplatesForRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssociatedApprovalRuleTemplatesForRepositoryResponse' Core.<$>
                   (x Core..:? "approvalRuleTemplateNames") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { approvalRuleTemplateNames :: Core.Maybe [Types.ApprovalRuleTemplateName]
    -- ^ The names of all approval rule templates associated with the repository.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that allows the operation to batch the next results of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedApprovalRuleTemplatesForRepositoryResponse' value with any optional fields omitted.
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssociatedApprovalRuleTemplatesForRepositoryResponse
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
  responseStatus
  = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'{approvalRuleTemplateNames
                                                                = Core.Nothing,
                                                              nextToken = Core.Nothing,
                                                              responseStatus}

-- | The names of all approval rule templates associated with the repository.
--
-- /Note:/ Consider using 'approvalRuleTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsApprovalRuleTemplateNames :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe [Types.ApprovalRuleTemplateName])
laartfrrrsApprovalRuleTemplateNames = Lens.field @"approvalRuleTemplateNames"
{-# INLINEABLE laartfrrrsApprovalRuleTemplateNames #-}
{-# DEPRECATED approvalRuleTemplateNames "Use generic-lens or generic-optics with 'approvalRuleTemplateNames' instead"  #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe Types.NextToken)
laartfrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE laartfrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsResponseStatus :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Core.Int
laartfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE laartfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
