{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all repositories associated with the specified approval rule template.
module Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
    (
    -- * Creating a request
      ListRepositoriesForApprovalRuleTemplate (..)
    , mkListRepositoriesForApprovalRuleTemplate
    -- ** Request lenses
    , lrfartApprovalRuleTemplateName
    , lrfartMaxResults
    , lrfartNextToken

    -- * Destructuring the response
    , ListRepositoriesForApprovalRuleTemplateResponse (..)
    , mkListRepositoriesForApprovalRuleTemplateResponse
    -- ** Response lenses
    , lrfartrrsNextToken
    , lrfartrrsRepositoryNames
    , lrfartrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The name of the approval rule template for which you want to list repositories that are associated with that template.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositoriesForApprovalRuleTemplate' value with any optional fields omitted.
mkListRepositoriesForApprovalRuleTemplate
    :: Types.ApprovalRuleTemplateName -- ^ 'approvalRuleTemplateName'
    -> ListRepositoriesForApprovalRuleTemplate
mkListRepositoriesForApprovalRuleTemplate approvalRuleTemplateName
  = ListRepositoriesForApprovalRuleTemplate'{approvalRuleTemplateName,
                                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the approval rule template for which you want to list repositories that are associated with that template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartApprovalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Types.ApprovalRuleTemplateName
lrfartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# INLINEABLE lrfartApprovalRuleTemplateName #-}
{-# DEPRECATED approvalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartMaxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Core.Int)
lrfartMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrfartMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Types.NextToken)
lrfartNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrfartNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListRepositoriesForApprovalRuleTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRepositoriesForApprovalRuleTemplate
         where
        toHeaders ListRepositoriesForApprovalRuleTemplate{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRepositoriesForApprovalRuleTemplate
         where
        toJSON ListRepositoriesForApprovalRuleTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListRepositoriesForApprovalRuleTemplate
         where
        type Rs ListRepositoriesForApprovalRuleTemplate =
             ListRepositoriesForApprovalRuleTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRepositoriesForApprovalRuleTemplateResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "repositoryNames"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplateResponse' smart constructor.
data ListRepositoriesForApprovalRuleTemplateResponse = ListRepositoriesForApprovalRuleTemplateResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that allows the operation to batch the next results of the operation.
  , repositoryNames :: Core.Maybe [Types.RepositoryName]
    -- ^ A list of repository names that are associated with the specified approval rule template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositoriesForApprovalRuleTemplateResponse' value with any optional fields omitted.
mkListRepositoriesForApprovalRuleTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRepositoriesForApprovalRuleTemplateResponse
mkListRepositoriesForApprovalRuleTemplateResponse responseStatus
  = ListRepositoriesForApprovalRuleTemplateResponse'{nextToken =
                                                       Core.Nothing,
                                                     repositoryNames = Core.Nothing, responseStatus}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe Types.NextToken)
lrfartrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrfartrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of repository names that are associated with the specified approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsRepositoryNames :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe [Types.RepositoryName])
lrfartrrsRepositoryNames = Lens.field @"repositoryNames"
{-# INLINEABLE lrfartrrsRepositoryNames #-}
{-# DEPRECATED repositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsResponseStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Core.Int
lrfartrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrfartrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
