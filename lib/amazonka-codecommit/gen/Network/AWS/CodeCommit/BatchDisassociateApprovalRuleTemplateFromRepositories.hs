{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between an approval rule template and one or more specified repositories. 
module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
    (
    -- * Creating a request
      BatchDisassociateApprovalRuleTemplateFromRepositories (..)
    , mkBatchDisassociateApprovalRuleTemplateFromRepositories
    -- ** Request lenses
    , bdartfrApprovalRuleTemplateName
    , bdartfrRepositoryNames

    -- * Destructuring the response
    , BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (..)
    , mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
    -- ** Response lenses
    , bdartfrrrsDisassociatedRepositoryNames
    , bdartfrrrsErrors
    , bdartfrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositories' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositories = BatchDisassociateApprovalRuleTemplateFromRepositories'
  { approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The name of the template that you want to disassociate from one or more repositories.
  , repositoryNames :: [Types.RepositoryName]
    -- ^ The repository names that you want to disassociate from the approval rule template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateApprovalRuleTemplateFromRepositories' value with any optional fields omitted.
mkBatchDisassociateApprovalRuleTemplateFromRepositories
    :: Types.ApprovalRuleTemplateName -- ^ 'approvalRuleTemplateName'
    -> BatchDisassociateApprovalRuleTemplateFromRepositories
mkBatchDisassociateApprovalRuleTemplateFromRepositories
  approvalRuleTemplateName
  = BatchDisassociateApprovalRuleTemplateFromRepositories'{approvalRuleTemplateName,
                                                           repositoryNames = Core.mempty}

-- | The name of the template that you want to disassociate from one or more repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrApprovalRuleTemplateName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories Types.ApprovalRuleTemplateName
bdartfrApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# INLINEABLE bdartfrApprovalRuleTemplateName #-}
{-# DEPRECATED approvalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead"  #-}

-- | The repository names that you want to disassociate from the approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories [Types.RepositoryName]
bdartfrRepositoryNames = Lens.field @"repositoryNames"
{-# INLINEABLE bdartfrRepositoryNames #-}
{-# DEPRECATED repositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead"  #-}

instance Core.ToQuery
           BatchDisassociateApprovalRuleTemplateFromRepositories
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           BatchDisassociateApprovalRuleTemplateFromRepositories
         where
        toHeaders BatchDisassociateApprovalRuleTemplateFromRepositories{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.BatchDisassociateApprovalRuleTemplateFromRepositories")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           BatchDisassociateApprovalRuleTemplateFromRepositories
         where
        toJSON BatchDisassociateApprovalRuleTemplateFromRepositories{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
                  Core.Just ("repositoryNames" Core..= repositoryNames)])

instance Core.AWSRequest
           BatchDisassociateApprovalRuleTemplateFromRepositories
         where
        type Rs BatchDisassociateApprovalRuleTemplateFromRepositories =
             BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
                   Core.<$>
                   (x Core..:? "disassociatedRepositoryNames" Core..!= Core.mempty)
                     Core.<*> x Core..:? "errors" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
  { disassociatedRepositoryNames :: [Types.RepositoryName]
    -- ^ A list of repository names that have had their association with the template removed.
  , errors :: [Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
    -- ^ A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' value with any optional fields omitted.
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  responseStatus
  = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'{disassociatedRepositoryNames
                                                                     = Core.mempty,
                                                                   errors = Core.mempty,
                                                                   responseStatus}

-- | A list of repository names that have had their association with the template removed.
--
-- /Note:/ Consider using 'disassociatedRepositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsDisassociatedRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Types.RepositoryName]
bdartfrrrsDisassociatedRepositoryNames = Lens.field @"disassociatedRepositoryNames"
{-# INLINEABLE bdartfrrrsDisassociatedRepositoryNames #-}
{-# DEPRECATED disassociatedRepositoryNames "Use generic-lens or generic-optics with 'disassociatedRepositoryNames' instead"  #-}

-- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsErrors :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
bdartfrrrsErrors = Lens.field @"errors"
{-# INLINEABLE bdartfrrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsResponseStatus :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse Core.Int
bdartfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdartfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
