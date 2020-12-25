{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchDisassociateApprovalRuleTemplateFromRepositories (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositories,

    -- ** Request lenses
    bdartfrApprovalRuleTemplateName,
    bdartfrRepositoryNames,

    -- * Destructuring the response
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- ** Response lenses
    bdartfrrrsDisassociatedRepositoryNames,
    bdartfrrrsErrors,
    bdartfrrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositories' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositories = BatchDisassociateApprovalRuleTemplateFromRepositories'
  { -- | The name of the template that you want to disassociate from one or more repositories.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The repository names that you want to disassociate from the approval rule template.
    repositoryNames :: [Types.RepositoryName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateApprovalRuleTemplateFromRepositories' value with any optional fields omitted.
mkBatchDisassociateApprovalRuleTemplateFromRepositories ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  BatchDisassociateApprovalRuleTemplateFromRepositories
mkBatchDisassociateApprovalRuleTemplateFromRepositories
  approvalRuleTemplateName =
    BatchDisassociateApprovalRuleTemplateFromRepositories'
      { approvalRuleTemplateName,
        repositoryNames = Core.mempty
      }

-- | The name of the template that you want to disassociate from one or more repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrApprovalRuleTemplateName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories Types.ApprovalRuleTemplateName
bdartfrApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED bdartfrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The repository names that you want to disassociate from the approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositories [Types.RepositoryName]
bdartfrRepositoryNames = Lens.field @"repositoryNames"
{-# DEPRECATED bdartfrRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

instance
  Core.FromJSON
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toJSON BatchDisassociateApprovalRuleTemplateFromRepositories {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just ("repositoryNames" Core..= repositoryNames)
          ]
      )

instance
  Core.AWSRequest
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  type
    Rs BatchDisassociateApprovalRuleTemplateFromRepositories =
      BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.BatchDisassociateApprovalRuleTemplateFromRepositories"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
            Core.<$> (x Core..:? "disassociatedRepositoryNames" Core..!= Core.mempty)
              Core.<*> (x Core..:? "errors" Core..!= Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
  { -- | A list of repository names that have had their association with the template removed.
    disassociatedRepositoryNames :: [Types.RepositoryName],
    -- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
    errors :: [Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' value with any optional fields omitted.
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  responseStatus =
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
      { disassociatedRepositoryNames =
          Core.mempty,
        errors = Core.mempty,
        responseStatus
      }

-- | A list of repository names that have had their association with the template removed.
--
-- /Note:/ Consider using 'disassociatedRepositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsDisassociatedRepositoryNames :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Types.RepositoryName]
bdartfrrrsDisassociatedRepositoryNames = Lens.field @"disassociatedRepositoryNames"
{-# DEPRECATED bdartfrrrsDisassociatedRepositoryNames "Use generic-lens or generic-optics with 'disassociatedRepositoryNames' instead." #-}

-- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsErrors :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
bdartfrrrsErrors = Lens.field @"errors"
{-# DEPRECATED bdartfrrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfrrrsResponseStatus :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse Core.Int
bdartfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdartfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
