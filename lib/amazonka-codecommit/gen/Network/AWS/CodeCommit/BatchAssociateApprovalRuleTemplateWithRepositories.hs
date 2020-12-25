{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and one or more specified repositories.
module Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
  ( -- * Creating a request
    BatchAssociateApprovalRuleTemplateWithRepositories (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositories,

    -- ** Request lenses
    baartwrApprovalRuleTemplateName,
    baartwrRepositoryNames,

    -- * Destructuring the response
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse,

    -- ** Response lenses
    baartwrrrsAssociatedRepositoryNames,
    baartwrrrsErrors,
    baartwrrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositories' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositories = BatchAssociateApprovalRuleTemplateWithRepositories'
  { -- | The name of the template you want to associate with one or more repositories.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The names of the repositories you want to associate with the template.
    repositoryNames :: [Types.RepositoryName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAssociateApprovalRuleTemplateWithRepositories' value with any optional fields omitted.
mkBatchAssociateApprovalRuleTemplateWithRepositories ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  BatchAssociateApprovalRuleTemplateWithRepositories
mkBatchAssociateApprovalRuleTemplateWithRepositories
  approvalRuleTemplateName =
    BatchAssociateApprovalRuleTemplateWithRepositories'
      { approvalRuleTemplateName,
        repositoryNames = Core.mempty
      }

-- | The name of the template you want to associate with one or more repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrApprovalRuleTemplateName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories Types.ApprovalRuleTemplateName
baartwrApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED baartwrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The names of the repositories you want to associate with the template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositories [Types.RepositoryName]
baartwrRepositoryNames = Lens.field @"repositoryNames"
{-# DEPRECATED baartwrRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

instance
  Core.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  toJSON BatchAssociateApprovalRuleTemplateWithRepositories {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just ("repositoryNames" Core..= repositoryNames)
          ]
      )

instance
  Core.AWSRequest
    BatchAssociateApprovalRuleTemplateWithRepositories
  where
  type
    Rs BatchAssociateApprovalRuleTemplateWithRepositories =
      BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.BatchAssociateApprovalRuleTemplateWithRepositories"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
            Core.<$> (x Core..:? "associatedRepositoryNames" Core..!= Core.mempty)
              Core.<*> (x Core..:? "errors" Core..!= Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesResponse = BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
  { -- | A list of names of the repositories that have been associated with the template.
    associatedRepositoryNames :: [Types.RepositoryName],
    -- | A list of any errors that might have occurred while attempting to create the association between the template and the repositories.
    errors :: [Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAssociateApprovalRuleTemplateWithRepositoriesResponse' value with any optional fields omitted.
mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchAssociateApprovalRuleTemplateWithRepositoriesResponse
mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
  responseStatus =
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'
      { associatedRepositoryNames =
          Core.mempty,
        errors = Core.mempty,
        responseStatus
      }

-- | A list of names of the repositories that have been associated with the template.
--
-- /Note:/ Consider using 'associatedRepositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrrsAssociatedRepositoryNames :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [Types.RepositoryName]
baartwrrrsAssociatedRepositoryNames = Lens.field @"associatedRepositoryNames"
{-# DEPRECATED baartwrrrsAssociatedRepositoryNames "Use generic-lens or generic-optics with 'associatedRepositoryNames' instead." #-}

-- | A list of any errors that might have occurred while attempting to create the association between the template and the repositories.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrrsErrors :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse [Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError]
baartwrrrsErrors = Lens.field @"errors"
{-# DEPRECATED baartwrrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwrrrsResponseStatus :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesResponse Core.Int
baartwrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED baartwrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
