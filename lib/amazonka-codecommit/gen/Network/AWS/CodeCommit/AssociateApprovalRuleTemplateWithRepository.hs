{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and a specified repository. Then, the next time a pull request is created in the repository where the destination reference (if specified) matches the destination reference (branch) for the pull request, an approval rule that matches the template conditions is automatically created for that pull request. If no destination references are specified in the template, an approval rule that matches the template contents is created for all pull requests in that repository.
module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
  ( -- * Creating a request
    AssociateApprovalRuleTemplateWithRepository (..),
    mkAssociateApprovalRuleTemplateWithRepository,

    -- ** Request lenses
    aartwrApprovalRuleTemplateName,
    aartwrRepositoryName,

    -- * Destructuring the response
    AssociateApprovalRuleTemplateWithRepositoryResponse (..),
    mkAssociateApprovalRuleTemplateWithRepositoryResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateApprovalRuleTemplateWithRepository' smart constructor.
data AssociateApprovalRuleTemplateWithRepository = AssociateApprovalRuleTemplateWithRepository'
  { -- | The name for the approval rule template.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The name of the repository that you want to associate with the template.
    repositoryName :: Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateApprovalRuleTemplateWithRepository' value with any optional fields omitted.
mkAssociateApprovalRuleTemplateWithRepository ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  -- | 'repositoryName'
  Types.RepositoryName ->
  AssociateApprovalRuleTemplateWithRepository
mkAssociateApprovalRuleTemplateWithRepository
  approvalRuleTemplateName
  repositoryName =
    AssociateApprovalRuleTemplateWithRepository'
      { approvalRuleTemplateName,
        repositoryName
      }

-- | The name for the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aartwrApprovalRuleTemplateName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Types.ApprovalRuleTemplateName
aartwrApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED aartwrApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The name of the repository that you want to associate with the template.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aartwrRepositoryName :: Lens.Lens' AssociateApprovalRuleTemplateWithRepository Types.RepositoryName
aartwrRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED aartwrRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON AssociateApprovalRuleTemplateWithRepository where
  toJSON AssociateApprovalRuleTemplateWithRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance
  Core.AWSRequest
    AssociateApprovalRuleTemplateWithRepository
  where
  type
    Rs AssociateApprovalRuleTemplateWithRepository =
      AssociateApprovalRuleTemplateWithRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.AssociateApprovalRuleTemplateWithRepository"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull
      AssociateApprovalRuleTemplateWithRepositoryResponse'

-- | /See:/ 'mkAssociateApprovalRuleTemplateWithRepositoryResponse' smart constructor.
data AssociateApprovalRuleTemplateWithRepositoryResponse = AssociateApprovalRuleTemplateWithRepositoryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateApprovalRuleTemplateWithRepositoryResponse' value with any optional fields omitted.
mkAssociateApprovalRuleTemplateWithRepositoryResponse ::
  AssociateApprovalRuleTemplateWithRepositoryResponse
mkAssociateApprovalRuleTemplateWithRepositoryResponse =
  AssociateApprovalRuleTemplateWithRepositoryResponse'
