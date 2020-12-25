{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListRepositoriesForApprovalRuleTemplate (..),
    mkListRepositoriesForApprovalRuleTemplate,

    -- ** Request lenses
    lrfartApprovalRuleTemplateName,
    lrfartMaxResults,
    lrfartNextToken,

    -- * Destructuring the response
    ListRepositoriesForApprovalRuleTemplateResponse (..),
    mkListRepositoriesForApprovalRuleTemplateResponse,

    -- ** Response lenses
    lrfartrrsNextToken,
    lrfartrrsRepositoryNames,
    lrfartrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplate' smart constructor.
data ListRepositoriesForApprovalRuleTemplate = ListRepositoriesForApprovalRuleTemplate'
  { -- | The name of the approval rule template for which you want to list repositories that are associated with that template.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositoriesForApprovalRuleTemplate' value with any optional fields omitted.
mkListRepositoriesForApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  ListRepositoriesForApprovalRuleTemplate
mkListRepositoriesForApprovalRuleTemplate approvalRuleTemplateName =
  ListRepositoriesForApprovalRuleTemplate'
    { approvalRuleTemplateName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the approval rule template for which you want to list repositories that are associated with that template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartApprovalRuleTemplateName :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate Types.ApprovalRuleTemplateName
lrfartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED lrfartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartMaxResults :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Core.Int)
lrfartMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrfartMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplate (Core.Maybe Types.NextToken)
lrfartNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrfartNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListRepositoriesForApprovalRuleTemplate where
  toJSON ListRepositoriesForApprovalRuleTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListRepositoriesForApprovalRuleTemplate where
  type
    Rs ListRepositoriesForApprovalRuleTemplate =
      ListRepositoriesForApprovalRuleTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.ListRepositoriesForApprovalRuleTemplate"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesForApprovalRuleTemplateResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "repositoryNames")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListRepositoriesForApprovalRuleTemplateResponse' smart constructor.
data ListRepositoriesForApprovalRuleTemplateResponse = ListRepositoriesForApprovalRuleTemplateResponse'
  { -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of repository names that are associated with the specified approval rule template.
    repositoryNames :: Core.Maybe [Types.RepositoryName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositoriesForApprovalRuleTemplateResponse' value with any optional fields omitted.
mkListRepositoriesForApprovalRuleTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRepositoriesForApprovalRuleTemplateResponse
mkListRepositoriesForApprovalRuleTemplateResponse responseStatus =
  ListRepositoriesForApprovalRuleTemplateResponse'
    { nextToken =
        Core.Nothing,
      repositoryNames = Core.Nothing,
      responseStatus
    }

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsNextToken :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe Types.NextToken)
lrfartrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrfartrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of repository names that are associated with the specified approval rule template.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsRepositoryNames :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse (Core.Maybe [Types.RepositoryName])
lrfartrrsRepositoryNames = Lens.field @"repositoryNames"
{-# DEPRECATED lrfartrrsRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfartrrsResponseStatus :: Lens.Lens' ListRepositoriesForApprovalRuleTemplateResponse Core.Int
lrfartrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrfartrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
