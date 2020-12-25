{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListAssociatedApprovalRuleTemplatesForRepository (..),
    mkListAssociatedApprovalRuleTemplatesForRepository,

    -- ** Request lenses
    laartfrRepositoryName,
    laartfrMaxResults,
    laartfrNextToken,

    -- * Destructuring the response
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse (..),
    mkListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- ** Response lenses
    laartfrrrsApprovalRuleTemplateNames,
    laartfrrrsNextToken,
    laartfrrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { -- | The name of the repository for which you want to list all associated approval rule templates.
    repositoryName :: Types.RepositoryName,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedApprovalRuleTemplatesForRepository' value with any optional fields omitted.
mkListAssociatedApprovalRuleTemplatesForRepository ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  ListAssociatedApprovalRuleTemplatesForRepository
mkListAssociatedApprovalRuleTemplatesForRepository repositoryName =
  ListAssociatedApprovalRuleTemplatesForRepository'
    { repositoryName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the repository for which you want to list all associated approval rule templates.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrRepositoryName :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository Types.RepositoryName
laartfrRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED laartfrRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrMaxResults :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Core.Int)
laartfrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laartfrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepository (Core.Maybe Types.NextToken)
laartfrNextToken = Lens.field @"nextToken"
{-# DEPRECATED laartfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance
  Core.FromJSON
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  toJSON ListAssociatedApprovalRuleTemplatesForRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance
  Core.AWSRequest
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  type
    Rs ListAssociatedApprovalRuleTemplatesForRepository =
      ListAssociatedApprovalRuleTemplatesForRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
            Core.<$> (x Core..:? "approvalRuleTemplateNames")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { -- | The names of all approval rule templates associated with the repository.
    approvalRuleTemplateNames :: Core.Maybe [Types.ApprovalRuleTemplateName],
    -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedApprovalRuleTemplatesForRepositoryResponse' value with any optional fields omitted.
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAssociatedApprovalRuleTemplatesForRepositoryResponse
mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
  responseStatus =
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
      { approvalRuleTemplateNames =
          Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | The names of all approval rule templates associated with the repository.
--
-- /Note:/ Consider using 'approvalRuleTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsApprovalRuleTemplateNames :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe [Types.ApprovalRuleTemplateName])
laartfrrrsApprovalRuleTemplateNames = Lens.field @"approvalRuleTemplateNames"
{-# DEPRECATED laartfrrrsApprovalRuleTemplateNames "Use generic-lens or generic-optics with 'approvalRuleTemplateNames' instead." #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsNextToken :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Core.Maybe Types.NextToken)
laartfrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED laartfrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laartfrrrsResponseStatus :: Lens.Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Core.Int
laartfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laartfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
