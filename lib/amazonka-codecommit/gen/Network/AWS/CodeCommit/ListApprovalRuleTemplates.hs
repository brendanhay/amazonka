{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListApprovalRuleTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates in the specified AWS Region in your AWS account. If an AWS Region is not specified, the AWS Region where you are signed in is used.
module Network.AWS.CodeCommit.ListApprovalRuleTemplates
  ( -- * Creating a request
    ListApprovalRuleTemplates (..),
    mkListApprovalRuleTemplates,

    -- ** Request lenses
    lartMaxResults,
    lartNextToken,

    -- * Destructuring the response
    ListApprovalRuleTemplatesResponse (..),
    mkListApprovalRuleTemplatesResponse,

    -- ** Response lenses
    lartrrsApprovalRuleTemplateNames,
    lartrrsNextToken,
    lartrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListApprovalRuleTemplates' smart constructor.
data ListApprovalRuleTemplates = ListApprovalRuleTemplates'
  { -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApprovalRuleTemplates' value with any optional fields omitted.
mkListApprovalRuleTemplates ::
  ListApprovalRuleTemplates
mkListApprovalRuleTemplates =
  ListApprovalRuleTemplates'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartMaxResults :: Lens.Lens' ListApprovalRuleTemplates (Core.Maybe Core.Int)
lartMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lartMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartNextToken :: Lens.Lens' ListApprovalRuleTemplates (Core.Maybe Types.NextToken)
lartNextToken = Lens.field @"nextToken"
{-# DEPRECATED lartNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListApprovalRuleTemplates where
  toJSON ListApprovalRuleTemplates {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListApprovalRuleTemplates where
  type
    Rs ListApprovalRuleTemplates =
      ListApprovalRuleTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.ListApprovalRuleTemplates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApprovalRuleTemplatesResponse'
            Core.<$> (x Core..:? "approvalRuleTemplateNames")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListApprovalRuleTemplatesResponse' smart constructor.
data ListApprovalRuleTemplatesResponse = ListApprovalRuleTemplatesResponse'
  { -- | The names of all the approval rule templates found in the AWS Region for your AWS account.
    approvalRuleTemplateNames :: Core.Maybe [Types.ApprovalRuleTemplateName],
    -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApprovalRuleTemplatesResponse' value with any optional fields omitted.
mkListApprovalRuleTemplatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListApprovalRuleTemplatesResponse
mkListApprovalRuleTemplatesResponse responseStatus =
  ListApprovalRuleTemplatesResponse'
    { approvalRuleTemplateNames =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The names of all the approval rule templates found in the AWS Region for your AWS account.
--
-- /Note:/ Consider using 'approvalRuleTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrrsApprovalRuleTemplateNames :: Lens.Lens' ListApprovalRuleTemplatesResponse (Core.Maybe [Types.ApprovalRuleTemplateName])
lartrrsApprovalRuleTemplateNames = Lens.field @"approvalRuleTemplateNames"
{-# DEPRECATED lartrrsApprovalRuleTemplateNames "Use generic-lens or generic-optics with 'approvalRuleTemplateNames' instead." #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrrsNextToken :: Lens.Lens' ListApprovalRuleTemplatesResponse (Core.Maybe Types.NextToken)
lartrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lartrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lartrrsResponseStatus :: Lens.Lens' ListApprovalRuleTemplatesResponse Core.Int
lartrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lartrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
