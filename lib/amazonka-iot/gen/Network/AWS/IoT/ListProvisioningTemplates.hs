{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListProvisioningTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the fleet provisioning templates in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplates
  ( -- * Creating a request
    ListProvisioningTemplates (..),
    mkListProvisioningTemplates,

    -- ** Request lenses
    lMaxResults,
    lNextToken,

    -- * Destructuring the response
    ListProvisioningTemplatesResponse (..),
    mkListProvisioningTemplatesResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsTemplates,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProvisioningTemplates' smart constructor.
data ListProvisioningTemplates = ListProvisioningTemplates'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisioningTemplates' value with any optional fields omitted.
mkListProvisioningTemplates ::
  ListProvisioningTemplates
mkListProvisioningTemplates =
  ListProvisioningTemplates'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListProvisioningTemplates (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListProvisioningTemplates (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListProvisioningTemplates where
  type
    Rs ListProvisioningTemplates =
      ListProvisioningTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/provisioning-templates",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplatesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "templates")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProvisioningTemplates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"templates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProvisioningTemplatesResponse' smart constructor.
data ListProvisioningTemplatesResponse = ListProvisioningTemplatesResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of fleet provisioning templates
    templates :: Core.Maybe [Types.ProvisioningTemplateSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProvisioningTemplatesResponse' value with any optional fields omitted.
mkListProvisioningTemplatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisioningTemplatesResponse
mkListProvisioningTemplatesResponse responseStatus =
  ListProvisioningTemplatesResponse'
    { nextToken = Core.Nothing,
      templates = Core.Nothing,
      responseStatus
    }

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListProvisioningTemplatesResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of fleet provisioning templates
--
-- /Note:/ Consider using 'templates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsTemplates :: Lens.Lens' ListProvisioningTemplatesResponse (Core.Maybe [Types.ProvisioningTemplateSummary])
lrsTemplates = Lens.field @"templates"
{-# DEPRECATED lrsTemplates "Use generic-lens or generic-optics with 'templates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListProvisioningTemplatesResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
