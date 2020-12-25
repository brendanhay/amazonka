{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListProvisioningTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of fleet provisioning template versions.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplateVersions
  ( -- * Creating a request
    ListProvisioningTemplateVersions (..),
    mkListProvisioningTemplateVersions,

    -- ** Request lenses
    lptvTemplateName,
    lptvMaxResults,
    lptvNextToken,

    -- * Destructuring the response
    ListProvisioningTemplateVersionsResponse (..),
    mkListProvisioningTemplateVersionsResponse,

    -- ** Response lenses
    lptvrrsNextToken,
    lptvrrsVersions,
    lptvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { -- | The name of the fleet provisioning template.
    templateName :: Types.TemplateName,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisioningTemplateVersions' value with any optional fields omitted.
mkListProvisioningTemplateVersions ::
  -- | 'templateName'
  Types.TemplateName ->
  ListProvisioningTemplateVersions
mkListProvisioningTemplateVersions templateName =
  ListProvisioningTemplateVersions'
    { templateName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvTemplateName :: Lens.Lens' ListProvisioningTemplateVersions Types.TemplateName
lptvTemplateName = Lens.field @"templateName"
{-# DEPRECATED lptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvMaxResults :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Core.Natural)
lptvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lptvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvNextToken :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Types.NextToken)
lptvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lptvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListProvisioningTemplateVersions where
  type
    Rs ListProvisioningTemplateVersions =
      ListProvisioningTemplateVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/provisioning-templates/" Core.<> (Core.toText templateName)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplateVersionsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProvisioningTemplateVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of fleet provisioning template versions.
    versions :: Core.Maybe [Types.ProvisioningTemplateVersionSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProvisioningTemplateVersionsResponse' value with any optional fields omitted.
mkListProvisioningTemplateVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisioningTemplateVersionsResponse
mkListProvisioningTemplateVersionsResponse responseStatus =
  ListProvisioningTemplateVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsNextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe Types.NextToken)
lptvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lptvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of fleet provisioning template versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsVersions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe [Types.ProvisioningTemplateVersionSummary])
lptvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lptvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsResponseStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Core.Int
lptvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lptvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
