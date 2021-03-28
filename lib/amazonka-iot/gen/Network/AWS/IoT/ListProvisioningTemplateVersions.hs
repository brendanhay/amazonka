{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListProvisioningTemplateVersions (..)
    , mkListProvisioningTemplateVersions
    -- ** Request lenses
    , lptvTemplateName
    , lptvMaxResults
    , lptvNextToken

    -- * Destructuring the response
    , ListProvisioningTemplateVersionsResponse (..)
    , mkListProvisioningTemplateVersionsResponse
    -- ** Response lenses
    , lptvrrsNextToken
    , lptvrrsVersions
    , lptvrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { templateName :: Types.TemplateName
    -- ^ The name of the fleet provisioning template.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisioningTemplateVersions' value with any optional fields omitted.
mkListProvisioningTemplateVersions
    :: Types.TemplateName -- ^ 'templateName'
    -> ListProvisioningTemplateVersions
mkListProvisioningTemplateVersions templateName
  = ListProvisioningTemplateVersions'{templateName,
                                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvTemplateName :: Lens.Lens' ListProvisioningTemplateVersions Types.TemplateName
lptvTemplateName = Lens.field @"templateName"
{-# INLINEABLE lptvTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvMaxResults :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Core.Natural)
lptvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lptvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvNextToken :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Types.NextToken)
lptvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lptvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListProvisioningTemplateVersions where
        toQuery ListProvisioningTemplateVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListProvisioningTemplateVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListProvisioningTemplateVersions where
        type Rs ListProvisioningTemplateVersions =
             ListProvisioningTemplateVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProvisioningTemplateVersionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListProvisioningTemplateVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to retrieve the next set of results.
  , versions :: Core.Maybe [Types.ProvisioningTemplateVersionSummary]
    -- ^ The list of fleet provisioning template versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListProvisioningTemplateVersionsResponse' value with any optional fields omitted.
mkListProvisioningTemplateVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProvisioningTemplateVersionsResponse
mkListProvisioningTemplateVersionsResponse responseStatus
  = ListProvisioningTemplateVersionsResponse'{nextToken =
                                                Core.Nothing,
                                              versions = Core.Nothing, responseStatus}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsNextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe Types.NextToken)
lptvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lptvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of fleet provisioning template versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsVersions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe [Types.ProvisioningTemplateVersionSummary])
lptvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lptvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrrsResponseStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Core.Int
lptvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lptvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
