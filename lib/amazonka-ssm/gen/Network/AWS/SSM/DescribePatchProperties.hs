{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of available patches organized by product, product family, classification, severity, and other properties of available patches. You can use the reported properties in the filters you specify in requests for actions such as 'CreatePatchBaseline' , 'UpdatePatchBaseline' , 'DescribeAvailablePatches' , and 'DescribePatchBaselines' .
--
-- The following section lists the properties that can be used in filters for each major operating system type:
--
--     * AMAZON_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * AMAZON_LINUX_2
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * CENTOS
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * DEBIAN
--
--     * Valid properties: PRODUCT, PRIORITY
--
--
--     * ORACLE_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * REDHAT_ENTERPRISE_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * SUSE
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * UBUNTU
--
--     * Valid properties: PRODUCT, PRIORITY
--
--
--     * WINDOWS
--
--     * Valid properties: PRODUCT, PRODUCT_FAMILY, CLASSIFICATION, MSRC_SEVERITY
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchProperties
    (
    -- * Creating a request
      DescribePatchProperties (..)
    , mkDescribePatchProperties
    -- ** Request lenses
    , dppOperatingSystem
    , dppProperty
    , dppMaxResults
    , dppNextToken
    , dppPatchSet

    -- * Destructuring the response
    , DescribePatchPropertiesResponse (..)
    , mkDescribePatchPropertiesResponse
    -- ** Response lenses
    , dpprrsNextToken
    , dpprrsProperties
    , dpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribePatchProperties' smart constructor.
data DescribePatchProperties = DescribePatchProperties'
  { operatingSystem :: Types.OperatingSystem
    -- ^ The operating system type for which to list patches.
  , property :: Types.PatchProperty
    -- ^ The patch property for which you want to view patch details. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , patchSet :: Core.Maybe Types.PatchSet
    -- ^ Indicates whether to list patches for the Windows operating system or for Microsoft applications. Not applicable for Linux operating systems.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchProperties' value with any optional fields omitted.
mkDescribePatchProperties
    :: Types.OperatingSystem -- ^ 'operatingSystem'
    -> Types.PatchProperty -- ^ 'property'
    -> DescribePatchProperties
mkDescribePatchProperties operatingSystem property
  = DescribePatchProperties'{operatingSystem, property,
                             maxResults = Core.Nothing, nextToken = Core.Nothing,
                             patchSet = Core.Nothing}

-- | The operating system type for which to list patches.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppOperatingSystem :: Lens.Lens' DescribePatchProperties Types.OperatingSystem
dppOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE dppOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

-- | The patch property for which you want to view patch details. 
--
-- /Note:/ Consider using 'property' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProperty :: Lens.Lens' DescribePatchProperties Types.PatchProperty
dppProperty = Lens.field @"property"
{-# INLINEABLE dppProperty #-}
{-# DEPRECATED property "Use generic-lens or generic-optics with 'property' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppMaxResults :: Lens.Lens' DescribePatchProperties (Core.Maybe Core.Natural)
dppMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dppMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppNextToken :: Lens.Lens' DescribePatchProperties (Core.Maybe Types.NextToken)
dppNextToken = Lens.field @"nextToken"
{-# INLINEABLE dppNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Indicates whether to list patches for the Windows operating system or for Microsoft applications. Not applicable for Linux operating systems.
--
-- /Note:/ Consider using 'patchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppPatchSet :: Lens.Lens' DescribePatchProperties (Core.Maybe Types.PatchSet)
dppPatchSet = Lens.field @"patchSet"
{-# INLINEABLE dppPatchSet #-}
{-# DEPRECATED patchSet "Use generic-lens or generic-optics with 'patchSet' instead"  #-}

instance Core.ToQuery DescribePatchProperties where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePatchProperties where
        toHeaders DescribePatchProperties{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribePatchProperties")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePatchProperties where
        toJSON DescribePatchProperties{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OperatingSystem" Core..= operatingSystem),
                  Core.Just ("Property" Core..= property),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("PatchSet" Core..=) Core.<$> patchSet])

instance Core.AWSRequest DescribePatchProperties where
        type Rs DescribePatchProperties = DescribePatchPropertiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePatchPropertiesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Properties" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePatchProperties where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"properties" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePatchPropertiesResponse' smart constructor.
data DescribePatchPropertiesResponse = DescribePatchPropertiesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You use this token in the next call.)
  , properties :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue]
    -- ^ A list of the properties for patches matching the filter request parameters.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchPropertiesResponse' value with any optional fields omitted.
mkDescribePatchPropertiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePatchPropertiesResponse
mkDescribePatchPropertiesResponse responseStatus
  = DescribePatchPropertiesResponse'{nextToken = Core.Nothing,
                                     properties = Core.Nothing, responseStatus}

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsNextToken :: Lens.Lens' DescribePatchPropertiesResponse (Core.Maybe Types.NextToken)
dpprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of the properties for patches matching the filter request parameters.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsProperties :: Lens.Lens' DescribePatchPropertiesResponse (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
dpprrsProperties = Lens.field @"properties"
{-# INLINEABLE dpprrsProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsResponseStatus :: Lens.Lens' DescribePatchPropertiesResponse Core.Int
dpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
