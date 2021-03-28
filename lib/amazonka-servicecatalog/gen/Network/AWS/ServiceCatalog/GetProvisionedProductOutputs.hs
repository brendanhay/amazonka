{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API takes either a @ProvisonedProductId@ or a @ProvisionedProductName@ , along with a list of one or more output keys, and responds with the key/value pairs of those outputs.
module Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
    (
    -- * Creating a request
      GetProvisionedProductOutputs (..)
    , mkGetProvisionedProductOutputs
    -- ** Request lenses
    , gppoAcceptLanguage
    , gppoOutputKeys
    , gppoPageSize
    , gppoPageToken
    , gppoProvisionedProductId
    , gppoProvisionedProductName

    -- * Destructuring the response
    , GetProvisionedProductOutputsResponse (..)
    , mkGetProvisionedProductOutputsResponse
    -- ** Response lenses
    , gpporrsNextPageToken
    , gpporrsOutputs
    , gpporrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkGetProvisionedProductOutputs' smart constructor.
data GetProvisionedProductOutputs = GetProvisionedProductOutputs'
  { acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  , outputKeys :: Core.Maybe [Types.OutputKey]
    -- ^ The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , provisionedProductId :: Core.Maybe Types.ProvisionedProductId
    -- ^ The identifier of the provisioned product that you want the outputs from.
  , provisionedProductName :: Core.Maybe Types.ProvisionedProductName
    -- ^ The name of the provisioned product that you want the outputs from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedProductOutputs' value with any optional fields omitted.
mkGetProvisionedProductOutputs
    :: GetProvisionedProductOutputs
mkGetProvisionedProductOutputs
  = GetProvisionedProductOutputs'{acceptLanguage = Core.Nothing,
                                  outputKeys = Core.Nothing, pageSize = Core.Nothing,
                                  pageToken = Core.Nothing, provisionedProductId = Core.Nothing,
                                  provisionedProductName = Core.Nothing}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoAcceptLanguage :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.AcceptLanguage)
gppoAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE gppoAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
--
-- /Note:/ Consider using 'outputKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoOutputKeys :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe [Types.OutputKey])
gppoOutputKeys = Lens.field @"outputKeys"
{-# INLINEABLE gppoOutputKeys #-}
{-# DEPRECATED outputKeys "Use generic-lens or generic-optics with 'outputKeys' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageSize :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Core.Natural)
gppoPageSize = Lens.field @"pageSize"
{-# INLINEABLE gppoPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageToken :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.PageToken)
gppoPageToken = Lens.field @"pageToken"
{-# INLINEABLE gppoPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The identifier of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductId :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.ProvisionedProductId)
gppoProvisionedProductId = Lens.field @"provisionedProductId"
{-# INLINEABLE gppoProvisionedProductId #-}
{-# DEPRECATED provisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead"  #-}

-- | The name of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductName :: Lens.Lens' GetProvisionedProductOutputs (Core.Maybe Types.ProvisionedProductName)
gppoProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE gppoProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

instance Core.ToQuery GetProvisionedProductOutputs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetProvisionedProductOutputs where
        toHeaders GetProvisionedProductOutputs{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.GetProvisionedProductOutputs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetProvisionedProductOutputs where
        toJSON GetProvisionedProductOutputs{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("OutputKeys" Core..=) Core.<$> outputKeys,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("ProvisionedProductId" Core..=) Core.<$> provisionedProductId,
                  ("ProvisionedProductName" Core..=) Core.<$>
                    provisionedProductName])

instance Core.AWSRequest GetProvisionedProductOutputs where
        type Rs GetProvisionedProductOutputs =
             GetProvisionedProductOutputsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetProvisionedProductOutputsResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "Outputs" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetProvisionedProductOutputsResponse' smart constructor.
data GetProvisionedProductOutputsResponse = GetProvisionedProductOutputsResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , outputs :: Core.Maybe [Types.RecordOutput]
    -- ^ Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedProductOutputsResponse' value with any optional fields omitted.
mkGetProvisionedProductOutputsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetProvisionedProductOutputsResponse
mkGetProvisionedProductOutputsResponse responseStatus
  = GetProvisionedProductOutputsResponse'{nextPageToken =
                                            Core.Nothing,
                                          outputs = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsNextPageToken :: Lens.Lens' GetProvisionedProductOutputsResponse (Core.Maybe Types.NextPageToken)
gpporrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gpporrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL. 
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsOutputs :: Lens.Lens' GetProvisionedProductOutputsResponse (Core.Maybe [Types.RecordOutput])
gpporrsOutputs = Lens.field @"outputs"
{-# INLINEABLE gpporrsOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporrsResponseStatus :: Lens.Lens' GetProvisionedProductOutputsResponse Core.Int
gpporrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpporrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
