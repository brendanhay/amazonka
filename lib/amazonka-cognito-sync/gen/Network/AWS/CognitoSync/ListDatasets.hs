{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.ListDatasets
    (
    -- * Creating a request
      ListDatasets (..)
    , mkListDatasets
    -- ** Request lenses
    , ldIdentityId
    , ldIdentityPoolId
    , ldMaxResults
    , ldNextToken

    -- * Destructuring the response
    , ListDatasetsResponse (..)
    , mkListDatasetsResponse
    -- ** Response lenses
    , ldrrsCount
    , ldrrsDatasets
    , ldrrsNextToken
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'mkListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { identityId :: Types.IdentityId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to be returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A pagination token for obtaining the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDatasets' value with any optional fields omitted.
mkListDatasets
    :: Types.IdentityId -- ^ 'identityId'
    -> Types.IdentityPoolId -- ^ 'identityPoolId'
    -> ListDatasets
mkListDatasets identityId identityPoolId
  = ListDatasets'{identityId, identityPoolId,
                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityId :: Lens.Lens' ListDatasets Types.IdentityId
ldIdentityId = Lens.field @"identityId"
{-# INLINEABLE ldIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIdentityPoolId :: Lens.Lens' ListDatasets Types.IdentityPoolId
ldIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE ldIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDatasets (Core.Maybe Core.Int)
ldMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDatasets (Core.Maybe Core.Text)
ldNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDatasets where
        toQuery ListDatasets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListDatasets where
        toHeaders ListDatasets{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListDatasets where
        type Rs ListDatasets = ListDatasetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/identities/"
                             Core.<> Core.toText identityId
                             Core.<> "/datasets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' Core.<$>
                   (x Core..:? "Count") Core.<*> x Core..:? "Datasets" Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'mkListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { count :: Core.Maybe Core.Int
    -- ^ Number of datasets returned.
  , datasets :: Core.Maybe [Types.Dataset]
    -- ^ A set of datasets.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A pagination token for obtaining the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDatasetsResponse' value with any optional fields omitted.
mkListDatasetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDatasetsResponse
mkListDatasetsResponse responseStatus
  = ListDatasetsResponse'{count = Core.Nothing,
                          datasets = Core.Nothing, nextToken = Core.Nothing, responseStatus}

-- | Number of datasets returned.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsCount :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Int)
ldrrsCount = Lens.field @"count"
{-# INLINEABLE ldrrsCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | A set of datasets.
--
-- /Note:/ Consider using 'datasets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDatasets :: Lens.Lens' ListDatasetsResponse (Core.Maybe [Types.Dataset])
ldrrsDatasets = Lens.field @"datasets"
{-# INLINEABLE ldrrsDatasets #-}
{-# DEPRECATED datasets "Use generic-lens or generic-optics with 'datasets' instead"  #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Text)
ldrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDatasetsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
