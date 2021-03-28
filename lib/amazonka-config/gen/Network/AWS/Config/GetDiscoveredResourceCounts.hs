{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetDiscoveredResourceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account. 
--
-- __Example__ 
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify that you want all resource types. 
--
--
--     * AWS Config returns the following:
--
--     * The resource types (EC2 instances, IAM users, and S3 buckets).
--
--
--     * The number of each resource type (25, 20, and 15).
--
--
--     * The total number of all resources (60).
--
--
--
--
-- The response is paginated. By default, AWS Config lists 100 'ResourceCount' objects on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
module Network.AWS.Config.GetDiscoveredResourceCounts
    (
    -- * Creating a request
      GetDiscoveredResourceCounts (..)
    , mkGetDiscoveredResourceCounts
    -- ** Request lenses
    , gdrcLimit
    , gdrcNextToken
    , gdrcResourceTypes

    -- * Destructuring the response
    , GetDiscoveredResourceCountsResponse (..)
    , mkGetDiscoveredResourceCountsResponse
    -- ** Response lenses
    , gdrcrrsNextToken
    , gdrcrrsResourceCounts
    , gdrcrrsTotalDiscoveredResources
    , gdrcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDiscoveredResourceCounts' smart constructor.
data GetDiscoveredResourceCounts = GetDiscoveredResourceCounts'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , resourceTypes :: Core.Maybe [Types.StringWithCharLimit256]
    -- ^ The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiscoveredResourceCounts' value with any optional fields omitted.
mkGetDiscoveredResourceCounts
    :: GetDiscoveredResourceCounts
mkGetDiscoveredResourceCounts
  = GetDiscoveredResourceCounts'{limit = Core.Nothing,
                                 nextToken = Core.Nothing, resourceTypes = Core.Nothing}

-- | The maximum number of 'ResourceCount' objects returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcLimit :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe Core.Natural)
gdrcLimit = Lens.field @"limit"
{-# INLINEABLE gdrcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcNextToken :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe Types.NextToken)
gdrcNextToken = Lens.field @"nextToken"
{-# INLINEABLE gdrcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The comma-separated list that specifies the resource types that you want AWS Config to return (for example, @"AWS::EC2::Instance"@ , @"AWS::IAM::User"@ ).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all resource types that AWS Config is recording in the region for your account.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcResourceTypes :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe [Types.StringWithCharLimit256])
gdrcResourceTypes = Lens.field @"resourceTypes"
{-# INLINEABLE gdrcResourceTypes #-}
{-# DEPRECATED resourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead"  #-}

instance Core.ToQuery GetDiscoveredResourceCounts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDiscoveredResourceCounts where
        toHeaders GetDiscoveredResourceCounts{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.GetDiscoveredResourceCounts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDiscoveredResourceCounts where
        toJSON GetDiscoveredResourceCounts{..}
          = Core.object
              (Core.catMaybes
                 [("limit" Core..=) Core.<$> limit,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("resourceTypes" Core..=) Core.<$> resourceTypes])

instance Core.AWSRequest GetDiscoveredResourceCounts where
        type Rs GetDiscoveredResourceCounts =
             GetDiscoveredResourceCountsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDiscoveredResourceCountsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "resourceCounts"
                     Core.<*> x Core..:? "totalDiscoveredResources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDiscoveredResourceCountsResponse' smart constructor.
data GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The string that you use in a subsequent request to get the next page of results in a paginated response.
  , resourceCounts :: Core.Maybe [Types.ResourceCount]
    -- ^ The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
  , totalDiscoveredResources :: Core.Maybe Core.Integer
    -- ^ The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types.
--
-- __Example__ 
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.
--
--
--     * AWS Config returns 25 for @totalDiscoveredResources@ .
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiscoveredResourceCountsResponse' value with any optional fields omitted.
mkGetDiscoveredResourceCountsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDiscoveredResourceCountsResponse
mkGetDiscoveredResourceCountsResponse responseStatus
  = GetDiscoveredResourceCountsResponse'{nextToken = Core.Nothing,
                                         resourceCounts = Core.Nothing,
                                         totalDiscoveredResources = Core.Nothing, responseStatus}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrrsNextToken :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe Types.NextToken)
gdrcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gdrcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of @ResourceCount@ objects. Each object is listed in descending order by the number of resources.
--
-- /Note:/ Consider using 'resourceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrrsResourceCounts :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe [Types.ResourceCount])
gdrcrrsResourceCounts = Lens.field @"resourceCounts"
{-# INLINEABLE gdrcrrsResourceCounts #-}
{-# DEPRECATED resourceCounts "Use generic-lens or generic-optics with 'resourceCounts' instead"  #-}

-- | The total number of resources that AWS Config is recording in the region for your account. If you specify resource types in the request, AWS Config returns only the total number of resources for those resource types.
--
-- __Example__ 
--
--     * AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets, for a total of 60 resources.
--
--
--     * You make a call to the @GetDiscoveredResourceCounts@ action and specify the resource type, @"AWS::EC2::Instances"@ , in the request.
--
--
--     * AWS Config returns 25 for @totalDiscoveredResources@ .
--
--
--
-- /Note:/ Consider using 'totalDiscoveredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrrsTotalDiscoveredResources :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe Core.Integer)
gdrcrrsTotalDiscoveredResources = Lens.field @"totalDiscoveredResources"
{-# INLINEABLE gdrcrrsTotalDiscoveredResources #-}
{-# DEPRECATED totalDiscoveredResources "Use generic-lens or generic-optics with 'totalDiscoveredResources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrcrrsResponseStatus :: Lens.Lens' GetDiscoveredResourceCountsResponse Core.Int
gdrcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
