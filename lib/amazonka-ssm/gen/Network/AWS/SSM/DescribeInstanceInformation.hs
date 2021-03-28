{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstanceInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your instances, including information about the operating system platform, the version of SSM Agent installed on the instance, instance status, and so on.
--
-- If you specify one or more instance IDs, it returns information for those instances. If you do not specify instance IDs, it returns information for all your instances. If you specify an instance ID that is not valid or an instance that you do not own, you receive an error.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceInformation
    (
    -- * Creating a request
      DescribeInstanceInformation (..)
    , mkDescribeInstanceInformation
    -- ** Request lenses
    , diiFilters
    , diiInstanceInformationFilterList
    , diiMaxResults
    , diiNextToken

    -- * Destructuring the response
    , DescribeInstanceInformationResponse (..)
    , mkDescribeInstanceInformationResponse
    -- ** Response lenses
    , diirrsInstanceInformationList
    , diirrsNextToken
    , diirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstanceInformation' smart constructor.
data DescribeInstanceInformation = DescribeInstanceInformation'
  { filters :: Core.Maybe [Types.InstanceInformationStringFilter]
    -- ^ One or more filters. Use a filter to return a more specific list of instances. You can filter based on tags applied to EC2 instances. Use this @Filters@ data type instead of @InstanceInformationFilterList@ , which is deprecated.
  , instanceInformationFilterList :: Core.Maybe [Types.InstanceInformationFilter]
    -- ^ This is a legacy method. We recommend that you don't use this method. Instead, use the @Filters@ data type. @Filters@ enables you to return instance information by filtering based on tags applied to managed instances.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceInformation' value with any optional fields omitted.
mkDescribeInstanceInformation
    :: DescribeInstanceInformation
mkDescribeInstanceInformation
  = DescribeInstanceInformation'{filters = Core.Nothing,
                                 instanceInformationFilterList = Core.Nothing,
                                 maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | One or more filters. Use a filter to return a more specific list of instances. You can filter based on tags applied to EC2 instances. Use this @Filters@ data type instead of @InstanceInformationFilterList@ , which is deprecated.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiFilters :: Lens.Lens' DescribeInstanceInformation (Core.Maybe [Types.InstanceInformationStringFilter])
diiFilters = Lens.field @"filters"
{-# INLINEABLE diiFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | This is a legacy method. We recommend that you don't use this method. Instead, use the @Filters@ data type. @Filters@ enables you to return instance information by filtering based on tags applied to managed instances.
--
-- /Note:/ Consider using 'instanceInformationFilterList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiInstanceInformationFilterList :: Lens.Lens' DescribeInstanceInformation (Core.Maybe [Types.InstanceInformationFilter])
diiInstanceInformationFilterList = Lens.field @"instanceInformationFilterList"
{-# INLINEABLE diiInstanceInformationFilterList #-}
{-# DEPRECATED instanceInformationFilterList "Use generic-lens or generic-optics with 'instanceInformationFilterList' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiMaxResults :: Lens.Lens' DescribeInstanceInformation (Core.Maybe Core.Natural)
diiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiNextToken :: Lens.Lens' DescribeInstanceInformation (Core.Maybe Types.NextToken)
diiNextToken = Lens.field @"nextToken"
{-# INLINEABLE diiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstanceInformation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstanceInformation where
        toHeaders DescribeInstanceInformation{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DescribeInstanceInformation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstanceInformation where
        toJSON DescribeInstanceInformation{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("InstanceInformationFilterList" Core..=) Core.<$>
                    instanceInformationFilterList,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeInstanceInformation where
        type Rs DescribeInstanceInformation =
             DescribeInstanceInformationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstanceInformationResponse' Core.<$>
                   (x Core..:? "InstanceInformationList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstanceInformation where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"instanceInformationList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeInstanceInformationResponse' smart constructor.
data DescribeInstanceInformationResponse = DescribeInstanceInformationResponse'
  { instanceInformationList :: Core.Maybe [Types.InstanceInformation]
    -- ^ The instance information list.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstanceInformationResponse' value with any optional fields omitted.
mkDescribeInstanceInformationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstanceInformationResponse
mkDescribeInstanceInformationResponse responseStatus
  = DescribeInstanceInformationResponse'{instanceInformationList =
                                           Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | The instance information list.
--
-- /Note:/ Consider using 'instanceInformationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirrsInstanceInformationList :: Lens.Lens' DescribeInstanceInformationResponse (Core.Maybe [Types.InstanceInformation])
diirrsInstanceInformationList = Lens.field @"instanceInformationList"
{-# INLINEABLE diirrsInstanceInformationList #-}
{-# DEPRECATED instanceInformationList "Use generic-lens or generic-optics with 'instanceInformationList' instead"  #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirrsNextToken :: Lens.Lens' DescribeInstanceInformationResponse (Core.Maybe Types.NextToken)
diirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirrsResponseStatus :: Lens.Lens' DescribeInstanceInformationResponse Core.Int
diirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
