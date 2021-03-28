{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstanceAssociationsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The status of the associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceAssociationsStatus
    (
    -- * Creating a request
      DescribeInstanceAssociationsStatus (..)
    , mkDescribeInstanceAssociationsStatus
    -- ** Request lenses
    , diasInstanceId
    , diasMaxResults
    , diasNextToken

    -- * Destructuring the response
    , DescribeInstanceAssociationsStatusResponse (..)
    , mkDescribeInstanceAssociationsStatusResponse
    -- ** Response lenses
    , diasrrsInstanceAssociationStatusInfos
    , diasrrsNextToken
    , diasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstanceAssociationsStatus' smart constructor.
data DescribeInstanceAssociationsStatus = DescribeInstanceAssociationsStatus'
  { instanceId :: Types.InstanceId
    -- ^ The instance IDs for which you want association status information.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceAssociationsStatus' value with any optional fields omitted.
mkDescribeInstanceAssociationsStatus
    :: Types.InstanceId -- ^ 'instanceId'
    -> DescribeInstanceAssociationsStatus
mkDescribeInstanceAssociationsStatus instanceId
  = DescribeInstanceAssociationsStatus'{instanceId,
                                        maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The instance IDs for which you want association status information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasInstanceId :: Lens.Lens' DescribeInstanceAssociationsStatus Types.InstanceId
diasInstanceId = Lens.field @"instanceId"
{-# INLINEABLE diasInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasMaxResults :: Lens.Lens' DescribeInstanceAssociationsStatus (Core.Maybe Core.Natural)
diasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasNextToken :: Lens.Lens' DescribeInstanceAssociationsStatus (Core.Maybe Types.NextToken)
diasNextToken = Lens.field @"nextToken"
{-# INLINEABLE diasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstanceAssociationsStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstanceAssociationsStatus where
        toHeaders DescribeInstanceAssociationsStatus{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DescribeInstanceAssociationsStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstanceAssociationsStatus where
        toJSON DescribeInstanceAssociationsStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeInstanceAssociationsStatus where
        type Rs DescribeInstanceAssociationsStatus =
             DescribeInstanceAssociationsStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstanceAssociationsStatusResponse' Core.<$>
                   (x Core..:? "InstanceAssociationStatusInfos") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstanceAssociationsStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"instanceAssociationStatusInfos" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeInstanceAssociationsStatusResponse' smart constructor.
data DescribeInstanceAssociationsStatusResponse = DescribeInstanceAssociationsStatusResponse'
  { instanceAssociationStatusInfos :: Core.Maybe [Types.InstanceAssociationStatusInfo]
    -- ^ Status information about the association.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstanceAssociationsStatusResponse' value with any optional fields omitted.
mkDescribeInstanceAssociationsStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstanceAssociationsStatusResponse
mkDescribeInstanceAssociationsStatusResponse responseStatus
  = DescribeInstanceAssociationsStatusResponse'{instanceAssociationStatusInfos
                                                  = Core.Nothing,
                                                nextToken = Core.Nothing, responseStatus}

-- | Status information about the association.
--
-- /Note:/ Consider using 'instanceAssociationStatusInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsInstanceAssociationStatusInfos :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Core.Maybe [Types.InstanceAssociationStatusInfo])
diasrrsInstanceAssociationStatusInfos = Lens.field @"instanceAssociationStatusInfos"
{-# INLINEABLE diasrrsInstanceAssociationStatusInfos #-}
{-# DEPRECATED instanceAssociationStatusInfos "Use generic-lens or generic-optics with 'instanceAssociationStatusInfos' instead"  #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsNextToken :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Core.Maybe Types.NextToken)
diasrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diasrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsResponseStatus :: Lens.Lens' DescribeInstanceAssociationsStatusResponse Core.Int
diasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
