{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListResourcesInProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resources that are included in the protection group. 
module Network.AWS.Shield.ListResourcesInProtectionGroup
    (
    -- * Creating a request
      ListResourcesInProtectionGroup (..)
    , mkListResourcesInProtectionGroup
    -- ** Request lenses
    , lripgProtectionGroupId
    , lripgMaxResults
    , lripgNextToken

    -- * Destructuring the response
    , ListResourcesInProtectionGroupResponse (..)
    , mkListResourcesInProtectionGroupResponse
    -- ** Response lenses
    , lripgrrsResourceArns
    , lripgrrsNextToken
    , lripgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkListResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { protectionGroupId :: Types.ProtectionGroupId
    -- ^ The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
  , nextToken :: Core.Maybe Types.Token
    -- ^ The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesInProtectionGroup' value with any optional fields omitted.
mkListResourcesInProtectionGroup
    :: Types.ProtectionGroupId -- ^ 'protectionGroupId'
    -> ListResourcesInProtectionGroup
mkListResourcesInProtectionGroup protectionGroupId
  = ListResourcesInProtectionGroup'{protectionGroupId,
                                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it. 
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgProtectionGroupId :: Lens.Lens' ListResourcesInProtectionGroup Types.ProtectionGroupId
lripgProtectionGroupId = Lens.field @"protectionGroupId"
{-# INLINEABLE lripgProtectionGroupId #-}
{-# DEPRECATED protectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead"  #-}

-- | The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgMaxResults :: Lens.Lens' ListResourcesInProtectionGroup (Core.Maybe Core.Natural)
lripgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lripgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgNextToken :: Lens.Lens' ListResourcesInProtectionGroup (Core.Maybe Types.Token)
lripgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lripgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResourcesInProtectionGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourcesInProtectionGroup where
        toHeaders ListResourcesInProtectionGroup{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShield_20160616.ListResourcesInProtectionGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourcesInProtectionGroup where
        toJSON ListResourcesInProtectionGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProtectionGroupId" Core..= protectionGroupId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListResourcesInProtectionGroup where
        type Rs ListResourcesInProtectionGroup =
             ListResourcesInProtectionGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourcesInProtectionGroupResponse' Core.<$>
                   (x Core..:? "ResourceArns" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { resourceArns :: [Types.ResourceArn]
    -- ^ The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesInProtectionGroupResponse' value with any optional fields omitted.
mkListResourcesInProtectionGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourcesInProtectionGroupResponse
mkListResourcesInProtectionGroupResponse responseStatus
  = ListResourcesInProtectionGroupResponse'{resourceArns =
                                              Core.mempty,
                                            nextToken = Core.Nothing, responseStatus}

-- | The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrrsResourceArns :: Lens.Lens' ListResourcesInProtectionGroupResponse [Types.ResourceArn]
lripgrrsResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE lripgrrsResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

-- | If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrrsNextToken :: Lens.Lens' ListResourcesInProtectionGroupResponse (Core.Maybe Types.Token)
lripgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lripgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrrsResponseStatus :: Lens.Lens' ListResourcesInProtectionGroupResponse Core.Int
lripgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lripgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
