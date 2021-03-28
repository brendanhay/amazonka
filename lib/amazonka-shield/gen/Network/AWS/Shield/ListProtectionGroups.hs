{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListProtectionGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the 'ProtectionGroup' objects for the account.
module Network.AWS.Shield.ListProtectionGroups
    (
    -- * Creating a request
      ListProtectionGroups (..)
    , mkListProtectionGroups
    -- ** Request lenses
    , lpgMaxResults
    , lpgNextToken

    -- * Destructuring the response
    , ListProtectionGroupsResponse (..)
    , mkListProtectionGroupsResponse
    -- ** Response lenses
    , lpgrrsProtectionGroups
    , lpgrrsNextToken
    , lpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkListProtectionGroups' smart constructor.
data ListProtectionGroups = ListProtectionGroups'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtectionGroups' value with any optional fields omitted.
mkListProtectionGroups
    :: ListProtectionGroups
mkListProtectionGroups
  = ListProtectionGroups'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgMaxResults :: Lens.Lens' ListProtectionGroups (Core.Maybe Core.Natural)
lpgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgNextToken :: Lens.Lens' ListProtectionGroups (Core.Maybe Types.NextToken)
lpgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListProtectionGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListProtectionGroups where
        toHeaders ListProtectionGroups{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.ListProtectionGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListProtectionGroups where
        toJSON ListProtectionGroups{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListProtectionGroups where
        type Rs ListProtectionGroups = ListProtectionGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProtectionGroupsResponse' Core.<$>
                   (x Core..:? "ProtectionGroups" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListProtectionGroupsResponse' smart constructor.
data ListProtectionGroupsResponse = ListProtectionGroupsResponse'
  { protectionGroups :: [Types.ProtectionGroup]
    -- ^ 
  , nextToken :: Core.Maybe Types.Token
    -- ^ If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtectionGroupsResponse' value with any optional fields omitted.
mkListProtectionGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProtectionGroupsResponse
mkListProtectionGroupsResponse responseStatus
  = ListProtectionGroupsResponse'{protectionGroups = Core.mempty,
                                  nextToken = Core.Nothing, responseStatus}

-- | 
--
-- /Note:/ Consider using 'protectionGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrrsProtectionGroups :: Lens.Lens' ListProtectionGroupsResponse [Types.ProtectionGroup]
lpgrrsProtectionGroups = Lens.field @"protectionGroups"
{-# INLINEABLE lpgrrsProtectionGroups #-}
{-# DEPRECATED protectionGroups "Use generic-lens or generic-optics with 'protectionGroups' instead"  #-}

-- | If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrrsNextToken :: Lens.Lens' ListProtectionGroupsResponse (Core.Maybe Types.Token)
lpgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrrsResponseStatus :: Lens.Lens' ListProtectionGroupsResponse Core.Int
lpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
