{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListInstanceProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all the instance profiles in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListInstanceProfiles
    (
    -- * Creating a request
      ListInstanceProfiles (..)
    , mkListInstanceProfiles
    -- ** Request lenses
    , lipMaxResults
    , lipNextToken

    -- * Destructuring the response
    , ListInstanceProfilesResponse (..)
    , mkListInstanceProfilesResponse
    -- ** Response lenses
    , liprrsInstanceProfiles
    , liprrsNextToken
    , liprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { maxResults :: Core.Maybe Core.Int
    -- ^ An integer that specifies the maximum number of items you want to return in the API response.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceProfiles' value with any optional fields omitted.
mkListInstanceProfiles
    :: ListInstanceProfiles
mkListInstanceProfiles
  = ListInstanceProfiles'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListInstanceProfiles (Core.Maybe Core.Int)
lipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListInstanceProfiles (Core.Maybe Types.PaginationToken)
lipNextToken = Lens.field @"nextToken"
{-# INLINEABLE lipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInstanceProfiles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListInstanceProfiles where
        toHeaders ListInstanceProfiles{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListInstanceProfiles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListInstanceProfiles where
        toJSON ListInstanceProfiles{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListInstanceProfiles where
        type Rs ListInstanceProfiles = ListInstanceProfilesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInstanceProfilesResponse' Core.<$>
                   (x Core..:? "instanceProfiles") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstanceProfiles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"instanceProfiles" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { instanceProfiles :: Core.Maybe [Types.InstanceProfile]
    -- ^ An object that contains information about your instance profiles.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that can be used in the next call to this operation to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceProfilesResponse' value with any optional fields omitted.
mkListInstanceProfilesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstanceProfilesResponse
mkListInstanceProfilesResponse responseStatus
  = ListInstanceProfilesResponse'{instanceProfiles = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | An object that contains information about your instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsInstanceProfiles :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe [Types.InstanceProfile])
liprrsInstanceProfiles = Lens.field @"instanceProfiles"
{-# INLINEABLE liprrsInstanceProfiles #-}
{-# DEPRECATED instanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead"  #-}

-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsNextToken :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe Types.PaginationToken)
liprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE liprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsResponseStatus :: Lens.Lens' ListInstanceProfilesResponse Core.Int
liprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
