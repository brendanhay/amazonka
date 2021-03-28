{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetResourcePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configurations for the resource policies set on individual resources, and also the account-level policy.
--
-- This operation also returns the Data Catalog resource policy. However, if you enabled metadata encryption in Data Catalog settings, and you do not have permission on the AWS KMS key, the operation can't return the Data Catalog resource policy.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetResourcePolicies
    (
    -- * Creating a request
      GetResourcePolicies (..)
    , mkGetResourcePolicies
    -- ** Request lenses
    , grpMaxResults
    , grpNextToken

    -- * Destructuring the response
    , GetResourcePoliciesResponse (..)
    , mkGetResourcePoliciesResponse
    -- ** Response lenses
    , grprrsGetResourcePoliciesResponseList
    , grprrsNextToken
    , grprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if this is a continuation request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicies' value with any optional fields omitted.
mkGetResourcePolicies
    :: GetResourcePolicies
mkGetResourcePolicies
  = GetResourcePolicies'{maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpMaxResults :: Lens.Lens' GetResourcePolicies (Core.Maybe Core.Natural)
grpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE grpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpNextToken :: Lens.Lens' GetResourcePolicies (Core.Maybe Types.Token)
grpNextToken = Lens.field @"nextToken"
{-# INLINEABLE grpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetResourcePolicies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetResourcePolicies where
        toHeaders GetResourcePolicies{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetResourcePolicies") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetResourcePolicies where
        toJSON GetResourcePolicies{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetResourcePolicies where
        type Rs GetResourcePolicies = GetResourcePoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourcePoliciesResponse' Core.<$>
                   (x Core..:? "GetResourcePoliciesResponseList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetResourcePolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"getResourcePoliciesResponseList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { getResourcePoliciesResponseList :: Core.Maybe [Types.GluePolicy]
    -- ^ A list of the individual resource policies and the account-level resource policy.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if the returned list does not contain the last resource policy available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetResourcePoliciesResponse' value with any optional fields omitted.
mkGetResourcePoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourcePoliciesResponse
mkGetResourcePoliciesResponse responseStatus
  = GetResourcePoliciesResponse'{getResourcePoliciesResponseList =
                                   Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | A list of the individual resource policies and the account-level resource policy.
--
-- /Note:/ Consider using 'getResourcePoliciesResponseList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsGetResourcePoliciesResponseList :: Lens.Lens' GetResourcePoliciesResponse (Core.Maybe [Types.GluePolicy])
grprrsGetResourcePoliciesResponseList = Lens.field @"getResourcePoliciesResponseList"
{-# INLINEABLE grprrsGetResourcePoliciesResponseList #-}
{-# DEPRECATED getResourcePoliciesResponseList "Use generic-lens or generic-optics with 'getResourcePoliciesResponseList' instead"  #-}

-- | A continuation token, if the returned list does not contain the last resource policy available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsNextToken :: Lens.Lens' GetResourcePoliciesResponse (Core.Maybe Types.Token)
grprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE grprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetResourcePoliciesResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
