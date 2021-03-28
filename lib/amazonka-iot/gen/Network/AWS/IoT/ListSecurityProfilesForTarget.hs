{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListSecurityProfilesForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles attached to a target (thing group).
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfilesForTarget
    (
    -- * Creating a request
      ListSecurityProfilesForTarget (..)
    , mkListSecurityProfilesForTarget
    -- ** Request lenses
    , lspftSecurityProfileTargetArn
    , lspftMaxResults
    , lspftNextToken
    , lspftRecursive

    -- * Destructuring the response
    , ListSecurityProfilesForTargetResponse (..)
    , mkListSecurityProfilesForTargetResponse
    -- ** Response lenses
    , lspftrrsNextToken
    , lspftrrsSecurityProfileTargetMappings
    , lspftrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSecurityProfilesForTarget' smart constructor.
data ListSecurityProfilesForTarget = ListSecurityProfilesForTarget'
  { securityProfileTargetArn :: Types.SecurityProfileTargetArn
    -- ^ The ARN of the target (thing group) whose attached security profiles you want to get.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , recursive :: Core.Maybe Core.Bool
    -- ^ If true, return child groups too.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityProfilesForTarget' value with any optional fields omitted.
mkListSecurityProfilesForTarget
    :: Types.SecurityProfileTargetArn -- ^ 'securityProfileTargetArn'
    -> ListSecurityProfilesForTarget
mkListSecurityProfilesForTarget securityProfileTargetArn
  = ListSecurityProfilesForTarget'{securityProfileTargetArn,
                                   maxResults = Core.Nothing, nextToken = Core.Nothing,
                                   recursive = Core.Nothing}

-- | The ARN of the target (thing group) whose attached security profiles you want to get.
--
-- /Note:/ Consider using 'securityProfileTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftSecurityProfileTargetArn :: Lens.Lens' ListSecurityProfilesForTarget Types.SecurityProfileTargetArn
lspftSecurityProfileTargetArn = Lens.field @"securityProfileTargetArn"
{-# INLINEABLE lspftSecurityProfileTargetArn #-}
{-# DEPRECATED securityProfileTargetArn "Use generic-lens or generic-optics with 'securityProfileTargetArn' instead"  #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftMaxResults :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Core.Natural)
lspftMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lspftMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftNextToken :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Types.NextToken)
lspftNextToken = Lens.field @"nextToken"
{-# INLINEABLE lspftNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If true, return child groups too.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftRecursive :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Core.Bool)
lspftRecursive = Lens.field @"recursive"
{-# INLINEABLE lspftRecursive #-}
{-# DEPRECATED recursive "Use generic-lens or generic-optics with 'recursive' instead"  #-}

instance Core.ToQuery ListSecurityProfilesForTarget where
        toQuery ListSecurityProfilesForTarget{..}
          = Core.toQueryPair "securityProfileTargetArn"
              securityProfileTargetArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "recursive") recursive

instance Core.ToHeaders ListSecurityProfilesForTarget where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListSecurityProfilesForTarget where
        type Rs ListSecurityProfilesForTarget =
             ListSecurityProfilesForTargetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/security-profiles-for-target",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSecurityProfilesForTargetResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*>
                     x Core..:? "securityProfileTargetMappings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSecurityProfilesForTarget where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"securityProfileTargetMappings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSecurityProfilesForTargetResponse' smart constructor.
data ListSecurityProfilesForTargetResponse = ListSecurityProfilesForTargetResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
  , securityProfileTargetMappings :: Core.Maybe [Types.SecurityProfileTargetMapping]
    -- ^ A list of security profiles and their associated targets.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityProfilesForTargetResponse' value with any optional fields omitted.
mkListSecurityProfilesForTargetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSecurityProfilesForTargetResponse
mkListSecurityProfilesForTargetResponse responseStatus
  = ListSecurityProfilesForTargetResponse'{nextToken = Core.Nothing,
                                           securityProfileTargetMappings = Core.Nothing,
                                           responseStatus}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrrsNextToken :: Lens.Lens' ListSecurityProfilesForTargetResponse (Core.Maybe Types.NextToken)
lspftrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lspftrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of security profiles and their associated targets.
--
-- /Note:/ Consider using 'securityProfileTargetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrrsSecurityProfileTargetMappings :: Lens.Lens' ListSecurityProfilesForTargetResponse (Core.Maybe [Types.SecurityProfileTargetMapping])
lspftrrsSecurityProfileTargetMappings = Lens.field @"securityProfileTargetMappings"
{-# INLINEABLE lspftrrsSecurityProfileTargetMappings #-}
{-# DEPRECATED securityProfileTargetMappings "Use generic-lens or generic-optics with 'securityProfileTargetMappings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspftrrsResponseStatus :: Lens.Lens' ListSecurityProfilesForTargetResponse Core.Int
lspftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lspftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
