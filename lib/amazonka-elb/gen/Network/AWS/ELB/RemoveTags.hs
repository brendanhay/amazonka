{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified load balancer.
module Network.AWS.ELB.RemoveTags
    (
    -- * Creating a request
      RemoveTags (..)
    , mkRemoveTags
    -- ** Request lenses
    , rtLoadBalancerNames
    , rtTags

    -- * Destructuring the response
    , RemoveTagsResponse (..)
    , mkRemoveTagsResponse
    -- ** Response lenses
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { loadBalancerNames :: [Types.AccessPointName]
    -- ^ The name of the load balancer. You can specify a maximum of one load balancer name.
  , tags :: Core.NonEmpty Types.TagKeyOnly
    -- ^ The list of tag keys to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTags' value with any optional fields omitted.
mkRemoveTags
    :: Core.NonEmpty Types.TagKeyOnly -- ^ 'tags'
    -> RemoveTags
mkRemoveTags tags
  = RemoveTags'{loadBalancerNames = Core.mempty, tags}

-- | The name of the load balancer. You can specify a maximum of one load balancer name.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLoadBalancerNames :: Lens.Lens' RemoveTags [Types.AccessPointName]
rtLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE rtLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

-- | The list of tag keys to remove.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' RemoveTags (Core.NonEmpty Types.TagKeyOnly)
rtTags = Lens.field @"tags"
{-# INLINEABLE rtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery RemoveTags where
        toQuery RemoveTags{..}
          = Core.toQueryPair "Action" ("RemoveTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.toQueryList "member" loadBalancerNames)
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "member" tags)

instance Core.ToHeaders RemoveTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "RemoveTagsResult"
              (\ s h x ->
                 RemoveTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of RemoveTags.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsResponse' value with any optional fields omitted.
mkRemoveTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveTagsResponse
mkRemoveTagsResponse responseStatus
  = RemoveTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RemoveTagsResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
