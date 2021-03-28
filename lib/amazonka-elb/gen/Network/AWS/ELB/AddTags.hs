{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load balancer can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same key is already associated with the load balancer, @AddTags@ updates its value.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.AddTags
    (
    -- * Creating a request
      AddTags (..)
    , mkAddTags
    -- ** Request lenses
    , atLoadBalancerNames
    , atTags

    -- * Destructuring the response
    , AddTagsResponse (..)
    , mkAddTagsResponse
    -- ** Response lenses
    , atrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { loadBalancerNames :: [Types.AccessPointName]
    -- ^ The name of the load balancer. You can specify one load balancer only.
  , tags :: Core.NonEmpty Types.Tag
    -- ^ The tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags
    :: Core.NonEmpty Types.Tag -- ^ 'tags'
    -> AddTags
mkAddTags tags = AddTags'{loadBalancerNames = Core.mempty, tags}

-- | The name of the load balancer. You can specify one load balancer only.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atLoadBalancerNames :: Lens.Lens' AddTags [Types.AccessPointName]
atLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE atLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags (Core.NonEmpty Types.Tag)
atTags = Lens.field @"tags"
{-# INLINEABLE atTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTags where
        toQuery AddTags{..}
          = Core.toQueryPair "Action" ("AddTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.toQueryList "member" loadBalancerNames)
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "member" tags)

instance Core.ToHeaders AddTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
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
          = Response.receiveXMLWrapper "AddTagsResult"
              (\ s h x ->
                 AddTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of AddTags.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsResponse' value with any optional fields omitted.
mkAddTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsResponse
mkAddTagsResponse responseStatus = AddTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
