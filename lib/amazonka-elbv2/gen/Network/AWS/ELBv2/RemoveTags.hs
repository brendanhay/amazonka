{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified Elastic Load Balancing resources. You can remove the tags for one or more Application Load Balancers, Network Load Balancers, Gateway Load Balancers, target groups, listeners, or rules.
module Network.AWS.ELBv2.RemoveTags
    (
    -- * Creating a request
      RemoveTags (..)
    , mkRemoveTags
    -- ** Request lenses
    , rtResourceArns
    , rtTagKeys

    -- * Destructuring the response
    , RemoveTagsResponse (..)
    , mkRemoveTagsResponse
    -- ** Response lenses
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { resourceArns :: [Types.ResourceArn]
    -- ^ The Amazon Resource Name (ARN) of the resource.
  , tagKeys :: [Types.TagKey]
    -- ^ The tag keys for the tags to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTags' value with any optional fields omitted.
mkRemoveTags
    :: RemoveTags
mkRemoveTags
  = RemoveTags'{resourceArns = Core.mempty, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceArns :: Lens.Lens' RemoveTags [Types.ResourceArn]
rtResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE rtResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

-- | The tag keys for the tags to remove.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Types.TagKey]
rtTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTags where
        toQuery RemoveTags{..}
          = Core.toQueryPair "Action" ("RemoveTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ResourceArns"
                (Core.toQueryList "member" resourceArns)
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

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

-- | /See:/ 'mkRemoveTagsResponse' smart constructor.
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
