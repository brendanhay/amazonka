{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters tag keys to prevent tags that have the specified tag keys from being included in scheduled event notifications for resources in the Region.
module Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
    (
    -- * Creating a request
      DeregisterInstanceEventNotificationAttributes (..)
    , mkDeregisterInstanceEventNotificationAttributes
    -- ** Request lenses
    , dienaDryRun
    , dienaInstanceTagAttribute

    -- * Destructuring the response
    , DeregisterInstanceEventNotificationAttributesResponse (..)
    , mkDeregisterInstanceEventNotificationAttributesResponse
    -- ** Response lenses
    , dienarrsInstanceTagAttribute
    , dienarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterInstanceEventNotificationAttributes' smart constructor.
data DeregisterInstanceEventNotificationAttributes = DeregisterInstanceEventNotificationAttributes'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , instanceTagAttribute :: Core.Maybe Types.DeregisterInstanceTagAttributeRequest
    -- ^ Information about the tag keys to deregister.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstanceEventNotificationAttributes' value with any optional fields omitted.
mkDeregisterInstanceEventNotificationAttributes
    :: DeregisterInstanceEventNotificationAttributes
mkDeregisterInstanceEventNotificationAttributes
  = DeregisterInstanceEventNotificationAttributes'{dryRun =
                                                     Core.Nothing,
                                                   instanceTagAttribute = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienaDryRun :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Core.Maybe Core.Bool)
dienaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dienaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Information about the tag keys to deregister.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienaInstanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Core.Maybe Types.DeregisterInstanceTagAttributeRequest)
dienaInstanceTagAttribute = Lens.field @"instanceTagAttribute"
{-# INLINEABLE dienaInstanceTagAttribute #-}
{-# DEPRECATED instanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead"  #-}

instance Core.ToQuery DeregisterInstanceEventNotificationAttributes
         where
        toQuery DeregisterInstanceEventNotificationAttributes{..}
          = Core.toQueryPair "Action"
              ("DeregisterInstanceEventNotificationAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceTagAttribute")
                instanceTagAttribute

instance Core.ToHeaders
           DeregisterInstanceEventNotificationAttributes
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DeregisterInstanceEventNotificationAttributes
         where
        type Rs DeregisterInstanceEventNotificationAttributes =
             DeregisterInstanceEventNotificationAttributesResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DeregisterInstanceEventNotificationAttributesResponse' Core.<$>
                   (x Core..@? "instanceTagAttribute") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterInstanceEventNotificationAttributesResponse' smart constructor.
data DeregisterInstanceEventNotificationAttributesResponse = DeregisterInstanceEventNotificationAttributesResponse'
  { instanceTagAttribute :: Core.Maybe Types.InstanceTagNotificationAttribute
    -- ^ The resulting set of tag keys.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstanceEventNotificationAttributesResponse' value with any optional fields omitted.
mkDeregisterInstanceEventNotificationAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterInstanceEventNotificationAttributesResponse
mkDeregisterInstanceEventNotificationAttributesResponse
  responseStatus
  = DeregisterInstanceEventNotificationAttributesResponse'{instanceTagAttribute
                                                             = Core.Nothing,
                                                           responseStatus}

-- | The resulting set of tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarrsInstanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse (Core.Maybe Types.InstanceTagNotificationAttribute)
dienarrsInstanceTagAttribute = Lens.field @"instanceTagAttribute"
{-# INLINEABLE dienarrsInstanceTagAttribute #-}
{-# DEPRECATED instanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarrsResponseStatus :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse Core.Int
dienarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dienarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
