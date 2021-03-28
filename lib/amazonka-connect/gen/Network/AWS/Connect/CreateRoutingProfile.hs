{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new routing profile.
module Network.AWS.Connect.CreateRoutingProfile
    (
    -- * Creating a request
      CreateRoutingProfile (..)
    , mkCreateRoutingProfile
    -- ** Request lenses
    , crpInstanceId
    , crpName
    , crpDescription
    , crpDefaultOutboundQueueId
    , crpMediaConcurrencies
    , crpQueueConfigs
    , crpTags

    -- * Destructuring the response
    , CreateRoutingProfileResponse (..)
    , mkCreateRoutingProfileResponse
    -- ** Response lenses
    , crprrsRoutingProfileArn
    , crprrsRoutingProfileId
    , crprrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , name :: Types.Name
    -- ^ The name of the routing profile. Must not be more than 127 characters.
  , description :: Types.Description
    -- ^ Description of the routing profile. Must not be more than 250 characters.
  , defaultOutboundQueueId :: Types.DefaultOutboundQueueId
    -- ^ The default outbound queue for the routing profile.
  , mediaConcurrencies :: [Types.MediaConcurrency]
    -- ^ The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
  , queueConfigs :: Core.Maybe (Core.NonEmpty Types.RoutingProfileQueueConfig)
    -- ^ The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoutingProfile' value with any optional fields omitted.
mkCreateRoutingProfile
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.Name -- ^ 'name'
    -> Types.Description -- ^ 'description'
    -> Types.DefaultOutboundQueueId -- ^ 'defaultOutboundQueueId'
    -> CreateRoutingProfile
mkCreateRoutingProfile instanceId name description
  defaultOutboundQueueId
  = CreateRoutingProfile'{instanceId, name, description,
                          defaultOutboundQueueId, mediaConcurrencies = Core.mempty,
                          queueConfigs = Core.Nothing, tags = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpInstanceId :: Lens.Lens' CreateRoutingProfile Types.InstanceId
crpInstanceId = Lens.field @"instanceId"
{-# INLINEABLE crpInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the routing profile. Must not be more than 127 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpName :: Lens.Lens' CreateRoutingProfile Types.Name
crpName = Lens.field @"name"
{-# INLINEABLE crpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Description of the routing profile. Must not be more than 250 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDescription :: Lens.Lens' CreateRoutingProfile Types.Description
crpDescription = Lens.field @"description"
{-# INLINEABLE crpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The default outbound queue for the routing profile.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDefaultOutboundQueueId :: Lens.Lens' CreateRoutingProfile Types.DefaultOutboundQueueId
crpDefaultOutboundQueueId = Lens.field @"defaultOutboundQueueId"
{-# INLINEABLE crpDefaultOutboundQueueId #-}
{-# DEPRECATED defaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead"  #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpMediaConcurrencies :: Lens.Lens' CreateRoutingProfile [Types.MediaConcurrency]
crpMediaConcurrencies = Lens.field @"mediaConcurrencies"
{-# INLINEABLE crpMediaConcurrencies #-}
{-# DEPRECATED mediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead"  #-}

-- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpQueueConfigs :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.NonEmpty Types.RoutingProfileQueueConfig))
crpQueueConfigs = Lens.field @"queueConfigs"
{-# INLINEABLE crpQueueConfigs #-}
{-# DEPRECATED queueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpTags :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crpTags = Lens.field @"tags"
{-# INLINEABLE crpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRoutingProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRoutingProfile where
        toHeaders CreateRoutingProfile{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRoutingProfile where
        toJSON CreateRoutingProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Description" Core..= description),
                  Core.Just
                    ("DefaultOutboundQueueId" Core..= defaultOutboundQueueId),
                  Core.Just ("MediaConcurrencies" Core..= mediaConcurrencies),
                  ("QueueConfigs" Core..=) Core.<$> queueConfigs,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRoutingProfile where
        type Rs CreateRoutingProfile = CreateRoutingProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/routing-profiles/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRoutingProfileResponse' Core.<$>
                   (x Core..:? "RoutingProfileArn") Core.<*>
                     x Core..:? "RoutingProfileId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { routingProfileArn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the routing profile.
  , routingProfileId :: Core.Maybe Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoutingProfileResponse' value with any optional fields omitted.
mkCreateRoutingProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRoutingProfileResponse
mkCreateRoutingProfileResponse responseStatus
  = CreateRoutingProfileResponse'{routingProfileArn = Core.Nothing,
                                  routingProfileId = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'routingProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsRoutingProfileArn :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Types.ARN)
crprrsRoutingProfileArn = Lens.field @"routingProfileArn"
{-# INLINEABLE crprrsRoutingProfileArn #-}
{-# DEPRECATED routingProfileArn "Use generic-lens or generic-optics with 'routingProfileArn' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsRoutingProfileId :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Types.RoutingProfileId)
crprrsRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE crprrsRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsResponseStatus :: Lens.Lens' CreateRoutingProfileResponse Core.Int
crprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
