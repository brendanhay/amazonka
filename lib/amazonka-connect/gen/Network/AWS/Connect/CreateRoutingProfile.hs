{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateRoutingProfile (..),
    mkCreateRoutingProfile,

    -- ** Request lenses
    crpInstanceId,
    crpName,
    crpDescription,
    crpDefaultOutboundQueueId,
    crpMediaConcurrencies,
    crpQueueConfigs,
    crpTags,

    -- * Destructuring the response
    CreateRoutingProfileResponse (..),
    mkCreateRoutingProfileResponse,

    -- ** Response lenses
    crprrsRoutingProfileArn,
    crprrsRoutingProfileId,
    crprrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The name of the routing profile. Must not be more than 127 characters.
    name :: Types.Name,
    -- | Description of the routing profile. Must not be more than 250 characters.
    description :: Types.Description,
    -- | The default outbound queue for the routing profile.
    defaultOutboundQueueId :: Types.DefaultOutboundQueueId,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
    mediaConcurrencies :: [Types.MediaConcurrency],
    -- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
    queueConfigs :: Core.Maybe (Core.NonEmpty Types.RoutingProfileQueueConfig),
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoutingProfile' value with any optional fields omitted.
mkCreateRoutingProfile ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'name'
  Types.Name ->
  -- | 'description'
  Types.Description ->
  -- | 'defaultOutboundQueueId'
  Types.DefaultOutboundQueueId ->
  CreateRoutingProfile
mkCreateRoutingProfile
  instanceId
  name
  description
  defaultOutboundQueueId =
    CreateRoutingProfile'
      { instanceId,
        name,
        description,
        defaultOutboundQueueId,
        mediaConcurrencies = Core.mempty,
        queueConfigs = Core.Nothing,
        tags = Core.Nothing
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpInstanceId :: Lens.Lens' CreateRoutingProfile Types.InstanceId
crpInstanceId = Lens.field @"instanceId"
{-# DEPRECATED crpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the routing profile. Must not be more than 127 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpName :: Lens.Lens' CreateRoutingProfile Types.Name
crpName = Lens.field @"name"
{-# DEPRECATED crpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Description of the routing profile. Must not be more than 250 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDescription :: Lens.Lens' CreateRoutingProfile Types.Description
crpDescription = Lens.field @"description"
{-# DEPRECATED crpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The default outbound queue for the routing profile.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpDefaultOutboundQueueId :: Lens.Lens' CreateRoutingProfile Types.DefaultOutboundQueueId
crpDefaultOutboundQueueId = Lens.field @"defaultOutboundQueueId"
{-# DEPRECATED crpDefaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead." #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpMediaConcurrencies :: Lens.Lens' CreateRoutingProfile [Types.MediaConcurrency]
crpMediaConcurrencies = Lens.field @"mediaConcurrencies"
{-# DEPRECATED crpMediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead." #-}

-- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpQueueConfigs :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.NonEmpty Types.RoutingProfileQueueConfig))
crpQueueConfigs = Lens.field @"queueConfigs"
{-# DEPRECATED crpQueueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpTags :: Lens.Lens' CreateRoutingProfile (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crpTags = Lens.field @"tags"
{-# DEPRECATED crpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRoutingProfile where
  toJSON CreateRoutingProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Description" Core..= description),
            Core.Just
              ("DefaultOutboundQueueId" Core..= defaultOutboundQueueId),
            Core.Just ("MediaConcurrencies" Core..= mediaConcurrencies),
            ("QueueConfigs" Core..=) Core.<$> queueConfigs,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRoutingProfile where
  type Rs CreateRoutingProfile = CreateRoutingProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/routing-profiles/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoutingProfileResponse'
            Core.<$> (x Core..:? "RoutingProfileArn")
            Core.<*> (x Core..:? "RoutingProfileId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Core.Maybe Types.ARN,
    -- | The identifier of the routing profile.
    routingProfileId :: Core.Maybe Types.RoutingProfileId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoutingProfileResponse' value with any optional fields omitted.
mkCreateRoutingProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRoutingProfileResponse
mkCreateRoutingProfileResponse responseStatus =
  CreateRoutingProfileResponse'
    { routingProfileArn = Core.Nothing,
      routingProfileId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'routingProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsRoutingProfileArn :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Types.ARN)
crprrsRoutingProfileArn = Lens.field @"routingProfileArn"
{-# DEPRECATED crprrsRoutingProfileArn "Use generic-lens or generic-optics with 'routingProfileArn' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsRoutingProfileId :: Lens.Lens' CreateRoutingProfileResponse (Core.Maybe Types.RoutingProfileId)
crprrsRoutingProfileId = Lens.field @"routingProfileId"
{-# DEPRECATED crprrsRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crprrsResponseStatus :: Lens.Lens' CreateRoutingProfileResponse Core.Int
crprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
