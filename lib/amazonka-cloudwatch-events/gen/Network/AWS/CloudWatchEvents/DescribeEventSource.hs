{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared with your account.
module Network.AWS.CloudWatchEvents.DescribeEventSource
  ( -- * Creating a request
    DescribeEventSource (..),
    mkDescribeEventSource,

    -- ** Request lenses
    desName,

    -- * Destructuring the response
    DescribeEventSourceResponse (..),
    mkDescribeEventSourceResponse,

    -- ** Response lenses
    desrrsArn,
    desrrsCreatedBy,
    desrrsCreationTime,
    desrrsExpirationTime,
    desrrsName,
    desrrsState,
    desrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventSource' smart constructor.
newtype DescribeEventSource = DescribeEventSource'
  { -- | The name of the partner event source to display the details of.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSource' value with any optional fields omitted.
mkDescribeEventSource ::
  -- | 'name'
  Types.Name ->
  DescribeEventSource
mkDescribeEventSource name = DescribeEventSource' {name}

-- | The name of the partner event source to display the details of.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desName :: Lens.Lens' DescribeEventSource Types.Name
desName = Lens.field @"name"
{-# DEPRECATED desName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeEventSource where
  toJSON DescribeEventSource {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeEventSource where
  type Rs DescribeEventSource = DescribeEventSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribeEventSource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventSourceResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ExpirationTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { -- | The ARN of the partner event source.
    arn :: Core.Maybe Types.String,
    -- | The name of the SaaS partner that created the event source.
    createdBy :: Core.Maybe Types.String,
    -- | The date and time that the event source was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the event source will expire if you do not create a matching event bus.
    expirationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the partner event source.
    name :: Core.Maybe Types.String,
    -- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
    state :: Core.Maybe Types.EventSourceState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEventSourceResponse' value with any optional fields omitted.
mkDescribeEventSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventSourceResponse
mkDescribeEventSourceResponse responseStatus =
  DescribeEventSourceResponse'
    { arn = Core.Nothing,
      createdBy = Core.Nothing,
      creationTime = Core.Nothing,
      expirationTime = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsArn :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Types.String)
desrrsArn = Lens.field @"arn"
{-# DEPRECATED desrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the SaaS partner that created the event source.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsCreatedBy :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Types.String)
desrrsCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED desrrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The date and time that the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsCreationTime :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.NominalDiffTime)
desrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED desrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the event source will expire if you do not create a matching event bus.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsExpirationTime :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.NominalDiffTime)
desrrsExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED desrrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsName :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Types.String)
desrrsName = Lens.field @"name"
{-# DEPRECATED desrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsState :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Types.EventSourceState)
desrrsState = Lens.field @"state"
{-# DEPRECATED desrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DescribeEventSourceResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
