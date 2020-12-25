{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an event bus in your account. This can include the external AWS accounts that are permitted to write events to your default event bus, and the associated policy. For custom event buses and partner event buses, it displays the name, ARN, policy, state, and creation time.
--
-- To enable your account to receive events from other accounts on its default event bus, use 'PutPermission' .
-- For more information about partner event buses, see 'CreateEventBus' .
module Network.AWS.CloudWatchEvents.DescribeEventBus
  ( -- * Creating a request
    DescribeEventBus (..),
    mkDescribeEventBus,

    -- ** Request lenses
    debName,

    -- * Destructuring the response
    DescribeEventBusResponse (..),
    mkDescribeEventBusResponse,

    -- ** Response lenses
    debrrsArn,
    debrrsName,
    debrrsPolicy,
    debrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventBus' smart constructor.
newtype DescribeEventBus = DescribeEventBus'
  { -- | The name or ARN of the event bus to show details for. If you omit this, the default event bus is displayed.
    name :: Core.Maybe Types.EventBusNameOrArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventBus' value with any optional fields omitted.
mkDescribeEventBus ::
  DescribeEventBus
mkDescribeEventBus = DescribeEventBus' {name = Core.Nothing}

-- | The name or ARN of the event bus to show details for. If you omit this, the default event bus is displayed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debName :: Lens.Lens' DescribeEventBus (Core.Maybe Types.EventBusNameOrArn)
debName = Lens.field @"name"
{-# DEPRECATED debName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeEventBus where
  toJSON DescribeEventBus {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest DescribeEventBus where
  type Rs DescribeEventBus = DescribeEventBusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribeEventBus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventBusResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEventBusResponse' smart constructor.
data DescribeEventBusResponse = DescribeEventBusResponse'
  { -- | The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
    arn :: Core.Maybe Types.String,
    -- | The name of the event bus. Currently, this is always @default@ .
    name :: Core.Maybe Types.String,
    -- | The policy that enables the external account to send events to your account.
    policy :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventBusResponse' value with any optional fields omitted.
mkDescribeEventBusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventBusResponse
mkDescribeEventBusResponse responseStatus =
  DescribeEventBusResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      policy = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrrsArn :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Types.String)
debrrsArn = Lens.field @"arn"
{-# DEPRECATED debrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the event bus. Currently, this is always @default@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrrsName :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Types.String)
debrrsName = Lens.field @"name"
{-# DEPRECATED debrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The policy that enables the external account to send events to your account.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrrsPolicy :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Types.String)
debrrsPolicy = Lens.field @"policy"
{-# DEPRECATED debrrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrrsResponseStatus :: Lens.Lens' DescribeEventBusResponse Core.Int
debrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED debrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
