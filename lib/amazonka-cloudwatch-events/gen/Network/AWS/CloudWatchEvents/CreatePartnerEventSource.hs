{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CreatePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Called by an SaaS partner to create a partner event source. This operation is not used by AWS customers.
--
-- Each partner event source can be used by one AWS account to create a matching partner event bus in that AWS account. A SaaS partner must create one partner event source for each AWS account that wants to receive those event types.
-- A partner event source creates events based on resources within the SaaS partner's service or application.
-- An AWS account that creates a partner event bus that matches the partner event source can use that event bus to receive events from the partner, and then process them using AWS Events rules and targets.
-- Partner event source names follow this format:
-- @/partner_name/ //event_namespace/ //event_name/ @
-- /partner_name/ is determined during partner registration and identifies the partner to AWS customers. /event_namespace/ is determined by the partner and is a way for the partner to categorize their events. /event_name/ is determined by the partner, and should uniquely identify an event-generating resource within the partner system. The combination of /event_namespace/ and /event_name/ should help AWS customers decide whether to create an event bus to receive these events.
module Network.AWS.CloudWatchEvents.CreatePartnerEventSource
  ( -- * Creating a request
    CreatePartnerEventSource (..),
    mkCreatePartnerEventSource,

    -- ** Request lenses
    cpesName,
    cpesAccount,

    -- * Destructuring the response
    CreatePartnerEventSourceResponse (..),
    mkCreatePartnerEventSourceResponse,

    -- ** Response lenses
    cpesrrsEventSourceArn,
    cpesrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePartnerEventSource' smart constructor.
data CreatePartnerEventSource = CreatePartnerEventSource'
  { -- | The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
    name :: Types.Name,
    -- | The AWS account ID that is permitted to create a matching partner event bus for this partner event source.
    account :: Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartnerEventSource' value with any optional fields omitted.
mkCreatePartnerEventSource ::
  -- | 'name'
  Types.Name ->
  -- | 'account'
  Types.AccountId ->
  CreatePartnerEventSource
mkCreatePartnerEventSource name account =
  CreatePartnerEventSource' {name, account}

-- | The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesName :: Lens.Lens' CreatePartnerEventSource Types.Name
cpesName = Lens.field @"name"
{-# DEPRECATED cpesName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS account ID that is permitted to create a matching partner event bus for this partner event source.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesAccount :: Lens.Lens' CreatePartnerEventSource Types.AccountId
cpesAccount = Lens.field @"account"
{-# DEPRECATED cpesAccount "Use generic-lens or generic-optics with 'account' instead." #-}

instance Core.FromJSON CreatePartnerEventSource where
  toJSON CreatePartnerEventSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Account" Core..= account)
          ]
      )

instance Core.AWSRequest CreatePartnerEventSource where
  type Rs CreatePartnerEventSource = CreatePartnerEventSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.CreatePartnerEventSource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePartnerEventSourceResponse'
            Core.<$> (x Core..:? "EventSourceArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePartnerEventSourceResponse' smart constructor.
data CreatePartnerEventSourceResponse = CreatePartnerEventSourceResponse'
  { -- | The ARN of the partner event source.
    eventSourceArn :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartnerEventSourceResponse' value with any optional fields omitted.
mkCreatePartnerEventSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePartnerEventSourceResponse
mkCreatePartnerEventSourceResponse responseStatus =
  CreatePartnerEventSourceResponse'
    { eventSourceArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesrrsEventSourceArn :: Lens.Lens' CreatePartnerEventSourceResponse (Core.Maybe Types.String)
cpesrrsEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED cpesrrsEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesrrsResponseStatus :: Lens.Lens' CreatePartnerEventSourceResponse Core.Int
cpesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
