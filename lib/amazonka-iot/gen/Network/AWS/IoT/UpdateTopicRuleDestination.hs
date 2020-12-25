{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a topic rule destination. You use this to change the status, endpoint URL, or confirmation URL of the destination.
module Network.AWS.IoT.UpdateTopicRuleDestination
  ( -- * Creating a request
    UpdateTopicRuleDestination (..),
    mkUpdateTopicRuleDestination,

    -- ** Request lenses
    utrdArn,
    utrdStatus,

    -- * Destructuring the response
    UpdateTopicRuleDestinationResponse (..),
    mkUpdateTopicRuleDestinationResponse,

    -- ** Response lenses
    utrdrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTopicRuleDestination' smart constructor.
data UpdateTopicRuleDestination = UpdateTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Types.Arn,
    -- | The status of the topic rule destination. Valid values are:
    --
    --
    --     * IN_PROGRESS
    --
    --     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    --
    --
    --     * ENABLED
    --
    --     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * DISABLED
    --
    --     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * ERROR
    --
    --     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    status :: Types.TopicRuleDestinationStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTopicRuleDestination' value with any optional fields omitted.
mkUpdateTopicRuleDestination ::
  -- | 'arn'
  Types.Arn ->
  -- | 'status'
  Types.TopicRuleDestinationStatus ->
  UpdateTopicRuleDestination
mkUpdateTopicRuleDestination arn status =
  UpdateTopicRuleDestination' {arn, status}

-- | The ARN of the topic rule destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdArn :: Lens.Lens' UpdateTopicRuleDestination Types.Arn
utrdArn = Lens.field @"arn"
{-# DEPRECATED utrdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The status of the topic rule destination. Valid values are:
--
--
--     * IN_PROGRESS
--
--     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--     * ENABLED
--
--     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * DISABLED
--
--     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * ERROR
--
--     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdStatus :: Lens.Lens' UpdateTopicRuleDestination Types.TopicRuleDestinationStatus
utrdStatus = Lens.field @"status"
{-# DEPRECATED utrdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON UpdateTopicRuleDestination where
  toJSON UpdateTopicRuleDestination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            Core.Just ("status" Core..= status)
          ]
      )

instance Core.AWSRequest UpdateTopicRuleDestination where
  type
    Rs UpdateTopicRuleDestination =
      UpdateTopicRuleDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath = Core.rawPath "/destinations",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTopicRuleDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTopicRuleDestinationResponse' smart constructor.
newtype UpdateTopicRuleDestinationResponse = UpdateTopicRuleDestinationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTopicRuleDestinationResponse' value with any optional fields omitted.
mkUpdateTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTopicRuleDestinationResponse
mkUpdateTopicRuleDestinationResponse responseStatus =
  UpdateTopicRuleDestinationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdrrsResponseStatus :: Lens.Lens' UpdateTopicRuleDestinationResponse Core.Int
utrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
