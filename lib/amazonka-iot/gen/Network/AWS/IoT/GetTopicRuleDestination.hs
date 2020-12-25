{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a topic rule destination.
module Network.AWS.IoT.GetTopicRuleDestination
  ( -- * Creating a request
    GetTopicRuleDestination (..),
    mkGetTopicRuleDestination,

    -- ** Request lenses
    gtrdArn,

    -- * Destructuring the response
    GetTopicRuleDestinationResponse (..),
    mkGetTopicRuleDestinationResponse,

    -- ** Response lenses
    gtrdrrsTopicRuleDestination,
    gtrdrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTopicRuleDestination' smart constructor.
newtype GetTopicRuleDestination = GetTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicRuleDestination' value with any optional fields omitted.
mkGetTopicRuleDestination ::
  -- | 'arn'
  Types.Arn ->
  GetTopicRuleDestination
mkGetTopicRuleDestination arn = GetTopicRuleDestination' {arn}

-- | The ARN of the topic rule destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdArn :: Lens.Lens' GetTopicRuleDestination Types.Arn
gtrdArn = Lens.field @"arn"
{-# DEPRECATED gtrdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.AWSRequest GetTopicRuleDestination where
  type Rs GetTopicRuleDestination = GetTopicRuleDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/destinations/" Core.<> (Core.toText arn)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            Core.<$> (x Core..:? "topicRuleDestination")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Core.Maybe Types.TopicRuleDestination,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicRuleDestinationResponse' value with any optional fields omitted.
mkGetTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTopicRuleDestinationResponse
mkGetTopicRuleDestinationResponse responseStatus =
  GetTopicRuleDestinationResponse'
    { topicRuleDestination =
        Core.Nothing,
      responseStatus
    }

-- | The topic rule destination.
--
-- /Note:/ Consider using 'topicRuleDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrrsTopicRuleDestination :: Lens.Lens' GetTopicRuleDestinationResponse (Core.Maybe Types.TopicRuleDestination)
gtrdrrsTopicRuleDestination = Lens.field @"topicRuleDestination"
{-# DEPRECATED gtrdrrsTopicRuleDestination "Use generic-lens or generic-optics with 'topicRuleDestination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrrsResponseStatus :: Lens.Lens' GetTopicRuleDestinationResponse Core.Int
gtrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
