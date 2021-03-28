{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTopicRuleDestination (..)
    , mkGetTopicRuleDestination
    -- ** Request lenses
    , gtrdArn

    -- * Destructuring the response
    , GetTopicRuleDestinationResponse (..)
    , mkGetTopicRuleDestinationResponse
    -- ** Response lenses
    , gtrdrrsTopicRuleDestination
    , gtrdrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTopicRuleDestination' smart constructor.
newtype GetTopicRuleDestination = GetTopicRuleDestination'
  { arn :: Types.Arn
    -- ^ The ARN of the topic rule destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicRuleDestination' value with any optional fields omitted.
mkGetTopicRuleDestination
    :: Types.Arn -- ^ 'arn'
    -> GetTopicRuleDestination
mkGetTopicRuleDestination arn = GetTopicRuleDestination'{arn}

-- | The ARN of the topic rule destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdArn :: Lens.Lens' GetTopicRuleDestination Types.Arn
gtrdArn = Lens.field @"arn"
{-# INLINEABLE gtrdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetTopicRuleDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTopicRuleDestination where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTopicRuleDestination where
        type Rs GetTopicRuleDestination = GetTopicRuleDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/destinations/" Core.<> Core.toText arn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTopicRuleDestinationResponse' Core.<$>
                   (x Core..:? "topicRuleDestination") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { topicRuleDestination :: Core.Maybe Types.TopicRuleDestination
    -- ^ The topic rule destination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicRuleDestinationResponse' value with any optional fields omitted.
mkGetTopicRuleDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTopicRuleDestinationResponse
mkGetTopicRuleDestinationResponse responseStatus
  = GetTopicRuleDestinationResponse'{topicRuleDestination =
                                       Core.Nothing,
                                     responseStatus}

-- | The topic rule destination.
--
-- /Note:/ Consider using 'topicRuleDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrrsTopicRuleDestination :: Lens.Lens' GetTopicRuleDestinationResponse (Core.Maybe Types.TopicRuleDestination)
gtrdrrsTopicRuleDestination = Lens.field @"topicRuleDestination"
{-# INLINEABLE gtrdrrsTopicRuleDestination #-}
{-# DEPRECATED topicRuleDestination "Use generic-lens or generic-optics with 'topicRuleDestination' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrrsResponseStatus :: Lens.Lens' GetTopicRuleDestinationResponse Core.Int
gtrdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
