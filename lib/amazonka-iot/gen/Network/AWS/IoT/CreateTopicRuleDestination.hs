{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic rule destination. The destination must be confirmed prior to use.
module Network.AWS.IoT.CreateTopicRuleDestination
    (
    -- * Creating a request
      CreateTopicRuleDestination (..)
    , mkCreateTopicRuleDestination
    -- ** Request lenses
    , ctrdDestinationConfiguration

    -- * Destructuring the response
    , CreateTopicRuleDestinationResponse (..)
    , mkCreateTopicRuleDestinationResponse
    -- ** Response lenses
    , crsTopicRuleDestination
    , crsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTopicRuleDestination' smart constructor.
newtype CreateTopicRuleDestination = CreateTopicRuleDestination'
  { destinationConfiguration :: Types.TopicRuleDestinationConfiguration
    -- ^ The topic rule destination configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopicRuleDestination' value with any optional fields omitted.
mkCreateTopicRuleDestination
    :: Types.TopicRuleDestinationConfiguration -- ^ 'destinationConfiguration'
    -> CreateTopicRuleDestination
mkCreateTopicRuleDestination destinationConfiguration
  = CreateTopicRuleDestination'{destinationConfiguration}

-- | The topic rule destination configuration.
--
-- /Note:/ Consider using 'destinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdDestinationConfiguration :: Lens.Lens' CreateTopicRuleDestination Types.TopicRuleDestinationConfiguration
ctrdDestinationConfiguration = Lens.field @"destinationConfiguration"
{-# INLINEABLE ctrdDestinationConfiguration #-}
{-# DEPRECATED destinationConfiguration "Use generic-lens or generic-optics with 'destinationConfiguration' instead"  #-}

instance Core.ToQuery CreateTopicRuleDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTopicRuleDestination where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateTopicRuleDestination where
        toJSON CreateTopicRuleDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("destinationConfiguration" Core..= destinationConfiguration)])

instance Core.AWSRequest CreateTopicRuleDestination where
        type Rs CreateTopicRuleDestination =
             CreateTopicRuleDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/destinations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTopicRuleDestinationResponse' Core.<$>
                   (x Core..:? "topicRuleDestination") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTopicRuleDestinationResponse' smart constructor.
data CreateTopicRuleDestinationResponse = CreateTopicRuleDestinationResponse'
  { topicRuleDestination :: Core.Maybe Types.TopicRuleDestination
    -- ^ The topic rule destination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopicRuleDestinationResponse' value with any optional fields omitted.
mkCreateTopicRuleDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTopicRuleDestinationResponse
mkCreateTopicRuleDestinationResponse responseStatus
  = CreateTopicRuleDestinationResponse'{topicRuleDestination =
                                          Core.Nothing,
                                        responseStatus}

-- | The topic rule destination.
--
-- /Note:/ Consider using 'topicRuleDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTopicRuleDestination :: Lens.Lens' CreateTopicRuleDestinationResponse (Core.Maybe Types.TopicRuleDestination)
crsTopicRuleDestination = Lens.field @"topicRuleDestination"
{-# INLINEABLE crsTopicRuleDestination #-}
{-# DEPRECATED topicRuleDestination "Use generic-lens or generic-optics with 'topicRuleDestination' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateTopicRuleDestinationResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
