{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.Publish
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes state information.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http HTTP Protocol> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.Publish
    (
    -- * Creating a request
      Publish (..)
    , mkPublish
    -- ** Request lenses
    , pTopic
    , pPayload
    , pQos

    -- * Destructuring the response
    , PublishResponse (..)
    , mkPublishResponse
    ) where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the Publish operation.
--
-- /See:/ 'mkPublish' smart constructor.
data Publish = Publish'
  { topic :: Types.Topic
    -- ^ The name of the MQTT topic.
  , payload :: Core.Maybe Core.ByteString
    -- ^ The state information, in JSON format.
  , qos :: Core.Maybe Core.Natural
    -- ^ The Quality of Service (QoS) level.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Publish' value with any optional fields omitted.
mkPublish
    :: Types.Topic -- ^ 'topic'
    -> Publish
mkPublish topic
  = Publish'{topic, payload = Core.Nothing, qos = Core.Nothing}

-- | The name of the MQTT topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTopic :: Lens.Lens' Publish Types.Topic
pTopic = Lens.field @"topic"
{-# INLINEABLE pTopic #-}
{-# DEPRECATED topic "Use generic-lens or generic-optics with 'topic' instead"  #-}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPayload :: Lens.Lens' Publish (Core.Maybe Core.ByteString)
pPayload = Lens.field @"payload"
{-# INLINEABLE pPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The Quality of Service (QoS) level.
--
-- /Note:/ Consider using 'qos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQos :: Lens.Lens' Publish (Core.Maybe Core.Natural)
pQos = Lens.field @"qos"
{-# INLINEABLE pQos #-}
{-# DEPRECATED qos "Use generic-lens or generic-optics with 'qos' instead"  #-}

instance Core.ToQuery Publish where
        toQuery Publish{..}
          = Core.maybe Core.mempty (Core.toQueryPair "qos") qos

instance Core.ToHeaders Publish where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest Publish where
        type Rs Publish = PublishResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/topics/" Core.<> Core.toText topic,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody payload}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PublishResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishResponse' value with any optional fields omitted.
mkPublishResponse
    :: PublishResponse
mkPublishResponse = PublishResponse'
