{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.UpdateThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html UpdateThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.UpdateThingShadow
    (
    -- * Creating a request
      UpdateThingShadow (..)
    , mkUpdateThingShadow
    -- ** Request lenses
    , utsThingName
    , utsPayload
    , utsShadowName

    -- * Destructuring the response
    , UpdateThingShadowResponse (..)
    , mkUpdateThingShadowResponse
    -- ** Response lenses
    , utsrrsPayload
    , utsrrsResponseStatus
    ) where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadow' smart constructor.
data UpdateThingShadow = UpdateThingShadow'
  { thingName :: Types.ThingName
    -- ^ The name of the thing.
  , payload :: Core.ByteString
    -- ^ The state information, in JSON format.
  , shadowName :: Core.Maybe Types.ShadowName
    -- ^ The name of the shadow.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingShadow' value with any optional fields omitted.
mkUpdateThingShadow
    :: Types.ThingName -- ^ 'thingName'
    -> Core.ByteString -- ^ 'payload'
    -> UpdateThingShadow
mkUpdateThingShadow thingName payload
  = UpdateThingShadow'{thingName, payload, shadowName = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsThingName :: Lens.Lens' UpdateThingShadow Types.ThingName
utsThingName = Lens.field @"thingName"
{-# INLINEABLE utsThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsPayload :: Lens.Lens' UpdateThingShadow Core.ByteString
utsPayload = Lens.field @"payload"
{-# INLINEABLE utsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsShadowName :: Lens.Lens' UpdateThingShadow (Core.Maybe Types.ShadowName)
utsShadowName = Lens.field @"shadowName"
{-# INLINEABLE utsShadowName #-}
{-# DEPRECATED shadowName "Use generic-lens or generic-optics with 'shadowName' instead"  #-}

instance Core.ToQuery UpdateThingShadow where
        toQuery UpdateThingShadow{..}
          = Core.maybe Core.mempty (Core.toQueryPair "name") shadowName

instance Core.ToHeaders UpdateThingShadow where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateThingShadow where
        type Rs UpdateThingShadow = UpdateThingShadowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/shadow",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody payload}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 UpdateThingShadowResponse' Core.<$>
                   (Core.pure x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadowResponse' smart constructor.
data UpdateThingShadowResponse = UpdateThingShadowResponse'
  { payload :: Core.Maybe Core.ByteString
    -- ^ The state information, in JSON format.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingShadowResponse' value with any optional fields omitted.
mkUpdateThingShadowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateThingShadowResponse
mkUpdateThingShadowResponse responseStatus
  = UpdateThingShadowResponse'{payload = Core.Nothing,
                               responseStatus}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsPayload :: Lens.Lens' UpdateThingShadowResponse (Core.Maybe Core.ByteString)
utsrrsPayload = Lens.field @"payload"
{-# INLINEABLE utsrrsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsResponseStatus :: Lens.Lens' UpdateThingShadowResponse Core.Int
utsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
