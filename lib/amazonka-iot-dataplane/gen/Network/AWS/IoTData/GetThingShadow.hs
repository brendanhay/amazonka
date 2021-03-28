{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.GetThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html GetThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.GetThingShadow
    (
    -- * Creating a request
      GetThingShadow (..)
    , mkGetThingShadow
    -- ** Request lenses
    , gtsThingName
    , gtsShadowName

    -- * Destructuring the response
    , GetThingShadowResponse (..)
    , mkGetThingShadowResponse
    -- ** Response lenses
    , gtsrrsPayload
    , gtsrrsResponseStatus
    ) where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadow' smart constructor.
data GetThingShadow = GetThingShadow'
  { thingName :: Types.ThingName
    -- ^ The name of the thing.
  , shadowName :: Core.Maybe Types.ShadowName
    -- ^ The name of the shadow.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingShadow' value with any optional fields omitted.
mkGetThingShadow
    :: Types.ThingName -- ^ 'thingName'
    -> GetThingShadow
mkGetThingShadow thingName
  = GetThingShadow'{thingName, shadowName = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsThingName :: Lens.Lens' GetThingShadow Types.ThingName
gtsThingName = Lens.field @"thingName"
{-# INLINEABLE gtsThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsShadowName :: Lens.Lens' GetThingShadow (Core.Maybe Types.ShadowName)
gtsShadowName = Lens.field @"shadowName"
{-# INLINEABLE gtsShadowName #-}
{-# DEPRECATED shadowName "Use generic-lens or generic-optics with 'shadowName' instead"  #-}

instance Core.ToQuery GetThingShadow where
        toQuery GetThingShadow{..}
          = Core.maybe Core.mempty (Core.toQueryPair "name") shadowName

instance Core.ToHeaders GetThingShadow where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetThingShadow where
        type Rs GetThingShadow = GetThingShadowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/shadow",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 GetThingShadowResponse' Core.<$>
                   (Core.pure x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { payload :: Core.Maybe Core.ByteString
    -- ^ The state information, in JSON format.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingShadowResponse' value with any optional fields omitted.
mkGetThingShadowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetThingShadowResponse
mkGetThingShadowResponse responseStatus
  = GetThingShadowResponse'{payload = Core.Nothing, responseStatus}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsPayload :: Lens.Lens' GetThingShadowResponse (Core.Maybe Core.ByteString)
gtsrrsPayload = Lens.field @"payload"
{-# INLINEABLE gtsrrsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResponseStatus :: Lens.Lens' GetThingShadowResponse Core.Int
gtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
