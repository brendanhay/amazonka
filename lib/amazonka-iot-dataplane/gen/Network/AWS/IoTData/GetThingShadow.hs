{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetThingShadow (..),
    mkGetThingShadow,

    -- ** Request lenses
    gtsThingName,
    gtsShadowName,

    -- * Destructuring the response
    GetThingShadowResponse (..),
    mkGetThingShadowResponse,

    -- ** Response lenses
    gtsrrsPayload,
    gtsrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadow' smart constructor.
data GetThingShadow = GetThingShadow'
  { -- | The name of the thing.
    thingName :: Types.ThingName,
    -- | The name of the shadow.
    shadowName :: Core.Maybe Types.ShadowName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingShadow' value with any optional fields omitted.
mkGetThingShadow ::
  -- | 'thingName'
  Types.ThingName ->
  GetThingShadow
mkGetThingShadow thingName =
  GetThingShadow' {thingName, shadowName = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsThingName :: Lens.Lens' GetThingShadow Types.ThingName
gtsThingName = Lens.field @"thingName"
{-# DEPRECATED gtsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsShadowName :: Lens.Lens' GetThingShadow (Core.Maybe Types.ShadowName)
gtsShadowName = Lens.field @"shadowName"
{-# DEPRECATED gtsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

instance Core.AWSRequest GetThingShadow where
  type Rs GetThingShadow = GetThingShadowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/things/" Core.<> (Core.toText thingName) Core.<> ("/shadow")),
        Core._rqQuery = Core.toQueryValue "name" Core.<$> shadowName,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveBytes
      ( \s h x ->
          GetThingShadowResponse'
            Core.<$> (Core.pure x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Core.Maybe Core.ByteString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingShadowResponse' value with any optional fields omitted.
mkGetThingShadowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetThingShadowResponse
mkGetThingShadowResponse responseStatus =
  GetThingShadowResponse' {payload = Core.Nothing, responseStatus}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsPayload :: Lens.Lens' GetThingShadowResponse (Core.Maybe Core.ByteString)
gtsrrsPayload = Lens.field @"payload"
{-# DEPRECATED gtsrrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResponseStatus :: Lens.Lens' GetThingShadowResponse Core.Int
gtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
