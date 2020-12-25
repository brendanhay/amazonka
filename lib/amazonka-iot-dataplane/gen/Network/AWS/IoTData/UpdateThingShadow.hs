{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateThingShadow (..),
    mkUpdateThingShadow,

    -- ** Request lenses
    utsThingName,
    utsPayload,
    utsShadowName,

    -- * Destructuring the response
    UpdateThingShadowResponse (..),
    mkUpdateThingShadowResponse,

    -- ** Response lenses
    utsrrsPayload,
    utsrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadow' smart constructor.
data UpdateThingShadow = UpdateThingShadow'
  { -- | The name of the thing.
    thingName :: Types.ThingName,
    -- | The state information, in JSON format.
    payload :: Core.ByteString,
    -- | The name of the shadow.
    shadowName :: Core.Maybe Types.ShadowName
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingShadow' value with any optional fields omitted.
mkUpdateThingShadow ::
  -- | 'thingName'
  Types.ThingName ->
  -- | 'payload'
  Core.ByteString ->
  UpdateThingShadow
mkUpdateThingShadow thingName payload =
  UpdateThingShadow' {thingName, payload, shadowName = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsThingName :: Lens.Lens' UpdateThingShadow Types.ThingName
utsThingName = Lens.field @"thingName"
{-# DEPRECATED utsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsPayload :: Lens.Lens' UpdateThingShadow Core.ByteString
utsPayload = Lens.field @"payload"
{-# DEPRECATED utsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsShadowName :: Lens.Lens' UpdateThingShadow (Core.Maybe Types.ShadowName)
utsShadowName = Lens.field @"shadowName"
{-# DEPRECATED utsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

instance Core.AWSRequest UpdateThingShadow where
  type Rs UpdateThingShadow = UpdateThingShadowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/things/" Core.<> (Core.toText thingName) Core.<> ("/shadow")),
        Core._rqQuery = Core.toQueryValue "name" Core.<$> shadowName,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toBody payload
      }
  response =
    Response.receiveBytes
      ( \s h x ->
          UpdateThingShadowResponse'
            Core.<$> (Core.pure x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadowResponse' smart constructor.
data UpdateThingShadowResponse = UpdateThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Core.Maybe Core.ByteString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingShadowResponse' value with any optional fields omitted.
mkUpdateThingShadowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateThingShadowResponse
mkUpdateThingShadowResponse responseStatus =
  UpdateThingShadowResponse'
    { payload = Core.Nothing,
      responseStatus
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsPayload :: Lens.Lens' UpdateThingShadowResponse (Core.Maybe Core.ByteString)
utsrrsPayload = Lens.field @"payload"
{-# DEPRECATED utsrrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsResponseStatus :: Lens.Lens' UpdateThingShadowResponse Core.Int
utsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
