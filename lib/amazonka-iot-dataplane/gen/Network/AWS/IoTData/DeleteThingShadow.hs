{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.DeleteThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html DeleteThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.DeleteThingShadow
  ( -- * Creating a request
    DeleteThingShadow (..),
    mkDeleteThingShadow,

    -- ** Request lenses
    dtsThingName,
    dtsShadowName,

    -- * Destructuring the response
    DeleteThingShadowResponse (..),
    mkDeleteThingShadowResponse,

    -- ** Response lenses
    dtsrrsPayload,
    dtsrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingShadow operation.
--
-- /See:/ 'mkDeleteThingShadow' smart constructor.
data DeleteThingShadow = DeleteThingShadow'
  { -- | The name of the thing.
    thingName :: Types.ThingName,
    -- | The name of the shadow.
    shadowName :: Core.Maybe Types.ShadowName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingShadow' value with any optional fields omitted.
mkDeleteThingShadow ::
  -- | 'thingName'
  Types.ThingName ->
  DeleteThingShadow
mkDeleteThingShadow thingName =
  DeleteThingShadow' {thingName, shadowName = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsThingName :: Lens.Lens' DeleteThingShadow Types.ThingName
dtsThingName = Lens.field @"thingName"
{-# DEPRECATED dtsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsShadowName :: Lens.Lens' DeleteThingShadow (Core.Maybe Types.ShadowName)
dtsShadowName = Lens.field @"shadowName"
{-# DEPRECATED dtsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

instance Core.AWSRequest DeleteThingShadow where
  type Rs DeleteThingShadow = DeleteThingShadowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
          DeleteThingShadowResponse'
            Core.<$> (Core.pure (Core.Just x)) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the DeleteThingShadow operation.
--
-- /See:/ 'mkDeleteThingShadowResponse' smart constructor.
data DeleteThingShadowResponse = DeleteThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Core.ByteString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingShadowResponse' value with any optional fields omitted.
mkDeleteThingShadowResponse ::
  -- | 'payload'
  Core.ByteString ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteThingShadowResponse
mkDeleteThingShadowResponse payload responseStatus =
  DeleteThingShadowResponse' {payload, responseStatus}

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsPayload :: Lens.Lens' DeleteThingShadowResponse Core.ByteString
dtsrrsPayload = Lens.field @"payload"
{-# DEPRECATED dtsrrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsResponseStatus :: Lens.Lens' DeleteThingShadowResponse Core.Int
dtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
