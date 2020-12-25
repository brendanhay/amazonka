{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEndpointsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new batch of endpoints for an application or updates the settings and attributes of a batch of existing endpoints for an application. You can also use this operation to define custom attributes for a batch of endpoints. If an update includes one or more values for a custom attribute, Amazon Pinpoint replaces (overwrites) any existing values with the new values.
module Network.AWS.Pinpoint.UpdateEndpointsBatch
  ( -- * Creating a request
    UpdateEndpointsBatch (..),
    mkUpdateEndpointsBatch,

    -- ** Request lenses
    uebApplicationId,
    uebEndpointBatchRequest,

    -- * Destructuring the response
    UpdateEndpointsBatchResponse (..),
    mkUpdateEndpointsBatchResponse,

    -- ** Response lenses
    uebrrsMessageBody,
    uebrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEndpointsBatch' smart constructor.
data UpdateEndpointsBatch = UpdateEndpointsBatch'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    endpointBatchRequest :: Types.EndpointBatchRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointsBatch' value with any optional fields omitted.
mkUpdateEndpointsBatch ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'endpointBatchRequest'
  Types.EndpointBatchRequest ->
  UpdateEndpointsBatch
mkUpdateEndpointsBatch applicationId endpointBatchRequest =
  UpdateEndpointsBatch' {applicationId, endpointBatchRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebApplicationId :: Lens.Lens' UpdateEndpointsBatch Core.Text
uebApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uebApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointBatchRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebEndpointBatchRequest :: Lens.Lens' UpdateEndpointsBatch Types.EndpointBatchRequest
uebEndpointBatchRequest = Lens.field @"endpointBatchRequest"
{-# DEPRECATED uebEndpointBatchRequest "Use generic-lens or generic-optics with 'endpointBatchRequest' instead." #-}

instance Core.FromJSON UpdateEndpointsBatch where
  toJSON UpdateEndpointsBatch {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointBatchRequest" Core..= endpointBatchRequest)]
      )

instance Core.AWSRequest UpdateEndpointsBatch where
  type Rs UpdateEndpointsBatch = UpdateEndpointsBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/endpoints")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointsBatchResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEndpointsBatchResponse' smart constructor.
data UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse'
  { messageBody :: Types.MessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointsBatchResponse' value with any optional fields omitted.
mkUpdateEndpointsBatchResponse ::
  -- | 'messageBody'
  Types.MessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateEndpointsBatchResponse
mkUpdateEndpointsBatchResponse messageBody responseStatus =
  UpdateEndpointsBatchResponse' {messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebrrsMessageBody :: Lens.Lens' UpdateEndpointsBatchResponse Types.MessageBody
uebrrsMessageBody = Lens.field @"messageBody"
{-# DEPRECATED uebrrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebrrsResponseStatus :: Lens.Lens' UpdateEndpointsBatchResponse Core.Int
uebrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uebrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
