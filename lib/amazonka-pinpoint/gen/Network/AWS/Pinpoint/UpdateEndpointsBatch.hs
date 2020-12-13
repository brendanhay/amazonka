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
    uebrsMessageBody,
    uebrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEndpointsBatch' smart constructor.
data UpdateEndpointsBatch = UpdateEndpointsBatch'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    endpointBatchRequest :: EndpointBatchRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointsBatch' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endpointBatchRequest' -
mkUpdateEndpointsBatch ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'endpointBatchRequest'
  EndpointBatchRequest ->
  UpdateEndpointsBatch
mkUpdateEndpointsBatch pApplicationId_ pEndpointBatchRequest_ =
  UpdateEndpointsBatch'
    { applicationId = pApplicationId_,
      endpointBatchRequest = pEndpointBatchRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebApplicationId :: Lens.Lens' UpdateEndpointsBatch Lude.Text
uebApplicationId = Lens.lens (applicationId :: UpdateEndpointsBatch -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateEndpointsBatch)
{-# DEPRECATED uebApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointBatchRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebEndpointBatchRequest :: Lens.Lens' UpdateEndpointsBatch EndpointBatchRequest
uebEndpointBatchRequest = Lens.lens (endpointBatchRequest :: UpdateEndpointsBatch -> EndpointBatchRequest) (\s a -> s {endpointBatchRequest = a} :: UpdateEndpointsBatch)
{-# DEPRECATED uebEndpointBatchRequest "Use generic-lens or generic-optics with 'endpointBatchRequest' instead." #-}

instance Lude.AWSRequest UpdateEndpointsBatch where
  type Rs UpdateEndpointsBatch = UpdateEndpointsBatchResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEndpointsBatchResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEndpointsBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEndpointsBatch where
  toJSON UpdateEndpointsBatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EndpointBatchRequest" Lude..= endpointBatchRequest)]
      )

instance Lude.ToPath UpdateEndpointsBatch where
  toPath UpdateEndpointsBatch' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/endpoints"]

instance Lude.ToQuery UpdateEndpointsBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointsBatchResponse' smart constructor.
data UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse'
  { messageBody :: MessageBody,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointsBatchResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' -
-- * 'responseStatus' - The response status code.
mkUpdateEndpointsBatchResponse ::
  -- | 'messageBody'
  MessageBody ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEndpointsBatchResponse
mkUpdateEndpointsBatchResponse pMessageBody_ pResponseStatus_ =
  UpdateEndpointsBatchResponse'
    { messageBody = pMessageBody_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebrsMessageBody :: Lens.Lens' UpdateEndpointsBatchResponse MessageBody
uebrsMessageBody = Lens.lens (messageBody :: UpdateEndpointsBatchResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdateEndpointsBatchResponse)
{-# DEPRECATED uebrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uebrsResponseStatus :: Lens.Lens' UpdateEndpointsBatchResponse Lude.Int
uebrsResponseStatus = Lens.lens (responseStatus :: UpdateEndpointsBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointsBatchResponse)
{-# DEPRECATED uebrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
