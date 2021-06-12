{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEndpointsBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new batch of endpoints for an application or updates the
-- settings and attributes of a batch of existing endpoints for an
-- application. You can also use this operation to define custom attributes
-- for a batch of endpoints. If an update includes one or more values for a
-- custom attribute, Amazon Pinpoint replaces (overwrites) any existing
-- values with the new values.
module Network.AWS.Pinpoint.UpdateEndpointsBatch
  ( -- * Creating a Request
    UpdateEndpointsBatch (..),
    newUpdateEndpointsBatch,

    -- * Request Lenses
    updateEndpointsBatch_applicationId,
    updateEndpointsBatch_endpointBatchRequest,

    -- * Destructuring the Response
    UpdateEndpointsBatchResponse (..),
    newUpdateEndpointsBatchResponse,

    -- * Response Lenses
    updateEndpointsBatchResponse_httpStatus,
    updateEndpointsBatchResponse_messageBody,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEndpointsBatch' smart constructor.
data UpdateEndpointsBatch = UpdateEndpointsBatch'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    endpointBatchRequest :: EndpointBatchRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpointsBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateEndpointsBatch_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointBatchRequest', 'updateEndpointsBatch_endpointBatchRequest' - Undocumented member.
newUpdateEndpointsBatch ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'endpointBatchRequest'
  EndpointBatchRequest ->
  UpdateEndpointsBatch
newUpdateEndpointsBatch
  pApplicationId_
  pEndpointBatchRequest_ =
    UpdateEndpointsBatch'
      { applicationId =
          pApplicationId_,
        endpointBatchRequest = pEndpointBatchRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateEndpointsBatch_applicationId :: Lens.Lens' UpdateEndpointsBatch Core.Text
updateEndpointsBatch_applicationId = Lens.lens (\UpdateEndpointsBatch' {applicationId} -> applicationId) (\s@UpdateEndpointsBatch' {} a -> s {applicationId = a} :: UpdateEndpointsBatch)

-- | Undocumented member.
updateEndpointsBatch_endpointBatchRequest :: Lens.Lens' UpdateEndpointsBatch EndpointBatchRequest
updateEndpointsBatch_endpointBatchRequest = Lens.lens (\UpdateEndpointsBatch' {endpointBatchRequest} -> endpointBatchRequest) (\s@UpdateEndpointsBatch' {} a -> s {endpointBatchRequest = a} :: UpdateEndpointsBatch)

instance Core.AWSRequest UpdateEndpointsBatch where
  type
    AWSResponse UpdateEndpointsBatch =
      UpdateEndpointsBatchResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointsBatchResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateEndpointsBatch

instance Core.NFData UpdateEndpointsBatch

instance Core.ToHeaders UpdateEndpointsBatch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEndpointsBatch where
  toJSON UpdateEndpointsBatch' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "EndpointBatchRequest"
                  Core..= endpointBatchRequest
              )
          ]
      )

instance Core.ToPath UpdateEndpointsBatch where
  toPath UpdateEndpointsBatch' {..} =
    Core.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/endpoints"]

instance Core.ToQuery UpdateEndpointsBatch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEndpointsBatchResponse' smart constructor.
data UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    messageBody :: MessageBody
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpointsBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointsBatchResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateEndpointsBatchResponse_messageBody' - Undocumented member.
newUpdateEndpointsBatchResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateEndpointsBatchResponse
newUpdateEndpointsBatchResponse
  pHttpStatus_
  pMessageBody_ =
    UpdateEndpointsBatchResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updateEndpointsBatchResponse_httpStatus :: Lens.Lens' UpdateEndpointsBatchResponse Core.Int
updateEndpointsBatchResponse_httpStatus = Lens.lens (\UpdateEndpointsBatchResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointsBatchResponse' {} a -> s {httpStatus = a} :: UpdateEndpointsBatchResponse)

-- | Undocumented member.
updateEndpointsBatchResponse_messageBody :: Lens.Lens' UpdateEndpointsBatchResponse MessageBody
updateEndpointsBatchResponse_messageBody = Lens.lens (\UpdateEndpointsBatchResponse' {messageBody} -> messageBody) (\s@UpdateEndpointsBatchResponse' {} a -> s {messageBody = a} :: UpdateEndpointsBatchResponse)

instance Core.NFData UpdateEndpointsBatchResponse
