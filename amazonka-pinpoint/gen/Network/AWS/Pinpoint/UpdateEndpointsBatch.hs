{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEndpointsBatch' smart constructor.
data UpdateEndpointsBatch = UpdateEndpointsBatch'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    endpointBatchRequest :: EndpointBatchRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
updateEndpointsBatch_applicationId :: Lens.Lens' UpdateEndpointsBatch Prelude.Text
updateEndpointsBatch_applicationId = Lens.lens (\UpdateEndpointsBatch' {applicationId} -> applicationId) (\s@UpdateEndpointsBatch' {} a -> s {applicationId = a} :: UpdateEndpointsBatch)

-- | Undocumented member.
updateEndpointsBatch_endpointBatchRequest :: Lens.Lens' UpdateEndpointsBatch EndpointBatchRequest
updateEndpointsBatch_endpointBatchRequest = Lens.lens (\UpdateEndpointsBatch' {endpointBatchRequest} -> endpointBatchRequest) (\s@UpdateEndpointsBatch' {} a -> s {endpointBatchRequest = a} :: UpdateEndpointsBatch)

instance Prelude.AWSRequest UpdateEndpointsBatch where
  type
    Rs UpdateEndpointsBatch =
      UpdateEndpointsBatchResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointsBatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateEndpointsBatch

instance Prelude.NFData UpdateEndpointsBatch

instance Prelude.ToHeaders UpdateEndpointsBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateEndpointsBatch where
  toJSON UpdateEndpointsBatch' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EndpointBatchRequest"
                  Prelude..= endpointBatchRequest
              )
          ]
      )

instance Prelude.ToPath UpdateEndpointsBatch where
  toPath UpdateEndpointsBatch' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/endpoints"
      ]

instance Prelude.ToQuery UpdateEndpointsBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointsBatchResponse' smart constructor.
data UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
updateEndpointsBatchResponse_httpStatus :: Lens.Lens' UpdateEndpointsBatchResponse Prelude.Int
updateEndpointsBatchResponse_httpStatus = Lens.lens (\UpdateEndpointsBatchResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointsBatchResponse' {} a -> s {httpStatus = a} :: UpdateEndpointsBatchResponse)

-- | Undocumented member.
updateEndpointsBatchResponse_messageBody :: Lens.Lens' UpdateEndpointsBatchResponse MessageBody
updateEndpointsBatchResponse_messageBody = Lens.lens (\UpdateEndpointsBatchResponse' {messageBody} -> messageBody) (\s@UpdateEndpointsBatchResponse' {} a -> s {messageBody = a} :: UpdateEndpointsBatchResponse)

instance Prelude.NFData UpdateEndpointsBatchResponse
