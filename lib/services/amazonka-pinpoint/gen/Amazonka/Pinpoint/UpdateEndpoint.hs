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
-- Module      : Amazonka.Pinpoint.UpdateEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new endpoint for an application or updates the settings and
-- attributes of an existing endpoint for an application. You can also use
-- this operation to define custom attributes for an endpoint. If an update
-- includes one or more values for a custom attribute, Amazon Pinpoint
-- replaces (overwrites) any existing values with the new values.
module Amazonka.Pinpoint.UpdateEndpoint
  ( -- * Creating a Request
    UpdateEndpoint (..),
    newUpdateEndpoint,

    -- * Request Lenses
    updateEndpoint_applicationId,
    updateEndpoint_endpointId,
    updateEndpoint_endpointRequest,

    -- * Destructuring the Response
    UpdateEndpointResponse (..),
    newUpdateEndpointResponse,

    -- * Response Lenses
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the endpoint.
    endpointId :: Prelude.Text,
    endpointRequest :: EndpointRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateEndpoint_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointId', 'updateEndpoint_endpointId' - The unique identifier for the endpoint.
--
-- 'endpointRequest', 'updateEndpoint_endpointRequest' - Undocumented member.
newUpdateEndpoint ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  -- | 'endpointRequest'
  EndpointRequest ->
  UpdateEndpoint
newUpdateEndpoint
  pApplicationId_
  pEndpointId_
  pEndpointRequest_ =
    UpdateEndpoint'
      { applicationId = pApplicationId_,
        endpointId = pEndpointId_,
        endpointRequest = pEndpointRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateEndpoint_applicationId :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_applicationId = Lens.lens (\UpdateEndpoint' {applicationId} -> applicationId) (\s@UpdateEndpoint' {} a -> s {applicationId = a} :: UpdateEndpoint)

-- | The unique identifier for the endpoint.
updateEndpoint_endpointId :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_endpointId = Lens.lens (\UpdateEndpoint' {endpointId} -> endpointId) (\s@UpdateEndpoint' {} a -> s {endpointId = a} :: UpdateEndpoint)

-- | Undocumented member.
updateEndpoint_endpointRequest :: Lens.Lens' UpdateEndpoint EndpointRequest
updateEndpoint_endpointRequest = Lens.lens (\UpdateEndpoint' {endpointRequest} -> endpointRequest) (\s@UpdateEndpoint' {} a -> s {endpointRequest = a} :: UpdateEndpoint)

instance Core.AWSRequest UpdateEndpoint where
  type
    AWSResponse UpdateEndpoint =
      UpdateEndpointResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateEndpoint where
  hashWithSalt _salt UpdateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` endpointRequest

instance Prelude.NFData UpdateEndpoint where
  rnf UpdateEndpoint' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf endpointRequest

instance Core.ToHeaders UpdateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Core.toJSON endpointRequest

instance Core.ToPath UpdateEndpoint where
  toPath UpdateEndpoint' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/endpoints/",
        Core.toBS endpointId
      ]

instance Core.ToQuery UpdateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateEndpointResponse_messageBody' - Undocumented member.
newUpdateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateEndpointResponse
newUpdateEndpointResponse pHttpStatus_ pMessageBody_ =
  UpdateEndpointResponse'
    { httpStatus = pHttpStatus_,
      messageBody = pMessageBody_
    }

-- | The response's http status code.
updateEndpointResponse_httpStatus :: Lens.Lens' UpdateEndpointResponse Prelude.Int
updateEndpointResponse_httpStatus = Lens.lens (\UpdateEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointResponse' {} a -> s {httpStatus = a} :: UpdateEndpointResponse)

-- | Undocumented member.
updateEndpointResponse_messageBody :: Lens.Lens' UpdateEndpointResponse MessageBody
updateEndpointResponse_messageBody = Lens.lens (\UpdateEndpointResponse' {messageBody} -> messageBody) (\s@UpdateEndpointResponse' {} a -> s {messageBody = a} :: UpdateEndpointResponse)

instance Prelude.NFData UpdateEndpointResponse where
  rnf UpdateEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody
