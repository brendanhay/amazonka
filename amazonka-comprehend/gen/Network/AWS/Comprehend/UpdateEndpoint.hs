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
-- Module      : Network.AWS.Comprehend.UpdateEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the specified endpoint.
module Network.AWS.Comprehend.UpdateEndpoint
  ( -- * Creating a Request
    UpdateEndpoint (..),
    newUpdateEndpoint,

    -- * Request Lenses
    updateEndpoint_endpointArn,
    updateEndpoint_desiredInferenceUnits,

    -- * Destructuring the Response
    UpdateEndpointResponse (..),
    newUpdateEndpointResponse,

    -- * Response Lenses
    updateEndpointResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The Amazon Resource Number (ARN) of the endpoint being updated.
    endpointArn :: Core.Text,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'updateEndpoint_endpointArn' - The Amazon Resource Number (ARN) of the endpoint being updated.
--
-- 'desiredInferenceUnits', 'updateEndpoint_desiredInferenceUnits' - The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
newUpdateEndpoint ::
  -- | 'endpointArn'
  Core.Text ->
  -- | 'desiredInferenceUnits'
  Core.Natural ->
  UpdateEndpoint
newUpdateEndpoint
  pEndpointArn_
  pDesiredInferenceUnits_ =
    UpdateEndpoint'
      { endpointArn = pEndpointArn_,
        desiredInferenceUnits = pDesiredInferenceUnits_
      }

-- | The Amazon Resource Number (ARN) of the endpoint being updated.
updateEndpoint_endpointArn :: Lens.Lens' UpdateEndpoint Core.Text
updateEndpoint_endpointArn = Lens.lens (\UpdateEndpoint' {endpointArn} -> endpointArn) (\s@UpdateEndpoint' {} a -> s {endpointArn = a} :: UpdateEndpoint)

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
updateEndpoint_desiredInferenceUnits :: Lens.Lens' UpdateEndpoint Core.Natural
updateEndpoint_desiredInferenceUnits = Lens.lens (\UpdateEndpoint' {desiredInferenceUnits} -> desiredInferenceUnits) (\s@UpdateEndpoint' {} a -> s {desiredInferenceUnits = a} :: UpdateEndpoint)

instance Core.AWSRequest UpdateEndpoint where
  type
    AWSResponse UpdateEndpoint =
      UpdateEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEndpointResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateEndpoint

instance Core.NFData UpdateEndpoint

instance Core.ToHeaders UpdateEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.UpdateEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just
              ( "DesiredInferenceUnits"
                  Core..= desiredInferenceUnits
              )
          ]
      )

instance Core.ToPath UpdateEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery UpdateEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointResponse_httpStatus' - The response's http status code.
newUpdateEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateEndpointResponse
newUpdateEndpointResponse pHttpStatus_ =
  UpdateEndpointResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateEndpointResponse_httpStatus :: Lens.Lens' UpdateEndpointResponse Core.Int
updateEndpointResponse_httpStatus = Lens.lens (\UpdateEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointResponse' {} a -> s {httpStatus = a} :: UpdateEndpointResponse)

instance Core.NFData UpdateEndpointResponse
