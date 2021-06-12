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
-- Module      : Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates variant weight of one or more variants associated with an
-- existing endpoint, or capacity of one variant associated with an
-- existing endpoint. When it receives the request, Amazon SageMaker sets
-- the endpoint status to @Updating@. After updating the endpoint, it sets
-- the status to @InService@. To check the status of an endpoint, use the
-- DescribeEndpoint API.
module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
  ( -- * Creating a Request
    UpdateEndpointWeightsAndCapacities (..),
    newUpdateEndpointWeightsAndCapacities,

    -- * Request Lenses
    updateEndpointWeightsAndCapacities_endpointName,
    updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities,

    -- * Destructuring the Response
    UpdateEndpointWeightsAndCapacitiesResponse (..),
    newUpdateEndpointWeightsAndCapacitiesResponse,

    -- * Response Lenses
    updateEndpointWeightsAndCapacitiesResponse_httpStatus,
    updateEndpointWeightsAndCapacitiesResponse_endpointArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateEndpointWeightsAndCapacities' smart constructor.
data UpdateEndpointWeightsAndCapacities = UpdateEndpointWeightsAndCapacities'
  { -- | The name of an existing Amazon SageMaker endpoint.
    endpointName :: Core.Text,
    -- | An object that provides new capacity and weight values for a variant.
    desiredWeightsAndCapacities :: Core.NonEmpty DesiredWeightAndCapacity
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpointWeightsAndCapacities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'updateEndpointWeightsAndCapacities_endpointName' - The name of an existing Amazon SageMaker endpoint.
--
-- 'desiredWeightsAndCapacities', 'updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities' - An object that provides new capacity and weight values for a variant.
newUpdateEndpointWeightsAndCapacities ::
  -- | 'endpointName'
  Core.Text ->
  -- | 'desiredWeightsAndCapacities'
  Core.NonEmpty DesiredWeightAndCapacity ->
  UpdateEndpointWeightsAndCapacities
newUpdateEndpointWeightsAndCapacities
  pEndpointName_
  pDesiredWeightsAndCapacities_ =
    UpdateEndpointWeightsAndCapacities'
      { endpointName =
          pEndpointName_,
        desiredWeightsAndCapacities =
          Lens._Coerce
            Lens.# pDesiredWeightsAndCapacities_
      }

-- | The name of an existing Amazon SageMaker endpoint.
updateEndpointWeightsAndCapacities_endpointName :: Lens.Lens' UpdateEndpointWeightsAndCapacities Core.Text
updateEndpointWeightsAndCapacities_endpointName = Lens.lens (\UpdateEndpointWeightsAndCapacities' {endpointName} -> endpointName) (\s@UpdateEndpointWeightsAndCapacities' {} a -> s {endpointName = a} :: UpdateEndpointWeightsAndCapacities)

-- | An object that provides new capacity and weight values for a variant.
updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities :: Lens.Lens' UpdateEndpointWeightsAndCapacities (Core.NonEmpty DesiredWeightAndCapacity)
updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities = Lens.lens (\UpdateEndpointWeightsAndCapacities' {desiredWeightsAndCapacities} -> desiredWeightsAndCapacities) (\s@UpdateEndpointWeightsAndCapacities' {} a -> s {desiredWeightsAndCapacities = a} :: UpdateEndpointWeightsAndCapacities) Core.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateEndpointWeightsAndCapacities
  where
  type
    AWSResponse UpdateEndpointWeightsAndCapacities =
      UpdateEndpointWeightsAndCapacitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointWeightsAndCapacitiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "EndpointArn")
      )

instance
  Core.Hashable
    UpdateEndpointWeightsAndCapacities

instance
  Core.NFData
    UpdateEndpointWeightsAndCapacities

instance
  Core.ToHeaders
    UpdateEndpointWeightsAndCapacities
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateEndpointWeightsAndCapacities" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateEndpointWeightsAndCapacities
  where
  toJSON UpdateEndpointWeightsAndCapacities' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointName" Core..= endpointName),
            Core.Just
              ( "DesiredWeightsAndCapacities"
                  Core..= desiredWeightsAndCapacities
              )
          ]
      )

instance
  Core.ToPath
    UpdateEndpointWeightsAndCapacities
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateEndpointWeightsAndCapacities
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEndpointWeightsAndCapacitiesResponse' smart constructor.
data UpdateEndpointWeightsAndCapacitiesResponse = UpdateEndpointWeightsAndCapacitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the updated endpoint.
    endpointArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEndpointWeightsAndCapacitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointWeightsAndCapacitiesResponse_httpStatus' - The response's http status code.
--
-- 'endpointArn', 'updateEndpointWeightsAndCapacitiesResponse_endpointArn' - The Amazon Resource Name (ARN) of the updated endpoint.
newUpdateEndpointWeightsAndCapacitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'endpointArn'
  Core.Text ->
  UpdateEndpointWeightsAndCapacitiesResponse
newUpdateEndpointWeightsAndCapacitiesResponse
  pHttpStatus_
  pEndpointArn_ =
    UpdateEndpointWeightsAndCapacitiesResponse'
      { httpStatus =
          pHttpStatus_,
        endpointArn = pEndpointArn_
      }

-- | The response's http status code.
updateEndpointWeightsAndCapacitiesResponse_httpStatus :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Core.Int
updateEndpointWeightsAndCapacitiesResponse_httpStatus = Lens.lens (\UpdateEndpointWeightsAndCapacitiesResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointWeightsAndCapacitiesResponse' {} a -> s {httpStatus = a} :: UpdateEndpointWeightsAndCapacitiesResponse)

-- | The Amazon Resource Name (ARN) of the updated endpoint.
updateEndpointWeightsAndCapacitiesResponse_endpointArn :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Core.Text
updateEndpointWeightsAndCapacitiesResponse_endpointArn = Lens.lens (\UpdateEndpointWeightsAndCapacitiesResponse' {endpointArn} -> endpointArn) (\s@UpdateEndpointWeightsAndCapacitiesResponse' {} a -> s {endpointArn = a} :: UpdateEndpointWeightsAndCapacitiesResponse)

instance
  Core.NFData
    UpdateEndpointWeightsAndCapacitiesResponse
