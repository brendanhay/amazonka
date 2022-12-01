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
-- Module      : Amazonka.SageMaker.UpdateEndpointWeightsAndCapacities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates variant weight of one or more variants associated with an
-- existing endpoint, or capacity of one variant associated with an
-- existing endpoint. When it receives the request, SageMaker sets the
-- endpoint status to @Updating@. After updating the endpoint, it sets the
-- status to @InService@. To check the status of an endpoint, use the
-- DescribeEndpoint API.
module Amazonka.SageMaker.UpdateEndpointWeightsAndCapacities
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateEndpointWeightsAndCapacities' smart constructor.
data UpdateEndpointWeightsAndCapacities = UpdateEndpointWeightsAndCapacities'
  { -- | The name of an existing SageMaker endpoint.
    endpointName :: Prelude.Text,
    -- | An object that provides new capacity and weight values for a variant.
    desiredWeightsAndCapacities :: Prelude.NonEmpty DesiredWeightAndCapacity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointWeightsAndCapacities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'updateEndpointWeightsAndCapacities_endpointName' - The name of an existing SageMaker endpoint.
--
-- 'desiredWeightsAndCapacities', 'updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities' - An object that provides new capacity and weight values for a variant.
newUpdateEndpointWeightsAndCapacities ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'desiredWeightsAndCapacities'
  Prelude.NonEmpty DesiredWeightAndCapacity ->
  UpdateEndpointWeightsAndCapacities
newUpdateEndpointWeightsAndCapacities
  pEndpointName_
  pDesiredWeightsAndCapacities_ =
    UpdateEndpointWeightsAndCapacities'
      { endpointName =
          pEndpointName_,
        desiredWeightsAndCapacities =
          Lens.coerced
            Lens.# pDesiredWeightsAndCapacities_
      }

-- | The name of an existing SageMaker endpoint.
updateEndpointWeightsAndCapacities_endpointName :: Lens.Lens' UpdateEndpointWeightsAndCapacities Prelude.Text
updateEndpointWeightsAndCapacities_endpointName = Lens.lens (\UpdateEndpointWeightsAndCapacities' {endpointName} -> endpointName) (\s@UpdateEndpointWeightsAndCapacities' {} a -> s {endpointName = a} :: UpdateEndpointWeightsAndCapacities)

-- | An object that provides new capacity and weight values for a variant.
updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities :: Lens.Lens' UpdateEndpointWeightsAndCapacities (Prelude.NonEmpty DesiredWeightAndCapacity)
updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities = Lens.lens (\UpdateEndpointWeightsAndCapacities' {desiredWeightsAndCapacities} -> desiredWeightsAndCapacities) (\s@UpdateEndpointWeightsAndCapacities' {} a -> s {desiredWeightsAndCapacities = a} :: UpdateEndpointWeightsAndCapacities) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateEndpointWeightsAndCapacities
  where
  type
    AWSResponse UpdateEndpointWeightsAndCapacities =
      UpdateEndpointWeightsAndCapacitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointWeightsAndCapacitiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "EndpointArn")
      )

instance
  Prelude.Hashable
    UpdateEndpointWeightsAndCapacities
  where
  hashWithSalt
    _salt
    UpdateEndpointWeightsAndCapacities' {..} =
      _salt `Prelude.hashWithSalt` endpointName
        `Prelude.hashWithSalt` desiredWeightsAndCapacities

instance
  Prelude.NFData
    UpdateEndpointWeightsAndCapacities
  where
  rnf UpdateEndpointWeightsAndCapacities' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf desiredWeightsAndCapacities

instance
  Core.ToHeaders
    UpdateEndpointWeightsAndCapacities
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateEndpointWeightsAndCapacities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateEndpointWeightsAndCapacities
  where
  toJSON UpdateEndpointWeightsAndCapacities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EndpointName" Core..= endpointName),
            Prelude.Just
              ( "DesiredWeightsAndCapacities"
                  Core..= desiredWeightsAndCapacities
              )
          ]
      )

instance
  Core.ToPath
    UpdateEndpointWeightsAndCapacities
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateEndpointWeightsAndCapacities
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointWeightsAndCapacitiesResponse' smart constructor.
data UpdateEndpointWeightsAndCapacitiesResponse = UpdateEndpointWeightsAndCapacitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'endpointArn'
  Prelude.Text ->
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
updateEndpointWeightsAndCapacitiesResponse_httpStatus :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Prelude.Int
updateEndpointWeightsAndCapacitiesResponse_httpStatus = Lens.lens (\UpdateEndpointWeightsAndCapacitiesResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointWeightsAndCapacitiesResponse' {} a -> s {httpStatus = a} :: UpdateEndpointWeightsAndCapacitiesResponse)

-- | The Amazon Resource Name (ARN) of the updated endpoint.
updateEndpointWeightsAndCapacitiesResponse_endpointArn :: Lens.Lens' UpdateEndpointWeightsAndCapacitiesResponse Prelude.Text
updateEndpointWeightsAndCapacitiesResponse_endpointArn = Lens.lens (\UpdateEndpointWeightsAndCapacitiesResponse' {endpointArn} -> endpointArn) (\s@UpdateEndpointWeightsAndCapacitiesResponse' {} a -> s {endpointArn = a} :: UpdateEndpointWeightsAndCapacitiesResponse)

instance
  Prelude.NFData
    UpdateEndpointWeightsAndCapacitiesResponse
  where
  rnf UpdateEndpointWeightsAndCapacitiesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointArn
