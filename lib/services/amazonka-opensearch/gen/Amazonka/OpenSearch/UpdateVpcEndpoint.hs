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
-- Module      : Amazonka.OpenSearch.UpdateVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an Amazon OpenSearch Service-managed interface VPC endpoint.
module Amazonka.OpenSearch.UpdateVpcEndpoint
  ( -- * Creating a Request
    UpdateVpcEndpoint (..),
    newUpdateVpcEndpoint,

    -- * Request Lenses
    updateVpcEndpoint_vpcEndpointId,
    updateVpcEndpoint_vpcOptions,

    -- * Destructuring the Response
    UpdateVpcEndpointResponse (..),
    newUpdateVpcEndpointResponse,

    -- * Response Lenses
    updateVpcEndpointResponse_httpStatus,
    updateVpcEndpointResponse_vpcEndpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVpcEndpoint' smart constructor.
data UpdateVpcEndpoint = UpdateVpcEndpoint'
  { -- | The unique identifier of the endpoint.
    vpcEndpointId :: Prelude.Text,
    -- | The security groups and\/or subnets to add, remove, or modify.
    vpcOptions :: VPCOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointId', 'updateVpcEndpoint_vpcEndpointId' - The unique identifier of the endpoint.
--
-- 'vpcOptions', 'updateVpcEndpoint_vpcOptions' - The security groups and\/or subnets to add, remove, or modify.
newUpdateVpcEndpoint ::
  -- | 'vpcEndpointId'
  Prelude.Text ->
  -- | 'vpcOptions'
  VPCOptions ->
  UpdateVpcEndpoint
newUpdateVpcEndpoint pVpcEndpointId_ pVpcOptions_ =
  UpdateVpcEndpoint'
    { vpcEndpointId = pVpcEndpointId_,
      vpcOptions = pVpcOptions_
    }

-- | The unique identifier of the endpoint.
updateVpcEndpoint_vpcEndpointId :: Lens.Lens' UpdateVpcEndpoint Prelude.Text
updateVpcEndpoint_vpcEndpointId = Lens.lens (\UpdateVpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@UpdateVpcEndpoint' {} a -> s {vpcEndpointId = a} :: UpdateVpcEndpoint)

-- | The security groups and\/or subnets to add, remove, or modify.
updateVpcEndpoint_vpcOptions :: Lens.Lens' UpdateVpcEndpoint VPCOptions
updateVpcEndpoint_vpcOptions = Lens.lens (\UpdateVpcEndpoint' {vpcOptions} -> vpcOptions) (\s@UpdateVpcEndpoint' {} a -> s {vpcOptions = a} :: UpdateVpcEndpoint)

instance Core.AWSRequest UpdateVpcEndpoint where
  type
    AWSResponse UpdateVpcEndpoint =
      UpdateVpcEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVpcEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcEndpoint")
      )

instance Prelude.Hashable UpdateVpcEndpoint where
  hashWithSalt _salt UpdateVpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcOptions

instance Prelude.NFData UpdateVpcEndpoint where
  rnf UpdateVpcEndpoint' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcOptions

instance Data.ToHeaders UpdateVpcEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateVpcEndpoint where
  toJSON UpdateVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VpcEndpointId" Data..= vpcEndpointId),
            Prelude.Just ("VpcOptions" Data..= vpcOptions)
          ]
      )

instance Data.ToPath UpdateVpcEndpoint where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/vpcEndpoints/update"

instance Data.ToQuery UpdateVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVpcEndpointResponse' smart constructor.
data UpdateVpcEndpointResponse = UpdateVpcEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The endpoint to be updated.
    vpcEndpoint :: VpcEndpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVpcEndpointResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpoint', 'updateVpcEndpointResponse_vpcEndpoint' - The endpoint to be updated.
newUpdateVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcEndpoint'
  VpcEndpoint ->
  UpdateVpcEndpointResponse
newUpdateVpcEndpointResponse
  pHttpStatus_
  pVpcEndpoint_ =
    UpdateVpcEndpointResponse'
      { httpStatus =
          pHttpStatus_,
        vpcEndpoint = pVpcEndpoint_
      }

-- | The response's http status code.
updateVpcEndpointResponse_httpStatus :: Lens.Lens' UpdateVpcEndpointResponse Prelude.Int
updateVpcEndpointResponse_httpStatus = Lens.lens (\UpdateVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateVpcEndpointResponse' {} a -> s {httpStatus = a} :: UpdateVpcEndpointResponse)

-- | The endpoint to be updated.
updateVpcEndpointResponse_vpcEndpoint :: Lens.Lens' UpdateVpcEndpointResponse VpcEndpoint
updateVpcEndpointResponse_vpcEndpoint = Lens.lens (\UpdateVpcEndpointResponse' {vpcEndpoint} -> vpcEndpoint) (\s@UpdateVpcEndpointResponse' {} a -> s {vpcEndpoint = a} :: UpdateVpcEndpointResponse)

instance Prelude.NFData UpdateVpcEndpointResponse where
  rnf UpdateVpcEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpoint
