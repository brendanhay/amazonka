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
-- Module      : Amazonka.OpenSearchServerless.UpdateVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an OpenSearch Serverless-managed interface endpoint. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
module Amazonka.OpenSearchServerless.UpdateVpcEndpoint
  ( -- * Creating a Request
    UpdateVpcEndpoint (..),
    newUpdateVpcEndpoint,

    -- * Request Lenses
    updateVpcEndpoint_addSecurityGroupIds,
    updateVpcEndpoint_addSubnetIds,
    updateVpcEndpoint_clientToken,
    updateVpcEndpoint_removeSecurityGroupIds,
    updateVpcEndpoint_removeSubnetIds,
    updateVpcEndpoint_id,

    -- * Destructuring the Response
    UpdateVpcEndpointResponse (..),
    newUpdateVpcEndpointResponse,

    -- * Response Lenses
    updateVpcEndpointResponse_updateVpcEndpointDetail,
    updateVpcEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVpcEndpoint' smart constructor.
data UpdateVpcEndpoint = UpdateVpcEndpoint'
  { -- | The unique identifiers of the security groups to add to the endpoint.
    -- Security groups define the ports, protocols, and sources for inbound
    -- traffic that you are authorizing into your endpoint.
    addSecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of one or more subnets to add to the endpoint.
    addSubnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers of the security groups to remove from the
    -- endpoint.
    removeSecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The unique identifiers of the subnets to remove from the endpoint.
    removeSubnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The unique identifier of the interface endpoint to update.
    id :: Prelude.Text
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
-- 'addSecurityGroupIds', 'updateVpcEndpoint_addSecurityGroupIds' - The unique identifiers of the security groups to add to the endpoint.
-- Security groups define the ports, protocols, and sources for inbound
-- traffic that you are authorizing into your endpoint.
--
-- 'addSubnetIds', 'updateVpcEndpoint_addSubnetIds' - The ID of one or more subnets to add to the endpoint.
--
-- 'clientToken', 'updateVpcEndpoint_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'removeSecurityGroupIds', 'updateVpcEndpoint_removeSecurityGroupIds' - The unique identifiers of the security groups to remove from the
-- endpoint.
--
-- 'removeSubnetIds', 'updateVpcEndpoint_removeSubnetIds' - The unique identifiers of the subnets to remove from the endpoint.
--
-- 'id', 'updateVpcEndpoint_id' - The unique identifier of the interface endpoint to update.
newUpdateVpcEndpoint ::
  -- | 'id'
  Prelude.Text ->
  UpdateVpcEndpoint
newUpdateVpcEndpoint pId_ =
  UpdateVpcEndpoint'
    { addSecurityGroupIds =
        Prelude.Nothing,
      addSubnetIds = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      removeSecurityGroupIds = Prelude.Nothing,
      removeSubnetIds = Prelude.Nothing,
      id = pId_
    }

-- | The unique identifiers of the security groups to add to the endpoint.
-- Security groups define the ports, protocols, and sources for inbound
-- traffic that you are authorizing into your endpoint.
updateVpcEndpoint_addSecurityGroupIds :: Lens.Lens' UpdateVpcEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpoint_addSecurityGroupIds = Lens.lens (\UpdateVpcEndpoint' {addSecurityGroupIds} -> addSecurityGroupIds) (\s@UpdateVpcEndpoint' {} a -> s {addSecurityGroupIds = a} :: UpdateVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of one or more subnets to add to the endpoint.
updateVpcEndpoint_addSubnetIds :: Lens.Lens' UpdateVpcEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpoint_addSubnetIds = Lens.lens (\UpdateVpcEndpoint' {addSubnetIds} -> addSubnetIds) (\s@UpdateVpcEndpoint' {} a -> s {addSubnetIds = a} :: UpdateVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
updateVpcEndpoint_clientToken :: Lens.Lens' UpdateVpcEndpoint (Prelude.Maybe Prelude.Text)
updateVpcEndpoint_clientToken = Lens.lens (\UpdateVpcEndpoint' {clientToken} -> clientToken) (\s@UpdateVpcEndpoint' {} a -> s {clientToken = a} :: UpdateVpcEndpoint)

-- | The unique identifiers of the security groups to remove from the
-- endpoint.
updateVpcEndpoint_removeSecurityGroupIds :: Lens.Lens' UpdateVpcEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpoint_removeSecurityGroupIds = Lens.lens (\UpdateVpcEndpoint' {removeSecurityGroupIds} -> removeSecurityGroupIds) (\s@UpdateVpcEndpoint' {} a -> s {removeSecurityGroupIds = a} :: UpdateVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifiers of the subnets to remove from the endpoint.
updateVpcEndpoint_removeSubnetIds :: Lens.Lens' UpdateVpcEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpoint_removeSubnetIds = Lens.lens (\UpdateVpcEndpoint' {removeSubnetIds} -> removeSubnetIds) (\s@UpdateVpcEndpoint' {} a -> s {removeSubnetIds = a} :: UpdateVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the interface endpoint to update.
updateVpcEndpoint_id :: Lens.Lens' UpdateVpcEndpoint Prelude.Text
updateVpcEndpoint_id = Lens.lens (\UpdateVpcEndpoint' {id} -> id) (\s@UpdateVpcEndpoint' {} a -> s {id = a} :: UpdateVpcEndpoint)

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
            Prelude.<$> (x Data..?> "UpdateVpcEndpointDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVpcEndpoint where
  hashWithSalt _salt UpdateVpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` addSecurityGroupIds
      `Prelude.hashWithSalt` addSubnetIds
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` removeSecurityGroupIds
      `Prelude.hashWithSalt` removeSubnetIds
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateVpcEndpoint where
  rnf UpdateVpcEndpoint' {..} =
    Prelude.rnf addSecurityGroupIds `Prelude.seq`
      Prelude.rnf addSubnetIds `Prelude.seq`
        Prelude.rnf clientToken `Prelude.seq`
          Prelude.rnf removeSecurityGroupIds `Prelude.seq`
            Prelude.rnf removeSubnetIds `Prelude.seq`
              Prelude.rnf id

instance Data.ToHeaders UpdateVpcEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateVpcEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVpcEndpoint where
  toJSON UpdateVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addSecurityGroupIds" Data..=)
              Prelude.<$> addSecurityGroupIds,
            ("addSubnetIds" Data..=) Prelude.<$> addSubnetIds,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("removeSecurityGroupIds" Data..=)
              Prelude.<$> removeSecurityGroupIds,
            ("removeSubnetIds" Data..=)
              Prelude.<$> removeSubnetIds,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateVpcEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVpcEndpointResponse' smart constructor.
data UpdateVpcEndpointResponse = UpdateVpcEndpointResponse'
  { -- | Details about the updated VPC endpoint.
    updateVpcEndpointDetail :: Prelude.Maybe UpdateVpcEndpointDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'updateVpcEndpointDetail', 'updateVpcEndpointResponse_updateVpcEndpointDetail' - Details about the updated VPC endpoint.
--
-- 'httpStatus', 'updateVpcEndpointResponse_httpStatus' - The response's http status code.
newUpdateVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVpcEndpointResponse
newUpdateVpcEndpointResponse pHttpStatus_ =
  UpdateVpcEndpointResponse'
    { updateVpcEndpointDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated VPC endpoint.
updateVpcEndpointResponse_updateVpcEndpointDetail :: Lens.Lens' UpdateVpcEndpointResponse (Prelude.Maybe UpdateVpcEndpointDetail)
updateVpcEndpointResponse_updateVpcEndpointDetail = Lens.lens (\UpdateVpcEndpointResponse' {updateVpcEndpointDetail} -> updateVpcEndpointDetail) (\s@UpdateVpcEndpointResponse' {} a -> s {updateVpcEndpointDetail = a} :: UpdateVpcEndpointResponse)

-- | The response's http status code.
updateVpcEndpointResponse_httpStatus :: Lens.Lens' UpdateVpcEndpointResponse Prelude.Int
updateVpcEndpointResponse_httpStatus = Lens.lens (\UpdateVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateVpcEndpointResponse' {} a -> s {httpStatus = a} :: UpdateVpcEndpointResponse)

instance Prelude.NFData UpdateVpcEndpointResponse where
  rnf UpdateVpcEndpointResponse' {..} =
    Prelude.rnf updateVpcEndpointDetail `Prelude.seq`
      Prelude.rnf httpStatus
