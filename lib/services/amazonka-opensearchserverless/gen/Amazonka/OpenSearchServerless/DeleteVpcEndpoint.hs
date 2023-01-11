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
-- Module      : Amazonka.OpenSearchServerless.DeleteVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenSearch Serverless-managed interface endpoint. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
module Amazonka.OpenSearchServerless.DeleteVpcEndpoint
  ( -- * Creating a Request
    DeleteVpcEndpoint (..),
    newDeleteVpcEndpoint,

    -- * Request Lenses
    deleteVpcEndpoint_clientToken,
    deleteVpcEndpoint_id,

    -- * Destructuring the Response
    DeleteVpcEndpointResponse (..),
    newDeleteVpcEndpointResponse,

    -- * Response Lenses
    deleteVpcEndpointResponse_deleteVpcEndpointDetail,
    deleteVpcEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcEndpoint' smart constructor.
data DeleteVpcEndpoint = DeleteVpcEndpoint'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The VPC endpoint identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteVpcEndpoint_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'id', 'deleteVpcEndpoint_id' - The VPC endpoint identifier.
newDeleteVpcEndpoint ::
  -- | 'id'
  Prelude.Text ->
  DeleteVpcEndpoint
newDeleteVpcEndpoint pId_ =
  DeleteVpcEndpoint'
    { clientToken = Prelude.Nothing,
      id = pId_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
deleteVpcEndpoint_clientToken :: Lens.Lens' DeleteVpcEndpoint (Prelude.Maybe Prelude.Text)
deleteVpcEndpoint_clientToken = Lens.lens (\DeleteVpcEndpoint' {clientToken} -> clientToken) (\s@DeleteVpcEndpoint' {} a -> s {clientToken = a} :: DeleteVpcEndpoint)

-- | The VPC endpoint identifier.
deleteVpcEndpoint_id :: Lens.Lens' DeleteVpcEndpoint Prelude.Text
deleteVpcEndpoint_id = Lens.lens (\DeleteVpcEndpoint' {id} -> id) (\s@DeleteVpcEndpoint' {} a -> s {id = a} :: DeleteVpcEndpoint)

instance Core.AWSRequest DeleteVpcEndpoint where
  type
    AWSResponse DeleteVpcEndpoint =
      DeleteVpcEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVpcEndpointResponse'
            Prelude.<$> (x Data..?> "deleteVpcEndpointDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcEndpoint where
  hashWithSalt _salt DeleteVpcEndpoint' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteVpcEndpoint where
  rnf DeleteVpcEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteVpcEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.DeleteVpcEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVpcEndpoint where
  toJSON DeleteVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DeleteVpcEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcEndpointResponse' smart constructor.
data DeleteVpcEndpointResponse = DeleteVpcEndpointResponse'
  { -- | Details about the deleted endpoint.
    deleteVpcEndpointDetail :: Prelude.Maybe DeleteVpcEndpointDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteVpcEndpointDetail', 'deleteVpcEndpointResponse_deleteVpcEndpointDetail' - Details about the deleted endpoint.
--
-- 'httpStatus', 'deleteVpcEndpointResponse_httpStatus' - The response's http status code.
newDeleteVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcEndpointResponse
newDeleteVpcEndpointResponse pHttpStatus_ =
  DeleteVpcEndpointResponse'
    { deleteVpcEndpointDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the deleted endpoint.
deleteVpcEndpointResponse_deleteVpcEndpointDetail :: Lens.Lens' DeleteVpcEndpointResponse (Prelude.Maybe DeleteVpcEndpointDetail)
deleteVpcEndpointResponse_deleteVpcEndpointDetail = Lens.lens (\DeleteVpcEndpointResponse' {deleteVpcEndpointDetail} -> deleteVpcEndpointDetail) (\s@DeleteVpcEndpointResponse' {} a -> s {deleteVpcEndpointDetail = a} :: DeleteVpcEndpointResponse)

-- | The response's http status code.
deleteVpcEndpointResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointResponse Prelude.Int
deleteVpcEndpointResponse_httpStatus = Lens.lens (\DeleteVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointResponse)

instance Prelude.NFData DeleteVpcEndpointResponse where
  rnf DeleteVpcEndpointResponse' {..} =
    Prelude.rnf deleteVpcEndpointDetail
      `Prelude.seq` Prelude.rnf httpStatus
