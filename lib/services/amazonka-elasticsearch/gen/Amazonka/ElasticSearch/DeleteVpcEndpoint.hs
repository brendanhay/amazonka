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
-- Module      : Amazonka.ElasticSearch.DeleteVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon OpenSearch Service-managed interface VPC endpoint.
module Amazonka.ElasticSearch.DeleteVpcEndpoint
  ( -- * Creating a Request
    DeleteVpcEndpoint (..),
    newDeleteVpcEndpoint,

    -- * Request Lenses
    deleteVpcEndpoint_vpcEndpointId,

    -- * Destructuring the Response
    DeleteVpcEndpointResponse (..),
    newDeleteVpcEndpointResponse,

    -- * Response Lenses
    deleteVpcEndpointResponse_httpStatus,
    deleteVpcEndpointResponse_vpcEndpointSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an Amazon OpenSearch Service-managed interface VPC endpoint.
--
-- /See:/ 'newDeleteVpcEndpoint' smart constructor.
data DeleteVpcEndpoint = DeleteVpcEndpoint'
  { -- | The unique identifier of the endpoint to be deleted.
    vpcEndpointId :: Prelude.Text
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
-- 'vpcEndpointId', 'deleteVpcEndpoint_vpcEndpointId' - The unique identifier of the endpoint to be deleted.
newDeleteVpcEndpoint ::
  -- | 'vpcEndpointId'
  Prelude.Text ->
  DeleteVpcEndpoint
newDeleteVpcEndpoint pVpcEndpointId_ =
  DeleteVpcEndpoint' {vpcEndpointId = pVpcEndpointId_}

-- | The unique identifier of the endpoint to be deleted.
deleteVpcEndpoint_vpcEndpointId :: Lens.Lens' DeleteVpcEndpoint Prelude.Text
deleteVpcEndpoint_vpcEndpointId = Lens.lens (\DeleteVpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@DeleteVpcEndpoint' {} a -> s {vpcEndpointId = a} :: DeleteVpcEndpoint)

instance Core.AWSRequest DeleteVpcEndpoint where
  type
    AWSResponse DeleteVpcEndpoint =
      DeleteVpcEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVpcEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcEndpointSummary")
      )

instance Prelude.Hashable DeleteVpcEndpoint where
  hashWithSalt _salt DeleteVpcEndpoint' {..} =
    _salt `Prelude.hashWithSalt` vpcEndpointId

instance Prelude.NFData DeleteVpcEndpoint where
  rnf DeleteVpcEndpoint' {..} =
    Prelude.rnf vpcEndpointId

instance Data.ToHeaders DeleteVpcEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpcEndpoint where
  toPath DeleteVpcEndpoint' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/vpcEndpoints/",
        Data.toBS vpcEndpointId
      ]

instance Data.ToQuery DeleteVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response parameters to the @DeleteVpcEndpoint@ operation.
-- Contains the summarized detail of the VPC Endpoint being deleted.
--
-- /See:/ 'newDeleteVpcEndpointResponse' smart constructor.
data DeleteVpcEndpointResponse = DeleteVpcEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the deleted endpoint, including its current status
    -- (@DELETING@ or @DELETE_FAILED@).
    vpcEndpointSummary :: VpcEndpointSummary
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
-- 'httpStatus', 'deleteVpcEndpointResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpointSummary', 'deleteVpcEndpointResponse_vpcEndpointSummary' - Information about the deleted endpoint, including its current status
-- (@DELETING@ or @DELETE_FAILED@).
newDeleteVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcEndpointSummary'
  VpcEndpointSummary ->
  DeleteVpcEndpointResponse
newDeleteVpcEndpointResponse
  pHttpStatus_
  pVpcEndpointSummary_ =
    DeleteVpcEndpointResponse'
      { httpStatus =
          pHttpStatus_,
        vpcEndpointSummary = pVpcEndpointSummary_
      }

-- | The response's http status code.
deleteVpcEndpointResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointResponse Prelude.Int
deleteVpcEndpointResponse_httpStatus = Lens.lens (\DeleteVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointResponse)

-- | Information about the deleted endpoint, including its current status
-- (@DELETING@ or @DELETE_FAILED@).
deleteVpcEndpointResponse_vpcEndpointSummary :: Lens.Lens' DeleteVpcEndpointResponse VpcEndpointSummary
deleteVpcEndpointResponse_vpcEndpointSummary = Lens.lens (\DeleteVpcEndpointResponse' {vpcEndpointSummary} -> vpcEndpointSummary) (\s@DeleteVpcEndpointResponse' {} a -> s {vpcEndpointSummary = a} :: DeleteVpcEndpointResponse)

instance Prelude.NFData DeleteVpcEndpointResponse where
  rnf DeleteVpcEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpointSummary
