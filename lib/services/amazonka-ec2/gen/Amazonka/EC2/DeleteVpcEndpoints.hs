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
-- Module      : Amazonka.EC2.DeleteVpcEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC endpoints.
--
-- When you delete a gateway endpoint, we delete the endpoint routes in the
-- route tables for the endpoint.
--
-- When you delete a Gateway Load Balancer endpoint, we delete its endpoint
-- network interfaces. You can only delete Gateway Load Balancer endpoints
-- when the routes that are associated with the endpoint are deleted.
--
-- When you delete an interface endpoint, we delete its endpoint network
-- interfaces.
module Amazonka.EC2.DeleteVpcEndpoints
  ( -- * Creating a Request
    DeleteVpcEndpoints (..),
    newDeleteVpcEndpoints,

    -- * Request Lenses
    deleteVpcEndpoints_dryRun,
    deleteVpcEndpoints_vpcEndpointIds,

    -- * Destructuring the Response
    DeleteVpcEndpointsResponse (..),
    newDeleteVpcEndpointsResponse,

    -- * Response Lenses
    deleteVpcEndpointsResponse_unsuccessful,
    deleteVpcEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcEndpoints' smart constructor.
data DeleteVpcEndpoints = DeleteVpcEndpoints'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the VPC endpoints.
    vpcEndpointIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpcEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcEndpointIds', 'deleteVpcEndpoints_vpcEndpointIds' - The IDs of the VPC endpoints.
newDeleteVpcEndpoints ::
  DeleteVpcEndpoints
newDeleteVpcEndpoints =
  DeleteVpcEndpoints'
    { dryRun = Prelude.Nothing,
      vpcEndpointIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpcEndpoints_dryRun :: Lens.Lens' DeleteVpcEndpoints (Prelude.Maybe Prelude.Bool)
deleteVpcEndpoints_dryRun = Lens.lens (\DeleteVpcEndpoints' {dryRun} -> dryRun) (\s@DeleteVpcEndpoints' {} a -> s {dryRun = a} :: DeleteVpcEndpoints)

-- | The IDs of the VPC endpoints.
deleteVpcEndpoints_vpcEndpointIds :: Lens.Lens' DeleteVpcEndpoints [Prelude.Text]
deleteVpcEndpoints_vpcEndpointIds = Lens.lens (\DeleteVpcEndpoints' {vpcEndpointIds} -> vpcEndpointIds) (\s@DeleteVpcEndpoints' {} a -> s {vpcEndpointIds = a} :: DeleteVpcEndpoints) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteVpcEndpoints where
  type
    AWSResponse DeleteVpcEndpoints =
      DeleteVpcEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcEndpointsResponse'
            Prelude.<$> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcEndpoints where
  hashWithSalt _salt DeleteVpcEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData DeleteVpcEndpoints where
  rnf DeleteVpcEndpoints' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpcEndpointIds

instance Data.ToHeaders DeleteVpcEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpcEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpcEndpoints where
  toQuery DeleteVpcEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVpcEndpoints" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | /See:/ 'newDeleteVpcEndpointsResponse' smart constructor.
data DeleteVpcEndpointsResponse = DeleteVpcEndpointsResponse'
  { -- | Information about the VPC endpoints that were not successfully deleted.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'deleteVpcEndpointsResponse_unsuccessful' - Information about the VPC endpoints that were not successfully deleted.
--
-- 'httpStatus', 'deleteVpcEndpointsResponse_httpStatus' - The response's http status code.
newDeleteVpcEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcEndpointsResponse
newDeleteVpcEndpointsResponse pHttpStatus_ =
  DeleteVpcEndpointsResponse'
    { unsuccessful =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC endpoints that were not successfully deleted.
deleteVpcEndpointsResponse_unsuccessful :: Lens.Lens' DeleteVpcEndpointsResponse (Prelude.Maybe [UnsuccessfulItem])
deleteVpcEndpointsResponse_unsuccessful = Lens.lens (\DeleteVpcEndpointsResponse' {unsuccessful} -> unsuccessful) (\s@DeleteVpcEndpointsResponse' {} a -> s {unsuccessful = a} :: DeleteVpcEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteVpcEndpointsResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointsResponse Prelude.Int
deleteVpcEndpointsResponse_httpStatus = Lens.lens (\DeleteVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointsResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointsResponse)

instance Prelude.NFData DeleteVpcEndpointsResponse where
  rnf DeleteVpcEndpointsResponse' {..} =
    Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
