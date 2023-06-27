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
-- Module      : Amazonka.EC2.RejectVpcEndpointConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects VPC endpoint connection requests to your VPC endpoint service.
module Amazonka.EC2.RejectVpcEndpointConnections
  ( -- * Creating a Request
    RejectVpcEndpointConnections (..),
    newRejectVpcEndpointConnections,

    -- * Request Lenses
    rejectVpcEndpointConnections_dryRun,
    rejectVpcEndpointConnections_serviceId,
    rejectVpcEndpointConnections_vpcEndpointIds,

    -- * Destructuring the Response
    RejectVpcEndpointConnectionsResponse (..),
    newRejectVpcEndpointConnectionsResponse,

    -- * Response Lenses
    rejectVpcEndpointConnectionsResponse_unsuccessful,
    rejectVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectVpcEndpointConnections' smart constructor.
data RejectVpcEndpointConnections = RejectVpcEndpointConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the service.
    serviceId :: Prelude.Text,
    -- | The IDs of the VPC endpoints.
    vpcEndpointIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rejectVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'rejectVpcEndpointConnections_serviceId' - The ID of the service.
--
-- 'vpcEndpointIds', 'rejectVpcEndpointConnections_vpcEndpointIds' - The IDs of the VPC endpoints.
newRejectVpcEndpointConnections ::
  -- | 'serviceId'
  Prelude.Text ->
  RejectVpcEndpointConnections
newRejectVpcEndpointConnections pServiceId_ =
  RejectVpcEndpointConnections'
    { dryRun =
        Prelude.Nothing,
      serviceId = pServiceId_,
      vpcEndpointIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectVpcEndpointConnections_dryRun :: Lens.Lens' RejectVpcEndpointConnections (Prelude.Maybe Prelude.Bool)
rejectVpcEndpointConnections_dryRun = Lens.lens (\RejectVpcEndpointConnections' {dryRun} -> dryRun) (\s@RejectVpcEndpointConnections' {} a -> s {dryRun = a} :: RejectVpcEndpointConnections)

-- | The ID of the service.
rejectVpcEndpointConnections_serviceId :: Lens.Lens' RejectVpcEndpointConnections Prelude.Text
rejectVpcEndpointConnections_serviceId = Lens.lens (\RejectVpcEndpointConnections' {serviceId} -> serviceId) (\s@RejectVpcEndpointConnections' {} a -> s {serviceId = a} :: RejectVpcEndpointConnections)

-- | The IDs of the VPC endpoints.
rejectVpcEndpointConnections_vpcEndpointIds :: Lens.Lens' RejectVpcEndpointConnections [Prelude.Text]
rejectVpcEndpointConnections_vpcEndpointIds = Lens.lens (\RejectVpcEndpointConnections' {vpcEndpointIds} -> vpcEndpointIds) (\s@RejectVpcEndpointConnections' {} a -> s {vpcEndpointIds = a} :: RejectVpcEndpointConnections) Prelude.. Lens.coerced

instance Core.AWSRequest RejectVpcEndpointConnections where
  type
    AWSResponse RejectVpcEndpointConnections =
      RejectVpcEndpointConnectionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RejectVpcEndpointConnectionsResponse'
            Prelude.<$> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectVpcEndpointConnections
  where
  hashWithSalt _salt RejectVpcEndpointConnections' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData RejectVpcEndpointConnections where
  rnf RejectVpcEndpointConnections' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf vpcEndpointIds

instance Data.ToHeaders RejectVpcEndpointConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RejectVpcEndpointConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery RejectVpcEndpointConnections where
  toQuery RejectVpcEndpointConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RejectVpcEndpointConnections" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ServiceId" Data.=: serviceId,
        Data.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | /See:/ 'newRejectVpcEndpointConnectionsResponse' smart constructor.
data RejectVpcEndpointConnectionsResponse = RejectVpcEndpointConnectionsResponse'
  { -- | Information about the endpoints that were not rejected, if applicable.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcEndpointConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'rejectVpcEndpointConnectionsResponse_unsuccessful' - Information about the endpoints that were not rejected, if applicable.
--
-- 'httpStatus', 'rejectVpcEndpointConnectionsResponse_httpStatus' - The response's http status code.
newRejectVpcEndpointConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectVpcEndpointConnectionsResponse
newRejectVpcEndpointConnectionsResponse pHttpStatus_ =
  RejectVpcEndpointConnectionsResponse'
    { unsuccessful =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the endpoints that were not rejected, if applicable.
rejectVpcEndpointConnectionsResponse_unsuccessful :: Lens.Lens' RejectVpcEndpointConnectionsResponse (Prelude.Maybe [UnsuccessfulItem])
rejectVpcEndpointConnectionsResponse_unsuccessful = Lens.lens (\RejectVpcEndpointConnectionsResponse' {unsuccessful} -> unsuccessful) (\s@RejectVpcEndpointConnectionsResponse' {} a -> s {unsuccessful = a} :: RejectVpcEndpointConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rejectVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' RejectVpcEndpointConnectionsResponse Prelude.Int
rejectVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\RejectVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@RejectVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: RejectVpcEndpointConnectionsResponse)

instance
  Prelude.NFData
    RejectVpcEndpointConnectionsResponse
  where
  rnf RejectVpcEndpointConnectionsResponse' {..} =
    Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
