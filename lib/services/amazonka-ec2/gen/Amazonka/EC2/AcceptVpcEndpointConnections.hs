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
-- Module      : Amazonka.EC2.AcceptVpcEndpointConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts connection requests to your VPC endpoint service.
module Amazonka.EC2.AcceptVpcEndpointConnections
  ( -- * Creating a Request
    AcceptVpcEndpointConnections (..),
    newAcceptVpcEndpointConnections,

    -- * Request Lenses
    acceptVpcEndpointConnections_dryRun,
    acceptVpcEndpointConnections_serviceId,
    acceptVpcEndpointConnections_vpcEndpointIds,

    -- * Destructuring the Response
    AcceptVpcEndpointConnectionsResponse (..),
    newAcceptVpcEndpointConnectionsResponse,

    -- * Response Lenses
    acceptVpcEndpointConnectionsResponse_unsuccessful,
    acceptVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptVpcEndpointConnections' smart constructor.
data AcceptVpcEndpointConnections = AcceptVpcEndpointConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC endpoint service.
    serviceId :: Prelude.Text,
    -- | The IDs of the interface VPC endpoints.
    vpcEndpointIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'acceptVpcEndpointConnections_serviceId' - The ID of the VPC endpoint service.
--
-- 'vpcEndpointIds', 'acceptVpcEndpointConnections_vpcEndpointIds' - The IDs of the interface VPC endpoints.
newAcceptVpcEndpointConnections ::
  -- | 'serviceId'
  Prelude.Text ->
  AcceptVpcEndpointConnections
newAcceptVpcEndpointConnections pServiceId_ =
  AcceptVpcEndpointConnections'
    { dryRun =
        Prelude.Nothing,
      serviceId = pServiceId_,
      vpcEndpointIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptVpcEndpointConnections_dryRun :: Lens.Lens' AcceptVpcEndpointConnections (Prelude.Maybe Prelude.Bool)
acceptVpcEndpointConnections_dryRun = Lens.lens (\AcceptVpcEndpointConnections' {dryRun} -> dryRun) (\s@AcceptVpcEndpointConnections' {} a -> s {dryRun = a} :: AcceptVpcEndpointConnections)

-- | The ID of the VPC endpoint service.
acceptVpcEndpointConnections_serviceId :: Lens.Lens' AcceptVpcEndpointConnections Prelude.Text
acceptVpcEndpointConnections_serviceId = Lens.lens (\AcceptVpcEndpointConnections' {serviceId} -> serviceId) (\s@AcceptVpcEndpointConnections' {} a -> s {serviceId = a} :: AcceptVpcEndpointConnections)

-- | The IDs of the interface VPC endpoints.
acceptVpcEndpointConnections_vpcEndpointIds :: Lens.Lens' AcceptVpcEndpointConnections [Prelude.Text]
acceptVpcEndpointConnections_vpcEndpointIds = Lens.lens (\AcceptVpcEndpointConnections' {vpcEndpointIds} -> vpcEndpointIds) (\s@AcceptVpcEndpointConnections' {} a -> s {vpcEndpointIds = a} :: AcceptVpcEndpointConnections) Prelude.. Lens.coerced

instance Core.AWSRequest AcceptVpcEndpointConnections where
  type
    AWSResponse AcceptVpcEndpointConnections =
      AcceptVpcEndpointConnectionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptVpcEndpointConnectionsResponse'
            Prelude.<$> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptVpcEndpointConnections
  where
  hashWithSalt _salt AcceptVpcEndpointConnections' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData AcceptVpcEndpointConnections where
  rnf AcceptVpcEndpointConnections' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf vpcEndpointIds

instance Data.ToHeaders AcceptVpcEndpointConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AcceptVpcEndpointConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptVpcEndpointConnections where
  toQuery AcceptVpcEndpointConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AcceptVpcEndpointConnections" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ServiceId" Data.=: serviceId,
        Data.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | /See:/ 'newAcceptVpcEndpointConnectionsResponse' smart constructor.
data AcceptVpcEndpointConnectionsResponse = AcceptVpcEndpointConnectionsResponse'
  { -- | Information about the interface endpoints that were not accepted, if
    -- applicable.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptVpcEndpointConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'acceptVpcEndpointConnectionsResponse_unsuccessful' - Information about the interface endpoints that were not accepted, if
-- applicable.
--
-- 'httpStatus', 'acceptVpcEndpointConnectionsResponse_httpStatus' - The response's http status code.
newAcceptVpcEndpointConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptVpcEndpointConnectionsResponse
newAcceptVpcEndpointConnectionsResponse pHttpStatus_ =
  AcceptVpcEndpointConnectionsResponse'
    { unsuccessful =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the interface endpoints that were not accepted, if
-- applicable.
acceptVpcEndpointConnectionsResponse_unsuccessful :: Lens.Lens' AcceptVpcEndpointConnectionsResponse (Prelude.Maybe [UnsuccessfulItem])
acceptVpcEndpointConnectionsResponse_unsuccessful = Lens.lens (\AcceptVpcEndpointConnectionsResponse' {unsuccessful} -> unsuccessful) (\s@AcceptVpcEndpointConnectionsResponse' {} a -> s {unsuccessful = a} :: AcceptVpcEndpointConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
acceptVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' AcceptVpcEndpointConnectionsResponse Prelude.Int
acceptVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\AcceptVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@AcceptVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: AcceptVpcEndpointConnectionsResponse)

instance
  Prelude.NFData
    AcceptVpcEndpointConnectionsResponse
  where
  rnf AcceptVpcEndpointConnectionsResponse' {..} =
    Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
