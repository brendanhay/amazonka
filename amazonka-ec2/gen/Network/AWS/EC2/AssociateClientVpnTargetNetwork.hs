{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.AssociateClientVpnTargetNetwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a target network with a Client VPN endpoint. A target network
-- is a subnet in a VPC. You can associate multiple subnets from the same
-- VPC with a Client VPN endpoint. You can associate only one subnet in
-- each Availability Zone. We recommend that you associate at least two
-- subnets to provide Availability Zone redundancy.
--
-- If you specified a VPC when you created the Client VPN endpoint or if
-- you have previous subnet associations, the specified subnet must be in
-- the same VPC. To specify a subnet that\'s in a different VPC, you must
-- first modify the Client VPN endpoint (ModifyClientVpnEndpoint) and
-- change the VPC that\'s associated with it.
module Network.AWS.EC2.AssociateClientVpnTargetNetwork
  ( -- * Creating a Request
    AssociateClientVpnTargetNetwork (..),
    newAssociateClientVpnTargetNetwork,

    -- * Request Lenses
    associateClientVpnTargetNetwork_dryRun,
    associateClientVpnTargetNetwork_clientToken,
    associateClientVpnTargetNetwork_clientVpnEndpointId,
    associateClientVpnTargetNetwork_subnetId,

    -- * Destructuring the Response
    AssociateClientVpnTargetNetworkResponse (..),
    newAssociateClientVpnTargetNetworkResponse,

    -- * Response Lenses
    associateClientVpnTargetNetworkResponse_status,
    associateClientVpnTargetNetworkResponse_associationId,
    associateClientVpnTargetNetworkResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateClientVpnTargetNetwork' smart constructor.
data AssociateClientVpnTargetNetwork = AssociateClientVpnTargetNetwork'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text,
    -- | The ID of the subnet to associate with the Client VPN endpoint.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateClientVpnTargetNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateClientVpnTargetNetwork_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'associateClientVpnTargetNetwork_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'clientVpnEndpointId', 'associateClientVpnTargetNetwork_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'subnetId', 'associateClientVpnTargetNetwork_subnetId' - The ID of the subnet to associate with the Client VPN endpoint.
newAssociateClientVpnTargetNetwork ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  AssociateClientVpnTargetNetwork
newAssociateClientVpnTargetNetwork
  pClientVpnEndpointId_
  pSubnetId_ =
    AssociateClientVpnTargetNetwork'
      { dryRun =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_,
        subnetId = pSubnetId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateClientVpnTargetNetwork_dryRun :: Lens.Lens' AssociateClientVpnTargetNetwork (Prelude.Maybe Prelude.Bool)
associateClientVpnTargetNetwork_dryRun = Lens.lens (\AssociateClientVpnTargetNetwork' {dryRun} -> dryRun) (\s@AssociateClientVpnTargetNetwork' {} a -> s {dryRun = a} :: AssociateClientVpnTargetNetwork)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
associateClientVpnTargetNetwork_clientToken :: Lens.Lens' AssociateClientVpnTargetNetwork (Prelude.Maybe Prelude.Text)
associateClientVpnTargetNetwork_clientToken = Lens.lens (\AssociateClientVpnTargetNetwork' {clientToken} -> clientToken) (\s@AssociateClientVpnTargetNetwork' {} a -> s {clientToken = a} :: AssociateClientVpnTargetNetwork)

-- | The ID of the Client VPN endpoint.
associateClientVpnTargetNetwork_clientVpnEndpointId :: Lens.Lens' AssociateClientVpnTargetNetwork Prelude.Text
associateClientVpnTargetNetwork_clientVpnEndpointId = Lens.lens (\AssociateClientVpnTargetNetwork' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@AssociateClientVpnTargetNetwork' {} a -> s {clientVpnEndpointId = a} :: AssociateClientVpnTargetNetwork)

-- | The ID of the subnet to associate with the Client VPN endpoint.
associateClientVpnTargetNetwork_subnetId :: Lens.Lens' AssociateClientVpnTargetNetwork Prelude.Text
associateClientVpnTargetNetwork_subnetId = Lens.lens (\AssociateClientVpnTargetNetwork' {subnetId} -> subnetId) (\s@AssociateClientVpnTargetNetwork' {} a -> s {subnetId = a} :: AssociateClientVpnTargetNetwork)

instance
  Prelude.AWSRequest
    AssociateClientVpnTargetNetwork
  where
  type
    Rs AssociateClientVpnTargetNetwork =
      AssociateClientVpnTargetNetworkResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateClientVpnTargetNetworkResponse'
            Prelude.<$> (x Prelude..@? "status")
            Prelude.<*> (x Prelude..@? "associationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateClientVpnTargetNetwork

instance
  Prelude.NFData
    AssociateClientVpnTargetNetwork

instance
  Prelude.ToHeaders
    AssociateClientVpnTargetNetwork
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    AssociateClientVpnTargetNetwork
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AssociateClientVpnTargetNetwork
  where
  toQuery AssociateClientVpnTargetNetwork' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AssociateClientVpnTargetNetwork" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "ClientToken" Prelude.=: clientToken,
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId,
        "SubnetId" Prelude.=: subnetId
      ]

-- | /See:/ 'newAssociateClientVpnTargetNetworkResponse' smart constructor.
data AssociateClientVpnTargetNetworkResponse = AssociateClientVpnTargetNetworkResponse'
  { -- | The current state of the target network association.
    status :: Prelude.Maybe AssociationStatus,
    -- | The unique ID of the target network association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateClientVpnTargetNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'associateClientVpnTargetNetworkResponse_status' - The current state of the target network association.
--
-- 'associationId', 'associateClientVpnTargetNetworkResponse_associationId' - The unique ID of the target network association.
--
-- 'httpStatus', 'associateClientVpnTargetNetworkResponse_httpStatus' - The response's http status code.
newAssociateClientVpnTargetNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateClientVpnTargetNetworkResponse
newAssociateClientVpnTargetNetworkResponse
  pHttpStatus_ =
    AssociateClientVpnTargetNetworkResponse'
      { status =
          Prelude.Nothing,
        associationId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current state of the target network association.
associateClientVpnTargetNetworkResponse_status :: Lens.Lens' AssociateClientVpnTargetNetworkResponse (Prelude.Maybe AssociationStatus)
associateClientVpnTargetNetworkResponse_status = Lens.lens (\AssociateClientVpnTargetNetworkResponse' {status} -> status) (\s@AssociateClientVpnTargetNetworkResponse' {} a -> s {status = a} :: AssociateClientVpnTargetNetworkResponse)

-- | The unique ID of the target network association.
associateClientVpnTargetNetworkResponse_associationId :: Lens.Lens' AssociateClientVpnTargetNetworkResponse (Prelude.Maybe Prelude.Text)
associateClientVpnTargetNetworkResponse_associationId = Lens.lens (\AssociateClientVpnTargetNetworkResponse' {associationId} -> associationId) (\s@AssociateClientVpnTargetNetworkResponse' {} a -> s {associationId = a} :: AssociateClientVpnTargetNetworkResponse)

-- | The response's http status code.
associateClientVpnTargetNetworkResponse_httpStatus :: Lens.Lens' AssociateClientVpnTargetNetworkResponse Prelude.Int
associateClientVpnTargetNetworkResponse_httpStatus = Lens.lens (\AssociateClientVpnTargetNetworkResponse' {httpStatus} -> httpStatus) (\s@AssociateClientVpnTargetNetworkResponse' {} a -> s {httpStatus = a} :: AssociateClientVpnTargetNetworkResponse)

instance
  Prelude.NFData
    AssociateClientVpnTargetNetworkResponse
