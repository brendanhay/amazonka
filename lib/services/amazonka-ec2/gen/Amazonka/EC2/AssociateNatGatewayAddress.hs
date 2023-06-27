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
-- Module      : Amazonka.EC2.AssociateNatGatewayAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates Elastic IP addresses (EIPs) and private IPv4 addresses with a
-- public NAT gateway. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html#nat-gateway-working-with Work with NAT gateways>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- By default, you can associate up to 2 Elastic IP addresses per public
-- NAT gateway. You can increase the limit by requesting a quota
-- adjustment. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-eips Elastic IP address quotas>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssociateNatGatewayAddress
  ( -- * Creating a Request
    AssociateNatGatewayAddress (..),
    newAssociateNatGatewayAddress,

    -- * Request Lenses
    associateNatGatewayAddress_dryRun,
    associateNatGatewayAddress_privateIpAddresses,
    associateNatGatewayAddress_natGatewayId,
    associateNatGatewayAddress_allocationIds,

    -- * Destructuring the Response
    AssociateNatGatewayAddressResponse (..),
    newAssociateNatGatewayAddressResponse,

    -- * Response Lenses
    associateNatGatewayAddressResponse_natGatewayAddresses,
    associateNatGatewayAddressResponse_natGatewayId,
    associateNatGatewayAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateNatGatewayAddress' smart constructor.
data AssociateNatGatewayAddress = AssociateNatGatewayAddress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 addresses that you want to assign to the NAT gateway.
    privateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Text,
    -- | The allocation IDs of EIPs that you want to associate with your NAT
    -- gateway.
    allocationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateNatGatewayAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateNatGatewayAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'privateIpAddresses', 'associateNatGatewayAddress_privateIpAddresses' - The private IPv4 addresses that you want to assign to the NAT gateway.
--
-- 'natGatewayId', 'associateNatGatewayAddress_natGatewayId' - The NAT gateway ID.
--
-- 'allocationIds', 'associateNatGatewayAddress_allocationIds' - The allocation IDs of EIPs that you want to associate with your NAT
-- gateway.
newAssociateNatGatewayAddress ::
  -- | 'natGatewayId'
  Prelude.Text ->
  AssociateNatGatewayAddress
newAssociateNatGatewayAddress pNatGatewayId_ =
  AssociateNatGatewayAddress'
    { dryRun =
        Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      natGatewayId = pNatGatewayId_,
      allocationIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateNatGatewayAddress_dryRun :: Lens.Lens' AssociateNatGatewayAddress (Prelude.Maybe Prelude.Bool)
associateNatGatewayAddress_dryRun = Lens.lens (\AssociateNatGatewayAddress' {dryRun} -> dryRun) (\s@AssociateNatGatewayAddress' {} a -> s {dryRun = a} :: AssociateNatGatewayAddress)

-- | The private IPv4 addresses that you want to assign to the NAT gateway.
associateNatGatewayAddress_privateIpAddresses :: Lens.Lens' AssociateNatGatewayAddress (Prelude.Maybe [Prelude.Text])
associateNatGatewayAddress_privateIpAddresses = Lens.lens (\AssociateNatGatewayAddress' {privateIpAddresses} -> privateIpAddresses) (\s@AssociateNatGatewayAddress' {} a -> s {privateIpAddresses = a} :: AssociateNatGatewayAddress) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
associateNatGatewayAddress_natGatewayId :: Lens.Lens' AssociateNatGatewayAddress Prelude.Text
associateNatGatewayAddress_natGatewayId = Lens.lens (\AssociateNatGatewayAddress' {natGatewayId} -> natGatewayId) (\s@AssociateNatGatewayAddress' {} a -> s {natGatewayId = a} :: AssociateNatGatewayAddress)

-- | The allocation IDs of EIPs that you want to associate with your NAT
-- gateway.
associateNatGatewayAddress_allocationIds :: Lens.Lens' AssociateNatGatewayAddress [Prelude.Text]
associateNatGatewayAddress_allocationIds = Lens.lens (\AssociateNatGatewayAddress' {allocationIds} -> allocationIds) (\s@AssociateNatGatewayAddress' {} a -> s {allocationIds = a} :: AssociateNatGatewayAddress) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateNatGatewayAddress where
  type
    AWSResponse AssociateNatGatewayAddress =
      AssociateNatGatewayAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateNatGatewayAddressResponse'
            Prelude.<$> ( x
                            Data..@? "natGatewayAddressSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "natGatewayId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateNatGatewayAddress where
  hashWithSalt _salt AssociateNatGatewayAddress' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` natGatewayId
      `Prelude.hashWithSalt` allocationIds

instance Prelude.NFData AssociateNatGatewayAddress where
  rnf AssociateNatGatewayAddress' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf allocationIds

instance Data.ToHeaders AssociateNatGatewayAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateNatGatewayAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateNatGatewayAddress where
  toQuery AssociateNatGatewayAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateNatGatewayAddress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "PrivateIpAddress"
              Prelude.<$> privateIpAddresses
          ),
        "NatGatewayId" Data.=: natGatewayId,
        Data.toQueryList "AllocationId" allocationIds
      ]

-- | /See:/ 'newAssociateNatGatewayAddressResponse' smart constructor.
data AssociateNatGatewayAddressResponse = AssociateNatGatewayAddressResponse'
  { -- | The IP addresses.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateNatGatewayAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayAddresses', 'associateNatGatewayAddressResponse_natGatewayAddresses' - The IP addresses.
--
-- 'natGatewayId', 'associateNatGatewayAddressResponse_natGatewayId' - The NAT gateway ID.
--
-- 'httpStatus', 'associateNatGatewayAddressResponse_httpStatus' - The response's http status code.
newAssociateNatGatewayAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateNatGatewayAddressResponse
newAssociateNatGatewayAddressResponse pHttpStatus_ =
  AssociateNatGatewayAddressResponse'
    { natGatewayAddresses =
        Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP addresses.
associateNatGatewayAddressResponse_natGatewayAddresses :: Lens.Lens' AssociateNatGatewayAddressResponse (Prelude.Maybe [NatGatewayAddress])
associateNatGatewayAddressResponse_natGatewayAddresses = Lens.lens (\AssociateNatGatewayAddressResponse' {natGatewayAddresses} -> natGatewayAddresses) (\s@AssociateNatGatewayAddressResponse' {} a -> s {natGatewayAddresses = a} :: AssociateNatGatewayAddressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
associateNatGatewayAddressResponse_natGatewayId :: Lens.Lens' AssociateNatGatewayAddressResponse (Prelude.Maybe Prelude.Text)
associateNatGatewayAddressResponse_natGatewayId = Lens.lens (\AssociateNatGatewayAddressResponse' {natGatewayId} -> natGatewayId) (\s@AssociateNatGatewayAddressResponse' {} a -> s {natGatewayId = a} :: AssociateNatGatewayAddressResponse)

-- | The response's http status code.
associateNatGatewayAddressResponse_httpStatus :: Lens.Lens' AssociateNatGatewayAddressResponse Prelude.Int
associateNatGatewayAddressResponse_httpStatus = Lens.lens (\AssociateNatGatewayAddressResponse' {httpStatus} -> httpStatus) (\s@AssociateNatGatewayAddressResponse' {} a -> s {httpStatus = a} :: AssociateNatGatewayAddressResponse)

instance
  Prelude.NFData
    AssociateNatGatewayAddressResponse
  where
  rnf AssociateNatGatewayAddressResponse' {..} =
    Prelude.rnf natGatewayAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
