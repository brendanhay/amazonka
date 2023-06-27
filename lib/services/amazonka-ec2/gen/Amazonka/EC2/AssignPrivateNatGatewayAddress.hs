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
-- Module      : Amazonka.EC2.AssignPrivateNatGatewayAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more private IPv4 addresses to a private NAT gateway. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html#nat-gateway-working-with Work with NAT gateways>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssignPrivateNatGatewayAddress
  ( -- * Creating a Request
    AssignPrivateNatGatewayAddress (..),
    newAssignPrivateNatGatewayAddress,

    -- * Request Lenses
    assignPrivateNatGatewayAddress_dryRun,
    assignPrivateNatGatewayAddress_privateIpAddressCount,
    assignPrivateNatGatewayAddress_privateIpAddresses,
    assignPrivateNatGatewayAddress_natGatewayId,

    -- * Destructuring the Response
    AssignPrivateNatGatewayAddressResponse (..),
    newAssignPrivateNatGatewayAddressResponse,

    -- * Response Lenses
    assignPrivateNatGatewayAddressResponse_natGatewayAddresses,
    assignPrivateNatGatewayAddressResponse_natGatewayId,
    assignPrivateNatGatewayAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssignPrivateNatGatewayAddress' smart constructor.
data AssignPrivateNatGatewayAddress = AssignPrivateNatGatewayAddress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The number of private IP addresses to assign to the NAT gateway. You
    -- can\'t specify this parameter when also specifying private IP addresses.
    privateIpAddressCount :: Prelude.Maybe Prelude.Natural,
    -- | The private IPv4 addresses you want to assign to the private NAT
    -- gateway.
    privateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateNatGatewayAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'assignPrivateNatGatewayAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'privateIpAddressCount', 'assignPrivateNatGatewayAddress_privateIpAddressCount' - The number of private IP addresses to assign to the NAT gateway. You
-- can\'t specify this parameter when also specifying private IP addresses.
--
-- 'privateIpAddresses', 'assignPrivateNatGatewayAddress_privateIpAddresses' - The private IPv4 addresses you want to assign to the private NAT
-- gateway.
--
-- 'natGatewayId', 'assignPrivateNatGatewayAddress_natGatewayId' - The NAT gateway ID.
newAssignPrivateNatGatewayAddress ::
  -- | 'natGatewayId'
  Prelude.Text ->
  AssignPrivateNatGatewayAddress
newAssignPrivateNatGatewayAddress pNatGatewayId_ =
  AssignPrivateNatGatewayAddress'
    { dryRun =
        Prelude.Nothing,
      privateIpAddressCount = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      natGatewayId = pNatGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
assignPrivateNatGatewayAddress_dryRun :: Lens.Lens' AssignPrivateNatGatewayAddress (Prelude.Maybe Prelude.Bool)
assignPrivateNatGatewayAddress_dryRun = Lens.lens (\AssignPrivateNatGatewayAddress' {dryRun} -> dryRun) (\s@AssignPrivateNatGatewayAddress' {} a -> s {dryRun = a} :: AssignPrivateNatGatewayAddress)

-- | The number of private IP addresses to assign to the NAT gateway. You
-- can\'t specify this parameter when also specifying private IP addresses.
assignPrivateNatGatewayAddress_privateIpAddressCount :: Lens.Lens' AssignPrivateNatGatewayAddress (Prelude.Maybe Prelude.Natural)
assignPrivateNatGatewayAddress_privateIpAddressCount = Lens.lens (\AssignPrivateNatGatewayAddress' {privateIpAddressCount} -> privateIpAddressCount) (\s@AssignPrivateNatGatewayAddress' {} a -> s {privateIpAddressCount = a} :: AssignPrivateNatGatewayAddress)

-- | The private IPv4 addresses you want to assign to the private NAT
-- gateway.
assignPrivateNatGatewayAddress_privateIpAddresses :: Lens.Lens' AssignPrivateNatGatewayAddress (Prelude.Maybe [Prelude.Text])
assignPrivateNatGatewayAddress_privateIpAddresses = Lens.lens (\AssignPrivateNatGatewayAddress' {privateIpAddresses} -> privateIpAddresses) (\s@AssignPrivateNatGatewayAddress' {} a -> s {privateIpAddresses = a} :: AssignPrivateNatGatewayAddress) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
assignPrivateNatGatewayAddress_natGatewayId :: Lens.Lens' AssignPrivateNatGatewayAddress Prelude.Text
assignPrivateNatGatewayAddress_natGatewayId = Lens.lens (\AssignPrivateNatGatewayAddress' {natGatewayId} -> natGatewayId) (\s@AssignPrivateNatGatewayAddress' {} a -> s {natGatewayId = a} :: AssignPrivateNatGatewayAddress)

instance
  Core.AWSRequest
    AssignPrivateNatGatewayAddress
  where
  type
    AWSResponse AssignPrivateNatGatewayAddress =
      AssignPrivateNatGatewayAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssignPrivateNatGatewayAddressResponse'
            Prelude.<$> ( x
                            Data..@? "natGatewayAddressSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "natGatewayId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssignPrivateNatGatewayAddress
  where
  hashWithSalt
    _salt
    AssignPrivateNatGatewayAddress' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` privateIpAddressCount
        `Prelude.hashWithSalt` privateIpAddresses
        `Prelude.hashWithSalt` natGatewayId

instance
  Prelude.NFData
    AssignPrivateNatGatewayAddress
  where
  rnf AssignPrivateNatGatewayAddress' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf privateIpAddressCount
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf natGatewayId

instance
  Data.ToHeaders
    AssignPrivateNatGatewayAddress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssignPrivateNatGatewayAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery AssignPrivateNatGatewayAddress where
  toQuery AssignPrivateNatGatewayAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssignPrivateNatGatewayAddress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PrivateIpAddressCount"
          Data.=: privateIpAddressCount,
        Data.toQuery
          ( Data.toQueryList "PrivateIpAddress"
              Prelude.<$> privateIpAddresses
          ),
        "NatGatewayId" Data.=: natGatewayId
      ]

-- | /See:/ 'newAssignPrivateNatGatewayAddressResponse' smart constructor.
data AssignPrivateNatGatewayAddressResponse = AssignPrivateNatGatewayAddressResponse'
  { -- | NAT gateway IP addresses.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateNatGatewayAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayAddresses', 'assignPrivateNatGatewayAddressResponse_natGatewayAddresses' - NAT gateway IP addresses.
--
-- 'natGatewayId', 'assignPrivateNatGatewayAddressResponse_natGatewayId' - The NAT gateway ID.
--
-- 'httpStatus', 'assignPrivateNatGatewayAddressResponse_httpStatus' - The response's http status code.
newAssignPrivateNatGatewayAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssignPrivateNatGatewayAddressResponse
newAssignPrivateNatGatewayAddressResponse
  pHttpStatus_ =
    AssignPrivateNatGatewayAddressResponse'
      { natGatewayAddresses =
          Prelude.Nothing,
        natGatewayId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | NAT gateway IP addresses.
assignPrivateNatGatewayAddressResponse_natGatewayAddresses :: Lens.Lens' AssignPrivateNatGatewayAddressResponse (Prelude.Maybe [NatGatewayAddress])
assignPrivateNatGatewayAddressResponse_natGatewayAddresses = Lens.lens (\AssignPrivateNatGatewayAddressResponse' {natGatewayAddresses} -> natGatewayAddresses) (\s@AssignPrivateNatGatewayAddressResponse' {} a -> s {natGatewayAddresses = a} :: AssignPrivateNatGatewayAddressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
assignPrivateNatGatewayAddressResponse_natGatewayId :: Lens.Lens' AssignPrivateNatGatewayAddressResponse (Prelude.Maybe Prelude.Text)
assignPrivateNatGatewayAddressResponse_natGatewayId = Lens.lens (\AssignPrivateNatGatewayAddressResponse' {natGatewayId} -> natGatewayId) (\s@AssignPrivateNatGatewayAddressResponse' {} a -> s {natGatewayId = a} :: AssignPrivateNatGatewayAddressResponse)

-- | The response's http status code.
assignPrivateNatGatewayAddressResponse_httpStatus :: Lens.Lens' AssignPrivateNatGatewayAddressResponse Prelude.Int
assignPrivateNatGatewayAddressResponse_httpStatus = Lens.lens (\AssignPrivateNatGatewayAddressResponse' {httpStatus} -> httpStatus) (\s@AssignPrivateNatGatewayAddressResponse' {} a -> s {httpStatus = a} :: AssignPrivateNatGatewayAddressResponse)

instance
  Prelude.NFData
    AssignPrivateNatGatewayAddressResponse
  where
  rnf AssignPrivateNatGatewayAddressResponse' {..} =
    Prelude.rnf natGatewayAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
