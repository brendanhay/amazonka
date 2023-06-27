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
-- Module      : Amazonka.EC2.UnassignPrivateNatGatewayAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns secondary private IPv4 addresses from a private NAT gateway.
-- You cannot unassign your primary private IP. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html#nat-gateway-edit-secondary Edit secondary IP address associations>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- While unassigning is in progress, you cannot assign\/unassign additional
-- IP addresses while the connections are being drained. You are, however,
-- allowed to delete the NAT gateway.
--
-- A private IP address will only be released at the end of
-- MaxDrainDurationSeconds. The private IP addresses stay associated and
-- support the existing connections but do not support any new connections
-- (new connections are distributed across the remaining assigned private
-- IP address). After the existing connections drain out, the private IP
-- addresses get released.
module Amazonka.EC2.UnassignPrivateNatGatewayAddress
  ( -- * Creating a Request
    UnassignPrivateNatGatewayAddress (..),
    newUnassignPrivateNatGatewayAddress,

    -- * Request Lenses
    unassignPrivateNatGatewayAddress_dryRun,
    unassignPrivateNatGatewayAddress_maxDrainDurationSeconds,
    unassignPrivateNatGatewayAddress_natGatewayId,
    unassignPrivateNatGatewayAddress_privateIpAddresses,

    -- * Destructuring the Response
    UnassignPrivateNatGatewayAddressResponse (..),
    newUnassignPrivateNatGatewayAddressResponse,

    -- * Response Lenses
    unassignPrivateNatGatewayAddressResponse_natGatewayAddresses,
    unassignPrivateNatGatewayAddressResponse_natGatewayId,
    unassignPrivateNatGatewayAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnassignPrivateNatGatewayAddress' smart constructor.
data UnassignPrivateNatGatewayAddress = UnassignPrivateNatGatewayAddress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum amount of time to wait (in seconds) before forcibly
    -- releasing the IP addresses if connections are still in progress. Default
    -- value is 350 seconds.
    maxDrainDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Text,
    -- | The private IPv4 addresses you want to unassign.
    privateIpAddresses :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateNatGatewayAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'unassignPrivateNatGatewayAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxDrainDurationSeconds', 'unassignPrivateNatGatewayAddress_maxDrainDurationSeconds' - The maximum amount of time to wait (in seconds) before forcibly
-- releasing the IP addresses if connections are still in progress. Default
-- value is 350 seconds.
--
-- 'natGatewayId', 'unassignPrivateNatGatewayAddress_natGatewayId' - The NAT gateway ID.
--
-- 'privateIpAddresses', 'unassignPrivateNatGatewayAddress_privateIpAddresses' - The private IPv4 addresses you want to unassign.
newUnassignPrivateNatGatewayAddress ::
  -- | 'natGatewayId'
  Prelude.Text ->
  UnassignPrivateNatGatewayAddress
newUnassignPrivateNatGatewayAddress pNatGatewayId_ =
  UnassignPrivateNatGatewayAddress'
    { dryRun =
        Prelude.Nothing,
      maxDrainDurationSeconds = Prelude.Nothing,
      natGatewayId = pNatGatewayId_,
      privateIpAddresses = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
unassignPrivateNatGatewayAddress_dryRun :: Lens.Lens' UnassignPrivateNatGatewayAddress (Prelude.Maybe Prelude.Bool)
unassignPrivateNatGatewayAddress_dryRun = Lens.lens (\UnassignPrivateNatGatewayAddress' {dryRun} -> dryRun) (\s@UnassignPrivateNatGatewayAddress' {} a -> s {dryRun = a} :: UnassignPrivateNatGatewayAddress)

-- | The maximum amount of time to wait (in seconds) before forcibly
-- releasing the IP addresses if connections are still in progress. Default
-- value is 350 seconds.
unassignPrivateNatGatewayAddress_maxDrainDurationSeconds :: Lens.Lens' UnassignPrivateNatGatewayAddress (Prelude.Maybe Prelude.Natural)
unassignPrivateNatGatewayAddress_maxDrainDurationSeconds = Lens.lens (\UnassignPrivateNatGatewayAddress' {maxDrainDurationSeconds} -> maxDrainDurationSeconds) (\s@UnassignPrivateNatGatewayAddress' {} a -> s {maxDrainDurationSeconds = a} :: UnassignPrivateNatGatewayAddress)

-- | The NAT gateway ID.
unassignPrivateNatGatewayAddress_natGatewayId :: Lens.Lens' UnassignPrivateNatGatewayAddress Prelude.Text
unassignPrivateNatGatewayAddress_natGatewayId = Lens.lens (\UnassignPrivateNatGatewayAddress' {natGatewayId} -> natGatewayId) (\s@UnassignPrivateNatGatewayAddress' {} a -> s {natGatewayId = a} :: UnassignPrivateNatGatewayAddress)

-- | The private IPv4 addresses you want to unassign.
unassignPrivateNatGatewayAddress_privateIpAddresses :: Lens.Lens' UnassignPrivateNatGatewayAddress [Prelude.Text]
unassignPrivateNatGatewayAddress_privateIpAddresses = Lens.lens (\UnassignPrivateNatGatewayAddress' {privateIpAddresses} -> privateIpAddresses) (\s@UnassignPrivateNatGatewayAddress' {} a -> s {privateIpAddresses = a} :: UnassignPrivateNatGatewayAddress) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UnassignPrivateNatGatewayAddress
  where
  type
    AWSResponse UnassignPrivateNatGatewayAddress =
      UnassignPrivateNatGatewayAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UnassignPrivateNatGatewayAddressResponse'
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
    UnassignPrivateNatGatewayAddress
  where
  hashWithSalt
    _salt
    UnassignPrivateNatGatewayAddress' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxDrainDurationSeconds
        `Prelude.hashWithSalt` natGatewayId
        `Prelude.hashWithSalt` privateIpAddresses

instance
  Prelude.NFData
    UnassignPrivateNatGatewayAddress
  where
  rnf UnassignPrivateNatGatewayAddress' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxDrainDurationSeconds
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf privateIpAddresses

instance
  Data.ToHeaders
    UnassignPrivateNatGatewayAddress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UnassignPrivateNatGatewayAddress where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UnassignPrivateNatGatewayAddress
  where
  toQuery UnassignPrivateNatGatewayAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UnassignPrivateNatGatewayAddress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxDrainDurationSeconds"
          Data.=: maxDrainDurationSeconds,
        "NatGatewayId" Data.=: natGatewayId,
        Data.toQueryList
          "PrivateIpAddress"
          privateIpAddresses
      ]

-- | /See:/ 'newUnassignPrivateNatGatewayAddressResponse' smart constructor.
data UnassignPrivateNatGatewayAddressResponse = UnassignPrivateNatGatewayAddressResponse'
  { -- | Information about the NAT gateway IP addresses.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateNatGatewayAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayAddresses', 'unassignPrivateNatGatewayAddressResponse_natGatewayAddresses' - Information about the NAT gateway IP addresses.
--
-- 'natGatewayId', 'unassignPrivateNatGatewayAddressResponse_natGatewayId' - The NAT gateway ID.
--
-- 'httpStatus', 'unassignPrivateNatGatewayAddressResponse_httpStatus' - The response's http status code.
newUnassignPrivateNatGatewayAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnassignPrivateNatGatewayAddressResponse
newUnassignPrivateNatGatewayAddressResponse
  pHttpStatus_ =
    UnassignPrivateNatGatewayAddressResponse'
      { natGatewayAddresses =
          Prelude.Nothing,
        natGatewayId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the NAT gateway IP addresses.
unassignPrivateNatGatewayAddressResponse_natGatewayAddresses :: Lens.Lens' UnassignPrivateNatGatewayAddressResponse (Prelude.Maybe [NatGatewayAddress])
unassignPrivateNatGatewayAddressResponse_natGatewayAddresses = Lens.lens (\UnassignPrivateNatGatewayAddressResponse' {natGatewayAddresses} -> natGatewayAddresses) (\s@UnassignPrivateNatGatewayAddressResponse' {} a -> s {natGatewayAddresses = a} :: UnassignPrivateNatGatewayAddressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
unassignPrivateNatGatewayAddressResponse_natGatewayId :: Lens.Lens' UnassignPrivateNatGatewayAddressResponse (Prelude.Maybe Prelude.Text)
unassignPrivateNatGatewayAddressResponse_natGatewayId = Lens.lens (\UnassignPrivateNatGatewayAddressResponse' {natGatewayId} -> natGatewayId) (\s@UnassignPrivateNatGatewayAddressResponse' {} a -> s {natGatewayId = a} :: UnassignPrivateNatGatewayAddressResponse)

-- | The response's http status code.
unassignPrivateNatGatewayAddressResponse_httpStatus :: Lens.Lens' UnassignPrivateNatGatewayAddressResponse Prelude.Int
unassignPrivateNatGatewayAddressResponse_httpStatus = Lens.lens (\UnassignPrivateNatGatewayAddressResponse' {httpStatus} -> httpStatus) (\s@UnassignPrivateNatGatewayAddressResponse' {} a -> s {httpStatus = a} :: UnassignPrivateNatGatewayAddressResponse)

instance
  Prelude.NFData
    UnassignPrivateNatGatewayAddressResponse
  where
  rnf UnassignPrivateNatGatewayAddressResponse' {..} =
    Prelude.rnf natGatewayAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
