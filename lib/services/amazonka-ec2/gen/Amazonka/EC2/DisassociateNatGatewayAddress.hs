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
-- Module      : Amazonka.EC2.DisassociateNatGatewayAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates secondary Elastic IP addresses (EIPs) from a public NAT
-- gateway. You cannot disassociate your primary EIP. For more information,
-- see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html#nat-gateway-edit-secondary Edit secondary IP address associations>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- While disassociating is in progress, you cannot associate\/disassociate
-- additional EIPs while the connections are being drained. You are,
-- however, allowed to delete the NAT gateway.
--
-- An EIP will only be released at the end of MaxDrainDurationSeconds. The
-- EIPs stay associated and support the existing connections but do not
-- support any new connections (new connections are distributed across the
-- remaining associated EIPs). As the existing connections drain out, the
-- EIPs (and the corresponding private IPs mapped to them) get released.
module Amazonka.EC2.DisassociateNatGatewayAddress
  ( -- * Creating a Request
    DisassociateNatGatewayAddress (..),
    newDisassociateNatGatewayAddress,

    -- * Request Lenses
    disassociateNatGatewayAddress_dryRun,
    disassociateNatGatewayAddress_maxDrainDurationSeconds,
    disassociateNatGatewayAddress_natGatewayId,
    disassociateNatGatewayAddress_associationIds,

    -- * Destructuring the Response
    DisassociateNatGatewayAddressResponse (..),
    newDisassociateNatGatewayAddressResponse,

    -- * Response Lenses
    disassociateNatGatewayAddressResponse_natGatewayAddresses,
    disassociateNatGatewayAddressResponse_natGatewayId,
    disassociateNatGatewayAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateNatGatewayAddress' smart constructor.
data DisassociateNatGatewayAddress = DisassociateNatGatewayAddress'
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
    -- | The association IDs of EIPs that have been associated with the NAT
    -- gateway.
    associationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNatGatewayAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateNatGatewayAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxDrainDurationSeconds', 'disassociateNatGatewayAddress_maxDrainDurationSeconds' - The maximum amount of time to wait (in seconds) before forcibly
-- releasing the IP addresses if connections are still in progress. Default
-- value is 350 seconds.
--
-- 'natGatewayId', 'disassociateNatGatewayAddress_natGatewayId' - The NAT gateway ID.
--
-- 'associationIds', 'disassociateNatGatewayAddress_associationIds' - The association IDs of EIPs that have been associated with the NAT
-- gateway.
newDisassociateNatGatewayAddress ::
  -- | 'natGatewayId'
  Prelude.Text ->
  DisassociateNatGatewayAddress
newDisassociateNatGatewayAddress pNatGatewayId_ =
  DisassociateNatGatewayAddress'
    { dryRun =
        Prelude.Nothing,
      maxDrainDurationSeconds = Prelude.Nothing,
      natGatewayId = pNatGatewayId_,
      associationIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateNatGatewayAddress_dryRun :: Lens.Lens' DisassociateNatGatewayAddress (Prelude.Maybe Prelude.Bool)
disassociateNatGatewayAddress_dryRun = Lens.lens (\DisassociateNatGatewayAddress' {dryRun} -> dryRun) (\s@DisassociateNatGatewayAddress' {} a -> s {dryRun = a} :: DisassociateNatGatewayAddress)

-- | The maximum amount of time to wait (in seconds) before forcibly
-- releasing the IP addresses if connections are still in progress. Default
-- value is 350 seconds.
disassociateNatGatewayAddress_maxDrainDurationSeconds :: Lens.Lens' DisassociateNatGatewayAddress (Prelude.Maybe Prelude.Natural)
disassociateNatGatewayAddress_maxDrainDurationSeconds = Lens.lens (\DisassociateNatGatewayAddress' {maxDrainDurationSeconds} -> maxDrainDurationSeconds) (\s@DisassociateNatGatewayAddress' {} a -> s {maxDrainDurationSeconds = a} :: DisassociateNatGatewayAddress)

-- | The NAT gateway ID.
disassociateNatGatewayAddress_natGatewayId :: Lens.Lens' DisassociateNatGatewayAddress Prelude.Text
disassociateNatGatewayAddress_natGatewayId = Lens.lens (\DisassociateNatGatewayAddress' {natGatewayId} -> natGatewayId) (\s@DisassociateNatGatewayAddress' {} a -> s {natGatewayId = a} :: DisassociateNatGatewayAddress)

-- | The association IDs of EIPs that have been associated with the NAT
-- gateway.
disassociateNatGatewayAddress_associationIds :: Lens.Lens' DisassociateNatGatewayAddress [Prelude.Text]
disassociateNatGatewayAddress_associationIds = Lens.lens (\DisassociateNatGatewayAddress' {associationIds} -> associationIds) (\s@DisassociateNatGatewayAddress' {} a -> s {associationIds = a} :: DisassociateNatGatewayAddress) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateNatGatewayAddress
  where
  type
    AWSResponse DisassociateNatGatewayAddress =
      DisassociateNatGatewayAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateNatGatewayAddressResponse'
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
    DisassociateNatGatewayAddress
  where
  hashWithSalt _salt DisassociateNatGatewayAddress' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxDrainDurationSeconds
      `Prelude.hashWithSalt` natGatewayId
      `Prelude.hashWithSalt` associationIds

instance Prelude.NFData DisassociateNatGatewayAddress where
  rnf DisassociateNatGatewayAddress' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxDrainDurationSeconds
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf associationIds

instance Data.ToHeaders DisassociateNatGatewayAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisassociateNatGatewayAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateNatGatewayAddress where
  toQuery DisassociateNatGatewayAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateNatGatewayAddress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxDrainDurationSeconds"
          Data.=: maxDrainDurationSeconds,
        "NatGatewayId" Data.=: natGatewayId,
        Data.toQueryList "AssociationId" associationIds
      ]

-- | /See:/ 'newDisassociateNatGatewayAddressResponse' smart constructor.
data DisassociateNatGatewayAddressResponse = DisassociateNatGatewayAddressResponse'
  { -- | Information about the NAT gateway IP addresses.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The NAT gateway ID.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNatGatewayAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayAddresses', 'disassociateNatGatewayAddressResponse_natGatewayAddresses' - Information about the NAT gateway IP addresses.
--
-- 'natGatewayId', 'disassociateNatGatewayAddressResponse_natGatewayId' - The NAT gateway ID.
--
-- 'httpStatus', 'disassociateNatGatewayAddressResponse_httpStatus' - The response's http status code.
newDisassociateNatGatewayAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateNatGatewayAddressResponse
newDisassociateNatGatewayAddressResponse pHttpStatus_ =
  DisassociateNatGatewayAddressResponse'
    { natGatewayAddresses =
        Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the NAT gateway IP addresses.
disassociateNatGatewayAddressResponse_natGatewayAddresses :: Lens.Lens' DisassociateNatGatewayAddressResponse (Prelude.Maybe [NatGatewayAddress])
disassociateNatGatewayAddressResponse_natGatewayAddresses = Lens.lens (\DisassociateNatGatewayAddressResponse' {natGatewayAddresses} -> natGatewayAddresses) (\s@DisassociateNatGatewayAddressResponse' {} a -> s {natGatewayAddresses = a} :: DisassociateNatGatewayAddressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The NAT gateway ID.
disassociateNatGatewayAddressResponse_natGatewayId :: Lens.Lens' DisassociateNatGatewayAddressResponse (Prelude.Maybe Prelude.Text)
disassociateNatGatewayAddressResponse_natGatewayId = Lens.lens (\DisassociateNatGatewayAddressResponse' {natGatewayId} -> natGatewayId) (\s@DisassociateNatGatewayAddressResponse' {} a -> s {natGatewayId = a} :: DisassociateNatGatewayAddressResponse)

-- | The response's http status code.
disassociateNatGatewayAddressResponse_httpStatus :: Lens.Lens' DisassociateNatGatewayAddressResponse Prelude.Int
disassociateNatGatewayAddressResponse_httpStatus = Lens.lens (\DisassociateNatGatewayAddressResponse' {httpStatus} -> httpStatus) (\s@DisassociateNatGatewayAddressResponse' {} a -> s {httpStatus = a} :: DisassociateNatGatewayAddressResponse)

instance
  Prelude.NFData
    DisassociateNatGatewayAddressResponse
  where
  rnf DisassociateNatGatewayAddressResponse' {..} =
    Prelude.rnf natGatewayAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
