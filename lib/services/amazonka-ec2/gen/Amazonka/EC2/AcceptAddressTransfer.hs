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
-- Module      : Amazonka.EC2.AcceptAddressTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an Elastic IP address transfer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-eips.html#using-instance-addressing-eips-transfer-accept Accept a transferred Elastic IP address>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AcceptAddressTransfer
  ( -- * Creating a Request
    AcceptAddressTransfer (..),
    newAcceptAddressTransfer,

    -- * Request Lenses
    acceptAddressTransfer_dryRun,
    acceptAddressTransfer_tagSpecifications,
    acceptAddressTransfer_address,

    -- * Destructuring the Response
    AcceptAddressTransferResponse (..),
    newAcceptAddressTransferResponse,

    -- * Response Lenses
    acceptAddressTransferResponse_addressTransfer,
    acceptAddressTransferResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptAddressTransfer' smart constructor.
data AcceptAddressTransfer = AcceptAddressTransfer'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | @tag@:\<key> - The key\/value combination of a tag assigned to the
    -- resource. Use the tag key in the filter name and the tag value as the
    -- filter value. For example, to find all resources that have a tag with
    -- the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for the
    -- filter name and @TeamA@ for the filter value.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Elastic IP address you are accepting for transfer.
    address :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAddressTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptAddressTransfer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'acceptAddressTransfer_tagSpecifications' - @tag@:\<key> - The key\/value combination of a tag assigned to the
-- resource. Use the tag key in the filter name and the tag value as the
-- filter value. For example, to find all resources that have a tag with
-- the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for the
-- filter name and @TeamA@ for the filter value.
--
-- 'address', 'acceptAddressTransfer_address' - The Elastic IP address you are accepting for transfer.
newAcceptAddressTransfer ::
  -- | 'address'
  Prelude.Text ->
  AcceptAddressTransfer
newAcceptAddressTransfer pAddress_ =
  AcceptAddressTransfer'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      address = pAddress_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptAddressTransfer_dryRun :: Lens.Lens' AcceptAddressTransfer (Prelude.Maybe Prelude.Bool)
acceptAddressTransfer_dryRun = Lens.lens (\AcceptAddressTransfer' {dryRun} -> dryRun) (\s@AcceptAddressTransfer' {} a -> s {dryRun = a} :: AcceptAddressTransfer)

-- | @tag@:\<key> - The key\/value combination of a tag assigned to the
-- resource. Use the tag key in the filter name and the tag value as the
-- filter value. For example, to find all resources that have a tag with
-- the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for the
-- filter name and @TeamA@ for the filter value.
acceptAddressTransfer_tagSpecifications :: Lens.Lens' AcceptAddressTransfer (Prelude.Maybe [TagSpecification])
acceptAddressTransfer_tagSpecifications = Lens.lens (\AcceptAddressTransfer' {tagSpecifications} -> tagSpecifications) (\s@AcceptAddressTransfer' {} a -> s {tagSpecifications = a} :: AcceptAddressTransfer) Prelude.. Lens.mapping Lens.coerced

-- | The Elastic IP address you are accepting for transfer.
acceptAddressTransfer_address :: Lens.Lens' AcceptAddressTransfer Prelude.Text
acceptAddressTransfer_address = Lens.lens (\AcceptAddressTransfer' {address} -> address) (\s@AcceptAddressTransfer' {} a -> s {address = a} :: AcceptAddressTransfer)

instance Core.AWSRequest AcceptAddressTransfer where
  type
    AWSResponse AcceptAddressTransfer =
      AcceptAddressTransferResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptAddressTransferResponse'
            Prelude.<$> (x Data..@? "addressTransfer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptAddressTransfer where
  hashWithSalt _salt AcceptAddressTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` address

instance Prelude.NFData AcceptAddressTransfer where
  rnf AcceptAddressTransfer' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf address

instance Data.ToHeaders AcceptAddressTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AcceptAddressTransfer where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptAddressTransfer where
  toQuery AcceptAddressTransfer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AcceptAddressTransfer" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Address" Data.=: address
      ]

-- | /See:/ 'newAcceptAddressTransferResponse' smart constructor.
data AcceptAddressTransferResponse = AcceptAddressTransferResponse'
  { -- | An Elastic IP address transfer.
    addressTransfer :: Prelude.Maybe AddressTransfer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAddressTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressTransfer', 'acceptAddressTransferResponse_addressTransfer' - An Elastic IP address transfer.
--
-- 'httpStatus', 'acceptAddressTransferResponse_httpStatus' - The response's http status code.
newAcceptAddressTransferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptAddressTransferResponse
newAcceptAddressTransferResponse pHttpStatus_ =
  AcceptAddressTransferResponse'
    { addressTransfer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Elastic IP address transfer.
acceptAddressTransferResponse_addressTransfer :: Lens.Lens' AcceptAddressTransferResponse (Prelude.Maybe AddressTransfer)
acceptAddressTransferResponse_addressTransfer = Lens.lens (\AcceptAddressTransferResponse' {addressTransfer} -> addressTransfer) (\s@AcceptAddressTransferResponse' {} a -> s {addressTransfer = a} :: AcceptAddressTransferResponse)

-- | The response's http status code.
acceptAddressTransferResponse_httpStatus :: Lens.Lens' AcceptAddressTransferResponse Prelude.Int
acceptAddressTransferResponse_httpStatus = Lens.lens (\AcceptAddressTransferResponse' {httpStatus} -> httpStatus) (\s@AcceptAddressTransferResponse' {} a -> s {httpStatus = a} :: AcceptAddressTransferResponse)

instance Prelude.NFData AcceptAddressTransferResponse where
  rnf AcceptAddressTransferResponse' {..} =
    Prelude.rnf addressTransfer
      `Prelude.seq` Prelude.rnf httpStatus
