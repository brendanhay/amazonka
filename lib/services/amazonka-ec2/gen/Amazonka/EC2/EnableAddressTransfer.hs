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
-- Module      : Amazonka.EC2.EnableAddressTransfer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Elastic IP address transfer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-eips.html#transfer-EIPs-intro Transfer Elastic IP addresses>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.EnableAddressTransfer
  ( -- * Creating a Request
    EnableAddressTransfer (..),
    newEnableAddressTransfer,

    -- * Request Lenses
    enableAddressTransfer_dryRun,
    enableAddressTransfer_allocationId,
    enableAddressTransfer_transferAccountId,

    -- * Destructuring the Response
    EnableAddressTransferResponse (..),
    newEnableAddressTransferResponse,

    -- * Response Lenses
    enableAddressTransferResponse_addressTransfer,
    enableAddressTransferResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAddressTransfer' smart constructor.
data EnableAddressTransfer = EnableAddressTransfer'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The allocation ID of an Elastic IP address.
    allocationId :: Prelude.Text,
    -- | The ID of the account that you want to transfer the Elastic IP address
    -- to.
    transferAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAddressTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableAddressTransfer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'allocationId', 'enableAddressTransfer_allocationId' - The allocation ID of an Elastic IP address.
--
-- 'transferAccountId', 'enableAddressTransfer_transferAccountId' - The ID of the account that you want to transfer the Elastic IP address
-- to.
newEnableAddressTransfer ::
  -- | 'allocationId'
  Prelude.Text ->
  -- | 'transferAccountId'
  Prelude.Text ->
  EnableAddressTransfer
newEnableAddressTransfer
  pAllocationId_
  pTransferAccountId_ =
    EnableAddressTransfer'
      { dryRun = Prelude.Nothing,
        allocationId = pAllocationId_,
        transferAccountId = pTransferAccountId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableAddressTransfer_dryRun :: Lens.Lens' EnableAddressTransfer (Prelude.Maybe Prelude.Bool)
enableAddressTransfer_dryRun = Lens.lens (\EnableAddressTransfer' {dryRun} -> dryRun) (\s@EnableAddressTransfer' {} a -> s {dryRun = a} :: EnableAddressTransfer)

-- | The allocation ID of an Elastic IP address.
enableAddressTransfer_allocationId :: Lens.Lens' EnableAddressTransfer Prelude.Text
enableAddressTransfer_allocationId = Lens.lens (\EnableAddressTransfer' {allocationId} -> allocationId) (\s@EnableAddressTransfer' {} a -> s {allocationId = a} :: EnableAddressTransfer)

-- | The ID of the account that you want to transfer the Elastic IP address
-- to.
enableAddressTransfer_transferAccountId :: Lens.Lens' EnableAddressTransfer Prelude.Text
enableAddressTransfer_transferAccountId = Lens.lens (\EnableAddressTransfer' {transferAccountId} -> transferAccountId) (\s@EnableAddressTransfer' {} a -> s {transferAccountId = a} :: EnableAddressTransfer)

instance Core.AWSRequest EnableAddressTransfer where
  type
    AWSResponse EnableAddressTransfer =
      EnableAddressTransferResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableAddressTransferResponse'
            Prelude.<$> (x Data..@? "addressTransfer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableAddressTransfer where
  hashWithSalt _salt EnableAddressTransfer' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` transferAccountId

instance Prelude.NFData EnableAddressTransfer where
  rnf EnableAddressTransfer' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf transferAccountId

instance Data.ToHeaders EnableAddressTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableAddressTransfer where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableAddressTransfer where
  toQuery EnableAddressTransfer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableAddressTransfer" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "AllocationId" Data.=: allocationId,
        "TransferAccountId" Data.=: transferAccountId
      ]

-- | /See:/ 'newEnableAddressTransferResponse' smart constructor.
data EnableAddressTransferResponse = EnableAddressTransferResponse'
  { -- | An Elastic IP address transfer.
    addressTransfer :: Prelude.Maybe AddressTransfer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAddressTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressTransfer', 'enableAddressTransferResponse_addressTransfer' - An Elastic IP address transfer.
--
-- 'httpStatus', 'enableAddressTransferResponse_httpStatus' - The response's http status code.
newEnableAddressTransferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableAddressTransferResponse
newEnableAddressTransferResponse pHttpStatus_ =
  EnableAddressTransferResponse'
    { addressTransfer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Elastic IP address transfer.
enableAddressTransferResponse_addressTransfer :: Lens.Lens' EnableAddressTransferResponse (Prelude.Maybe AddressTransfer)
enableAddressTransferResponse_addressTransfer = Lens.lens (\EnableAddressTransferResponse' {addressTransfer} -> addressTransfer) (\s@EnableAddressTransferResponse' {} a -> s {addressTransfer = a} :: EnableAddressTransferResponse)

-- | The response's http status code.
enableAddressTransferResponse_httpStatus :: Lens.Lens' EnableAddressTransferResponse Prelude.Int
enableAddressTransferResponse_httpStatus = Lens.lens (\EnableAddressTransferResponse' {httpStatus} -> httpStatus) (\s@EnableAddressTransferResponse' {} a -> s {httpStatus = a} :: EnableAddressTransferResponse)

instance Prelude.NFData EnableAddressTransferResponse where
  rnf EnableAddressTransferResponse' {..} =
    Prelude.rnf addressTransfer
      `Prelude.seq` Prelude.rnf httpStatus
