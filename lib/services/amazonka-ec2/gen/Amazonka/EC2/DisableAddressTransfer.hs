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
-- Module      : Amazonka.EC2.DisableAddressTransfer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables Elastic IP address transfer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-eips.html#transfer-EIPs-intro Transfer Elastic IP addresses>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.DisableAddressTransfer
  ( -- * Creating a Request
    DisableAddressTransfer (..),
    newDisableAddressTransfer,

    -- * Request Lenses
    disableAddressTransfer_dryRun,
    disableAddressTransfer_allocationId,

    -- * Destructuring the Response
    DisableAddressTransferResponse (..),
    newDisableAddressTransferResponse,

    -- * Response Lenses
    disableAddressTransferResponse_addressTransfer,
    disableAddressTransferResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableAddressTransfer' smart constructor.
data DisableAddressTransfer = DisableAddressTransfer'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The allocation ID of an Elastic IP address.
    allocationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAddressTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableAddressTransfer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'allocationId', 'disableAddressTransfer_allocationId' - The allocation ID of an Elastic IP address.
newDisableAddressTransfer ::
  -- | 'allocationId'
  Prelude.Text ->
  DisableAddressTransfer
newDisableAddressTransfer pAllocationId_ =
  DisableAddressTransfer'
    { dryRun = Prelude.Nothing,
      allocationId = pAllocationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableAddressTransfer_dryRun :: Lens.Lens' DisableAddressTransfer (Prelude.Maybe Prelude.Bool)
disableAddressTransfer_dryRun = Lens.lens (\DisableAddressTransfer' {dryRun} -> dryRun) (\s@DisableAddressTransfer' {} a -> s {dryRun = a} :: DisableAddressTransfer)

-- | The allocation ID of an Elastic IP address.
disableAddressTransfer_allocationId :: Lens.Lens' DisableAddressTransfer Prelude.Text
disableAddressTransfer_allocationId = Lens.lens (\DisableAddressTransfer' {allocationId} -> allocationId) (\s@DisableAddressTransfer' {} a -> s {allocationId = a} :: DisableAddressTransfer)

instance Core.AWSRequest DisableAddressTransfer where
  type
    AWSResponse DisableAddressTransfer =
      DisableAddressTransferResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableAddressTransferResponse'
            Prelude.<$> (x Core..@? "addressTransfer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableAddressTransfer where
  hashWithSalt _salt DisableAddressTransfer' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` allocationId

instance Prelude.NFData DisableAddressTransfer where
  rnf DisableAddressTransfer' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf allocationId

instance Core.ToHeaders DisableAddressTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisableAddressTransfer where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableAddressTransfer where
  toQuery DisableAddressTransfer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DisableAddressTransfer" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "AllocationId" Core.=: allocationId
      ]

-- | /See:/ 'newDisableAddressTransferResponse' smart constructor.
data DisableAddressTransferResponse = DisableAddressTransferResponse'
  { -- | An Elastic IP address transfer.
    addressTransfer :: Prelude.Maybe AddressTransfer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAddressTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressTransfer', 'disableAddressTransferResponse_addressTransfer' - An Elastic IP address transfer.
--
-- 'httpStatus', 'disableAddressTransferResponse_httpStatus' - The response's http status code.
newDisableAddressTransferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableAddressTransferResponse
newDisableAddressTransferResponse pHttpStatus_ =
  DisableAddressTransferResponse'
    { addressTransfer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Elastic IP address transfer.
disableAddressTransferResponse_addressTransfer :: Lens.Lens' DisableAddressTransferResponse (Prelude.Maybe AddressTransfer)
disableAddressTransferResponse_addressTransfer = Lens.lens (\DisableAddressTransferResponse' {addressTransfer} -> addressTransfer) (\s@DisableAddressTransferResponse' {} a -> s {addressTransfer = a} :: DisableAddressTransferResponse)

-- | The response's http status code.
disableAddressTransferResponse_httpStatus :: Lens.Lens' DisableAddressTransferResponse Prelude.Int
disableAddressTransferResponse_httpStatus = Lens.lens (\DisableAddressTransferResponse' {httpStatus} -> httpStatus) (\s@DisableAddressTransferResponse' {} a -> s {httpStatus = a} :: DisableAddressTransferResponse)

instance
  Prelude.NFData
    DisableAddressTransferResponse
  where
  rnf DisableAddressTransferResponse' {..} =
    Prelude.rnf addressTransfer
      `Prelude.seq` Prelude.rnf httpStatus
