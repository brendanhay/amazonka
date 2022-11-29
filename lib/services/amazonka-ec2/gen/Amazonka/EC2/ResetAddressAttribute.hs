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
-- Module      : Amazonka.EC2.ResetAddressAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the attribute of the specified IP address. For requirements, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html#Using_Elastic_Addressing_Reverse_DNS Using reverse DNS for email applications>.
module Amazonka.EC2.ResetAddressAttribute
  ( -- * Creating a Request
    ResetAddressAttribute (..),
    newResetAddressAttribute,

    -- * Request Lenses
    resetAddressAttribute_dryRun,
    resetAddressAttribute_allocationId,
    resetAddressAttribute_attribute,

    -- * Destructuring the Response
    ResetAddressAttributeResponse (..),
    newResetAddressAttributeResponse,

    -- * Response Lenses
    resetAddressAttributeResponse_address,
    resetAddressAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetAddressAttribute' smart constructor.
data ResetAddressAttribute = ResetAddressAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | [EC2-VPC] The allocation ID.
    allocationId :: Prelude.Text,
    -- | The attribute of the IP address.
    attribute :: AddressAttributeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAddressAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetAddressAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'allocationId', 'resetAddressAttribute_allocationId' - [EC2-VPC] The allocation ID.
--
-- 'attribute', 'resetAddressAttribute_attribute' - The attribute of the IP address.
newResetAddressAttribute ::
  -- | 'allocationId'
  Prelude.Text ->
  -- | 'attribute'
  AddressAttributeName ->
  ResetAddressAttribute
newResetAddressAttribute pAllocationId_ pAttribute_ =
  ResetAddressAttribute'
    { dryRun = Prelude.Nothing,
      allocationId = pAllocationId_,
      attribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetAddressAttribute_dryRun :: Lens.Lens' ResetAddressAttribute (Prelude.Maybe Prelude.Bool)
resetAddressAttribute_dryRun = Lens.lens (\ResetAddressAttribute' {dryRun} -> dryRun) (\s@ResetAddressAttribute' {} a -> s {dryRun = a} :: ResetAddressAttribute)

-- | [EC2-VPC] The allocation ID.
resetAddressAttribute_allocationId :: Lens.Lens' ResetAddressAttribute Prelude.Text
resetAddressAttribute_allocationId = Lens.lens (\ResetAddressAttribute' {allocationId} -> allocationId) (\s@ResetAddressAttribute' {} a -> s {allocationId = a} :: ResetAddressAttribute)

-- | The attribute of the IP address.
resetAddressAttribute_attribute :: Lens.Lens' ResetAddressAttribute AddressAttributeName
resetAddressAttribute_attribute = Lens.lens (\ResetAddressAttribute' {attribute} -> attribute) (\s@ResetAddressAttribute' {} a -> s {attribute = a} :: ResetAddressAttribute)

instance Core.AWSRequest ResetAddressAttribute where
  type
    AWSResponse ResetAddressAttribute =
      ResetAddressAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ResetAddressAttributeResponse'
            Prelude.<$> (x Core..@? "address")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetAddressAttribute where
  hashWithSalt _salt ResetAddressAttribute' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` attribute

instance Prelude.NFData ResetAddressAttribute where
  rnf ResetAddressAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf attribute

instance Core.ToHeaders ResetAddressAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResetAddressAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetAddressAttribute where
  toQuery ResetAddressAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ResetAddressAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "AllocationId" Core.=: allocationId,
        "Attribute" Core.=: attribute
      ]

-- | /See:/ 'newResetAddressAttributeResponse' smart constructor.
data ResetAddressAttributeResponse = ResetAddressAttributeResponse'
  { -- | Information about the IP address.
    address :: Prelude.Maybe AddressAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAddressAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'resetAddressAttributeResponse_address' - Information about the IP address.
--
-- 'httpStatus', 'resetAddressAttributeResponse_httpStatus' - The response's http status code.
newResetAddressAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetAddressAttributeResponse
newResetAddressAttributeResponse pHttpStatus_ =
  ResetAddressAttributeResponse'
    { address =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IP address.
resetAddressAttributeResponse_address :: Lens.Lens' ResetAddressAttributeResponse (Prelude.Maybe AddressAttribute)
resetAddressAttributeResponse_address = Lens.lens (\ResetAddressAttributeResponse' {address} -> address) (\s@ResetAddressAttributeResponse' {} a -> s {address = a} :: ResetAddressAttributeResponse)

-- | The response's http status code.
resetAddressAttributeResponse_httpStatus :: Lens.Lens' ResetAddressAttributeResponse Prelude.Int
resetAddressAttributeResponse_httpStatus = Lens.lens (\ResetAddressAttributeResponse' {httpStatus} -> httpStatus) (\s@ResetAddressAttributeResponse' {} a -> s {httpStatus = a} :: ResetAddressAttributeResponse)

instance Prelude.NFData ResetAddressAttributeResponse where
  rnf ResetAddressAttributeResponse' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf httpStatus
