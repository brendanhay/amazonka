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
-- Module      : Amazonka.GlobalAccelerator.WithdrawByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an address range that is provisioned as an address
-- pool. You can perform this operation at most once every 10 seconds, even
-- if you specify different address ranges each time.
--
-- It can take a few minutes before traffic to the specified addresses
-- stops routing to AWS because of propagation delays.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /AWS Global Accelerator Developer Guide/.
module Amazonka.GlobalAccelerator.WithdrawByoipCidr
  ( -- * Creating a Request
    WithdrawByoipCidr (..),
    newWithdrawByoipCidr,

    -- * Request Lenses
    withdrawByoipCidr_cidr,

    -- * Destructuring the Response
    WithdrawByoipCidrResponse (..),
    newWithdrawByoipCidrResponse,

    -- * Response Lenses
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { -- | The address range, in CIDR notation.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WithdrawByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'withdrawByoipCidr_cidr' - The address range, in CIDR notation.
newWithdrawByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  WithdrawByoipCidr
newWithdrawByoipCidr pCidr_ =
  WithdrawByoipCidr' {cidr = pCidr_}

-- | The address range, in CIDR notation.
withdrawByoipCidr_cidr :: Lens.Lens' WithdrawByoipCidr Prelude.Text
withdrawByoipCidr_cidr = Lens.lens (\WithdrawByoipCidr' {cidr} -> cidr) (\s@WithdrawByoipCidr' {} a -> s {cidr = a} :: WithdrawByoipCidr)

instance Core.AWSRequest WithdrawByoipCidr where
  type
    AWSResponse WithdrawByoipCidr =
      WithdrawByoipCidrResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          WithdrawByoipCidrResponse'
            Prelude.<$> (x Core..?> "ByoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable WithdrawByoipCidr

instance Prelude.NFData WithdrawByoipCidr

instance Core.ToHeaders WithdrawByoipCidr where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.WithdrawByoipCidr" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON WithdrawByoipCidr where
  toJSON WithdrawByoipCidr' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Cidr" Core..= cidr)]
      )

instance Core.ToPath WithdrawByoipCidr where
  toPath = Prelude.const "/"

instance Core.ToQuery WithdrawByoipCidr where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { -- | Information about the address pool.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WithdrawByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'withdrawByoipCidrResponse_byoipCidr' - Information about the address pool.
--
-- 'httpStatus', 'withdrawByoipCidrResponse_httpStatus' - The response's http status code.
newWithdrawByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  WithdrawByoipCidrResponse
newWithdrawByoipCidrResponse pHttpStatus_ =
  WithdrawByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address pool.
withdrawByoipCidrResponse_byoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Prelude.Maybe ByoipCidr)
withdrawByoipCidrResponse_byoipCidr = Lens.lens (\WithdrawByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@WithdrawByoipCidrResponse' {} a -> s {byoipCidr = a} :: WithdrawByoipCidrResponse)

-- | The response's http status code.
withdrawByoipCidrResponse_httpStatus :: Lens.Lens' WithdrawByoipCidrResponse Prelude.Int
withdrawByoipCidrResponse_httpStatus = Lens.lens (\WithdrawByoipCidrResponse' {httpStatus} -> httpStatus) (\s@WithdrawByoipCidrResponse' {} a -> s {httpStatus = a} :: WithdrawByoipCidrResponse)

instance Prelude.NFData WithdrawByoipCidrResponse
