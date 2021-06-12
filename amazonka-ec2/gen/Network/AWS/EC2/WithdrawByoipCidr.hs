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
-- Module      : Network.AWS.EC2.WithdrawByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an address range that is provisioned as an address
-- pool.
--
-- You can perform this operation at most once every 10 seconds, even if
-- you specify different address ranges each time.
--
-- It can take a few minutes before traffic to the specified addresses
-- stops routing to AWS because of BGP propagation delays.
module Network.AWS.EC2.WithdrawByoipCidr
  ( -- * Creating a Request
    WithdrawByoipCidr (..),
    newWithdrawByoipCidr,

    -- * Request Lenses
    withdrawByoipCidr_dryRun,
    withdrawByoipCidr_cidr,

    -- * Destructuring the Response
    WithdrawByoipCidrResponse (..),
    newWithdrawByoipCidrResponse,

    -- * Response Lenses
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The address range, in CIDR notation.
    cidr :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WithdrawByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'withdrawByoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'withdrawByoipCidr_cidr' - The address range, in CIDR notation.
newWithdrawByoipCidr ::
  -- | 'cidr'
  Core.Text ->
  WithdrawByoipCidr
newWithdrawByoipCidr pCidr_ =
  WithdrawByoipCidr'
    { dryRun = Core.Nothing,
      cidr = pCidr_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
withdrawByoipCidr_dryRun :: Lens.Lens' WithdrawByoipCidr (Core.Maybe Core.Bool)
withdrawByoipCidr_dryRun = Lens.lens (\WithdrawByoipCidr' {dryRun} -> dryRun) (\s@WithdrawByoipCidr' {} a -> s {dryRun = a} :: WithdrawByoipCidr)

-- | The address range, in CIDR notation.
withdrawByoipCidr_cidr :: Lens.Lens' WithdrawByoipCidr Core.Text
withdrawByoipCidr_cidr = Lens.lens (\WithdrawByoipCidr' {cidr} -> cidr) (\s@WithdrawByoipCidr' {} a -> s {cidr = a} :: WithdrawByoipCidr)

instance Core.AWSRequest WithdrawByoipCidr where
  type
    AWSResponse WithdrawByoipCidr =
      WithdrawByoipCidrResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          WithdrawByoipCidrResponse'
            Core.<$> (x Core..@? "byoipCidr")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable WithdrawByoipCidr

instance Core.NFData WithdrawByoipCidr

instance Core.ToHeaders WithdrawByoipCidr where
  toHeaders = Core.const Core.mempty

instance Core.ToPath WithdrawByoipCidr where
  toPath = Core.const "/"

instance Core.ToQuery WithdrawByoipCidr where
  toQuery WithdrawByoipCidr' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("WithdrawByoipCidr" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Cidr" Core.=: cidr
      ]

-- | /See:/ 'newWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { -- | Information about the address pool.
    byoipCidr :: Core.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  WithdrawByoipCidrResponse
newWithdrawByoipCidrResponse pHttpStatus_ =
  WithdrawByoipCidrResponse'
    { byoipCidr =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address pool.
withdrawByoipCidrResponse_byoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Core.Maybe ByoipCidr)
withdrawByoipCidrResponse_byoipCidr = Lens.lens (\WithdrawByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@WithdrawByoipCidrResponse' {} a -> s {byoipCidr = a} :: WithdrawByoipCidrResponse)

-- | The response's http status code.
withdrawByoipCidrResponse_httpStatus :: Lens.Lens' WithdrawByoipCidrResponse Core.Int
withdrawByoipCidrResponse_httpStatus = Lens.lens (\WithdrawByoipCidrResponse' {httpStatus} -> httpStatus) (\s@WithdrawByoipCidrResponse' {} a -> s {httpStatus = a} :: WithdrawByoipCidrResponse)

instance Core.NFData WithdrawByoipCidrResponse
