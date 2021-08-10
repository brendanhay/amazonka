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
-- Module      : Network.AWS.EC2.DeprovisionByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified address range that you provisioned for use with
-- your AWS resources through bring your own IP addresses (BYOIP) and
-- deletes the corresponding address pool.
--
-- Before you can release an address range, you must stop advertising it
-- using WithdrawByoipCidr and you must not have any IP addresses allocated
-- from its address range.
module Network.AWS.EC2.DeprovisionByoipCidr
  ( -- * Creating a Request
    DeprovisionByoipCidr (..),
    newDeprovisionByoipCidr,

    -- * Request Lenses
    deprovisionByoipCidr_dryRun,
    deprovisionByoipCidr_cidr,

    -- * Destructuring the Response
    DeprovisionByoipCidrResponse (..),
    newDeprovisionByoipCidrResponse,

    -- * Response Lenses
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeprovisionByoipCidr' smart constructor.
data DeprovisionByoipCidr = DeprovisionByoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The address range, in CIDR notation. The prefix must be the same prefix
    -- that you specified when you provisioned the address range.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deprovisionByoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'deprovisionByoipCidr_cidr' - The address range, in CIDR notation. The prefix must be the same prefix
-- that you specified when you provisioned the address range.
newDeprovisionByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  DeprovisionByoipCidr
newDeprovisionByoipCidr pCidr_ =
  DeprovisionByoipCidr'
    { dryRun = Prelude.Nothing,
      cidr = pCidr_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deprovisionByoipCidr_dryRun :: Lens.Lens' DeprovisionByoipCidr (Prelude.Maybe Prelude.Bool)
deprovisionByoipCidr_dryRun = Lens.lens (\DeprovisionByoipCidr' {dryRun} -> dryRun) (\s@DeprovisionByoipCidr' {} a -> s {dryRun = a} :: DeprovisionByoipCidr)

-- | The address range, in CIDR notation. The prefix must be the same prefix
-- that you specified when you provisioned the address range.
deprovisionByoipCidr_cidr :: Lens.Lens' DeprovisionByoipCidr Prelude.Text
deprovisionByoipCidr_cidr = Lens.lens (\DeprovisionByoipCidr' {cidr} -> cidr) (\s@DeprovisionByoipCidr' {} a -> s {cidr = a} :: DeprovisionByoipCidr)

instance Core.AWSRequest DeprovisionByoipCidr where
  type
    AWSResponse DeprovisionByoipCidr =
      DeprovisionByoipCidrResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeprovisionByoipCidrResponse'
            Prelude.<$> (x Core..@? "byoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeprovisionByoipCidr

instance Prelude.NFData DeprovisionByoipCidr

instance Core.ToHeaders DeprovisionByoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeprovisionByoipCidr where
  toPath = Prelude.const "/"

instance Core.ToQuery DeprovisionByoipCidr where
  toQuery DeprovisionByoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeprovisionByoipCidr" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Cidr" Core.=: cidr
      ]

-- | /See:/ 'newDeprovisionByoipCidrResponse' smart constructor.
data DeprovisionByoipCidrResponse = DeprovisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'deprovisionByoipCidrResponse_byoipCidr' - Information about the address range.
--
-- 'httpStatus', 'deprovisionByoipCidrResponse_httpStatus' - The response's http status code.
newDeprovisionByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprovisionByoipCidrResponse
newDeprovisionByoipCidrResponse pHttpStatus_ =
  DeprovisionByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range.
deprovisionByoipCidrResponse_byoipCidr :: Lens.Lens' DeprovisionByoipCidrResponse (Prelude.Maybe ByoipCidr)
deprovisionByoipCidrResponse_byoipCidr = Lens.lens (\DeprovisionByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@DeprovisionByoipCidrResponse' {} a -> s {byoipCidr = a} :: DeprovisionByoipCidrResponse)

-- | The response's http status code.
deprovisionByoipCidrResponse_httpStatus :: Lens.Lens' DeprovisionByoipCidrResponse Prelude.Int
deprovisionByoipCidrResponse_httpStatus = Lens.lens (\DeprovisionByoipCidrResponse' {httpStatus} -> httpStatus) (\s@DeprovisionByoipCidrResponse' {} a -> s {httpStatus = a} :: DeprovisionByoipCidrResponse)

instance Prelude.NFData DeprovisionByoipCidrResponse
