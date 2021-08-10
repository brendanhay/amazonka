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
-- Module      : Network.AWS.EC2.AdvertiseByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises an IPv4 or IPv6 address range that is provisioned for use
-- with your AWS resources through bring your own IP addresses (BYOIP).
--
-- You can perform this operation at most once every 10 seconds, even if
-- you specify different address ranges each time.
--
-- We recommend that you stop advertising the BYOIP CIDR from other
-- locations when you advertise it from AWS. To minimize down time, you can
-- configure your AWS resources to use an address from a BYOIP CIDR before
-- it is advertised, and then simultaneously stop advertising it from the
-- current location and start advertising it through AWS.
--
-- It can take a few minutes before traffic to the specified addresses
-- starts routing to AWS because of BGP propagation delays.
--
-- To stop advertising the BYOIP CIDR, use WithdrawByoipCidr.
module Network.AWS.EC2.AdvertiseByoipCidr
  ( -- * Creating a Request
    AdvertiseByoipCidr (..),
    newAdvertiseByoipCidr,

    -- * Request Lenses
    advertiseByoipCidr_dryRun,
    advertiseByoipCidr_cidr,

    -- * Destructuring the Response
    AdvertiseByoipCidrResponse (..),
    newAdvertiseByoipCidrResponse,

    -- * Response Lenses
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdvertiseByoipCidr' smart constructor.
data AdvertiseByoipCidr = AdvertiseByoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The address range, in CIDR notation. This must be the exact range that
    -- you provisioned. You can\'t advertise only a portion of the provisioned
    -- range.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvertiseByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'advertiseByoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'advertiseByoipCidr_cidr' - The address range, in CIDR notation. This must be the exact range that
-- you provisioned. You can\'t advertise only a portion of the provisioned
-- range.
newAdvertiseByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  AdvertiseByoipCidr
newAdvertiseByoipCidr pCidr_ =
  AdvertiseByoipCidr'
    { dryRun = Prelude.Nothing,
      cidr = pCidr_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
advertiseByoipCidr_dryRun :: Lens.Lens' AdvertiseByoipCidr (Prelude.Maybe Prelude.Bool)
advertiseByoipCidr_dryRun = Lens.lens (\AdvertiseByoipCidr' {dryRun} -> dryRun) (\s@AdvertiseByoipCidr' {} a -> s {dryRun = a} :: AdvertiseByoipCidr)

-- | The address range, in CIDR notation. This must be the exact range that
-- you provisioned. You can\'t advertise only a portion of the provisioned
-- range.
advertiseByoipCidr_cidr :: Lens.Lens' AdvertiseByoipCidr Prelude.Text
advertiseByoipCidr_cidr = Lens.lens (\AdvertiseByoipCidr' {cidr} -> cidr) (\s@AdvertiseByoipCidr' {} a -> s {cidr = a} :: AdvertiseByoipCidr)

instance Core.AWSRequest AdvertiseByoipCidr where
  type
    AWSResponse AdvertiseByoipCidr =
      AdvertiseByoipCidrResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AdvertiseByoipCidrResponse'
            Prelude.<$> (x Core..@? "byoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdvertiseByoipCidr

instance Prelude.NFData AdvertiseByoipCidr

instance Core.ToHeaders AdvertiseByoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AdvertiseByoipCidr where
  toPath = Prelude.const "/"

instance Core.ToQuery AdvertiseByoipCidr where
  toQuery AdvertiseByoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AdvertiseByoipCidr" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Cidr" Core.=: cidr
      ]

-- | /See:/ 'newAdvertiseByoipCidrResponse' smart constructor.
data AdvertiseByoipCidrResponse = AdvertiseByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvertiseByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'advertiseByoipCidrResponse_byoipCidr' - Information about the address range.
--
-- 'httpStatus', 'advertiseByoipCidrResponse_httpStatus' - The response's http status code.
newAdvertiseByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdvertiseByoipCidrResponse
newAdvertiseByoipCidrResponse pHttpStatus_ =
  AdvertiseByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range.
advertiseByoipCidrResponse_byoipCidr :: Lens.Lens' AdvertiseByoipCidrResponse (Prelude.Maybe ByoipCidr)
advertiseByoipCidrResponse_byoipCidr = Lens.lens (\AdvertiseByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@AdvertiseByoipCidrResponse' {} a -> s {byoipCidr = a} :: AdvertiseByoipCidrResponse)

-- | The response's http status code.
advertiseByoipCidrResponse_httpStatus :: Lens.Lens' AdvertiseByoipCidrResponse Prelude.Int
advertiseByoipCidrResponse_httpStatus = Lens.lens (\AdvertiseByoipCidrResponse' {httpStatus} -> httpStatus) (\s@AdvertiseByoipCidrResponse' {} a -> s {httpStatus = a} :: AdvertiseByoipCidrResponse)

instance Prelude.NFData AdvertiseByoipCidrResponse
