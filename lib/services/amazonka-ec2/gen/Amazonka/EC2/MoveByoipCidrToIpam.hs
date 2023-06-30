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
-- Module      : Amazonka.EC2.MoveByoipCidrToIpam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Move an BYOIP IPv4 CIDR to IPAM from a public IPv4 pool.
--
-- If you already have an IPv4 BYOIP CIDR with Amazon Web Services, you can
-- move the CIDR to IPAM from a public IPv4 pool. You cannot move an IPv6
-- CIDR to IPAM. If you are bringing a new IP address to Amazon Web
-- Services for the first time, complete the steps in
-- <https://docs.aws.amazon.com/vpc/latest/ipam/tutorials-byoip-ipam.html Tutorial: BYOIP address CIDRs to IPAM>.
module Amazonka.EC2.MoveByoipCidrToIpam
  ( -- * Creating a Request
    MoveByoipCidrToIpam (..),
    newMoveByoipCidrToIpam,

    -- * Request Lenses
    moveByoipCidrToIpam_dryRun,
    moveByoipCidrToIpam_cidr,
    moveByoipCidrToIpam_ipamPoolId,
    moveByoipCidrToIpam_ipamPoolOwner,

    -- * Destructuring the Response
    MoveByoipCidrToIpamResponse (..),
    newMoveByoipCidrToIpamResponse,

    -- * Response Lenses
    moveByoipCidrToIpamResponse_byoipCidr,
    moveByoipCidrToIpamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMoveByoipCidrToIpam' smart constructor.
data MoveByoipCidrToIpam = MoveByoipCidrToIpam'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The BYOIP CIDR.
    cidr :: Prelude.Text,
    -- | The IPAM pool ID.
    ipamPoolId :: Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the IPAM pool.
    ipamPoolOwner :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveByoipCidrToIpam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'moveByoipCidrToIpam_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'moveByoipCidrToIpam_cidr' - The BYOIP CIDR.
--
-- 'ipamPoolId', 'moveByoipCidrToIpam_ipamPoolId' - The IPAM pool ID.
--
-- 'ipamPoolOwner', 'moveByoipCidrToIpam_ipamPoolOwner' - The Amazon Web Services account ID of the owner of the IPAM pool.
newMoveByoipCidrToIpam ::
  -- | 'cidr'
  Prelude.Text ->
  -- | 'ipamPoolId'
  Prelude.Text ->
  -- | 'ipamPoolOwner'
  Prelude.Text ->
  MoveByoipCidrToIpam
newMoveByoipCidrToIpam
  pCidr_
  pIpamPoolId_
  pIpamPoolOwner_ =
    MoveByoipCidrToIpam'
      { dryRun = Prelude.Nothing,
        cidr = pCidr_,
        ipamPoolId = pIpamPoolId_,
        ipamPoolOwner = pIpamPoolOwner_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
moveByoipCidrToIpam_dryRun :: Lens.Lens' MoveByoipCidrToIpam (Prelude.Maybe Prelude.Bool)
moveByoipCidrToIpam_dryRun = Lens.lens (\MoveByoipCidrToIpam' {dryRun} -> dryRun) (\s@MoveByoipCidrToIpam' {} a -> s {dryRun = a} :: MoveByoipCidrToIpam)

-- | The BYOIP CIDR.
moveByoipCidrToIpam_cidr :: Lens.Lens' MoveByoipCidrToIpam Prelude.Text
moveByoipCidrToIpam_cidr = Lens.lens (\MoveByoipCidrToIpam' {cidr} -> cidr) (\s@MoveByoipCidrToIpam' {} a -> s {cidr = a} :: MoveByoipCidrToIpam)

-- | The IPAM pool ID.
moveByoipCidrToIpam_ipamPoolId :: Lens.Lens' MoveByoipCidrToIpam Prelude.Text
moveByoipCidrToIpam_ipamPoolId = Lens.lens (\MoveByoipCidrToIpam' {ipamPoolId} -> ipamPoolId) (\s@MoveByoipCidrToIpam' {} a -> s {ipamPoolId = a} :: MoveByoipCidrToIpam)

-- | The Amazon Web Services account ID of the owner of the IPAM pool.
moveByoipCidrToIpam_ipamPoolOwner :: Lens.Lens' MoveByoipCidrToIpam Prelude.Text
moveByoipCidrToIpam_ipamPoolOwner = Lens.lens (\MoveByoipCidrToIpam' {ipamPoolOwner} -> ipamPoolOwner) (\s@MoveByoipCidrToIpam' {} a -> s {ipamPoolOwner = a} :: MoveByoipCidrToIpam)

instance Core.AWSRequest MoveByoipCidrToIpam where
  type
    AWSResponse MoveByoipCidrToIpam =
      MoveByoipCidrToIpamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          MoveByoipCidrToIpamResponse'
            Prelude.<$> (x Data..@? "byoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MoveByoipCidrToIpam where
  hashWithSalt _salt MoveByoipCidrToIpam' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` ipamPoolOwner

instance Prelude.NFData MoveByoipCidrToIpam where
  rnf MoveByoipCidrToIpam' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf ipamPoolOwner

instance Data.ToHeaders MoveByoipCidrToIpam where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath MoveByoipCidrToIpam where
  toPath = Prelude.const "/"

instance Data.ToQuery MoveByoipCidrToIpam where
  toQuery MoveByoipCidrToIpam' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("MoveByoipCidrToIpam" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Cidr" Data.=: cidr,
        "IpamPoolId" Data.=: ipamPoolId,
        "IpamPoolOwner" Data.=: ipamPoolOwner
      ]

-- | /See:/ 'newMoveByoipCidrToIpamResponse' smart constructor.
data MoveByoipCidrToIpamResponse = MoveByoipCidrToIpamResponse'
  { -- | The BYOIP CIDR.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveByoipCidrToIpamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'moveByoipCidrToIpamResponse_byoipCidr' - The BYOIP CIDR.
--
-- 'httpStatus', 'moveByoipCidrToIpamResponse_httpStatus' - The response's http status code.
newMoveByoipCidrToIpamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MoveByoipCidrToIpamResponse
newMoveByoipCidrToIpamResponse pHttpStatus_ =
  MoveByoipCidrToIpamResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The BYOIP CIDR.
moveByoipCidrToIpamResponse_byoipCidr :: Lens.Lens' MoveByoipCidrToIpamResponse (Prelude.Maybe ByoipCidr)
moveByoipCidrToIpamResponse_byoipCidr = Lens.lens (\MoveByoipCidrToIpamResponse' {byoipCidr} -> byoipCidr) (\s@MoveByoipCidrToIpamResponse' {} a -> s {byoipCidr = a} :: MoveByoipCidrToIpamResponse)

-- | The response's http status code.
moveByoipCidrToIpamResponse_httpStatus :: Lens.Lens' MoveByoipCidrToIpamResponse Prelude.Int
moveByoipCidrToIpamResponse_httpStatus = Lens.lens (\MoveByoipCidrToIpamResponse' {httpStatus} -> httpStatus) (\s@MoveByoipCidrToIpamResponse' {} a -> s {httpStatus = a} :: MoveByoipCidrToIpamResponse)

instance Prelude.NFData MoveByoipCidrToIpamResponse where
  rnf MoveByoipCidrToIpamResponse' {..} =
    Prelude.rnf byoipCidr
      `Prelude.seq` Prelude.rnf httpStatus
