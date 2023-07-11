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
-- Module      : Amazonka.EC2.ReleaseIpamPoolAllocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Release an allocation within an IPAM pool. You can only use this action
-- to release manual allocations. To remove an allocation for a resource
-- without deleting the resource, set its monitored state to false using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyIpamResourceCidr.html ModifyIpamResourceCidr>.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/release-pool-alloc-ipam.html Release an allocation>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.ReleaseIpamPoolAllocation
  ( -- * Creating a Request
    ReleaseIpamPoolAllocation (..),
    newReleaseIpamPoolAllocation,

    -- * Request Lenses
    releaseIpamPoolAllocation_dryRun,
    releaseIpamPoolAllocation_ipamPoolId,
    releaseIpamPoolAllocation_cidr,
    releaseIpamPoolAllocation_ipamPoolAllocationId,

    -- * Destructuring the Response
    ReleaseIpamPoolAllocationResponse (..),
    newReleaseIpamPoolAllocationResponse,

    -- * Response Lenses
    releaseIpamPoolAllocationResponse_success,
    releaseIpamPoolAllocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleaseIpamPoolAllocation' smart constructor.
data ReleaseIpamPoolAllocation = ReleaseIpamPoolAllocation'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the IPAM pool which contains the allocation you want to
    -- release.
    ipamPoolId :: Prelude.Text,
    -- | The CIDR of the allocation you want to release.
    cidr :: Prelude.Text,
    -- | The ID of the allocation.
    ipamPoolAllocationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseIpamPoolAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'releaseIpamPoolAllocation_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamPoolId', 'releaseIpamPoolAllocation_ipamPoolId' - The ID of the IPAM pool which contains the allocation you want to
-- release.
--
-- 'cidr', 'releaseIpamPoolAllocation_cidr' - The CIDR of the allocation you want to release.
--
-- 'ipamPoolAllocationId', 'releaseIpamPoolAllocation_ipamPoolAllocationId' - The ID of the allocation.
newReleaseIpamPoolAllocation ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  -- | 'cidr'
  Prelude.Text ->
  -- | 'ipamPoolAllocationId'
  Prelude.Text ->
  ReleaseIpamPoolAllocation
newReleaseIpamPoolAllocation
  pIpamPoolId_
  pCidr_
  pIpamPoolAllocationId_ =
    ReleaseIpamPoolAllocation'
      { dryRun =
          Prelude.Nothing,
        ipamPoolId = pIpamPoolId_,
        cidr = pCidr_,
        ipamPoolAllocationId = pIpamPoolAllocationId_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
releaseIpamPoolAllocation_dryRun :: Lens.Lens' ReleaseIpamPoolAllocation (Prelude.Maybe Prelude.Bool)
releaseIpamPoolAllocation_dryRun = Lens.lens (\ReleaseIpamPoolAllocation' {dryRun} -> dryRun) (\s@ReleaseIpamPoolAllocation' {} a -> s {dryRun = a} :: ReleaseIpamPoolAllocation)

-- | The ID of the IPAM pool which contains the allocation you want to
-- release.
releaseIpamPoolAllocation_ipamPoolId :: Lens.Lens' ReleaseIpamPoolAllocation Prelude.Text
releaseIpamPoolAllocation_ipamPoolId = Lens.lens (\ReleaseIpamPoolAllocation' {ipamPoolId} -> ipamPoolId) (\s@ReleaseIpamPoolAllocation' {} a -> s {ipamPoolId = a} :: ReleaseIpamPoolAllocation)

-- | The CIDR of the allocation you want to release.
releaseIpamPoolAllocation_cidr :: Lens.Lens' ReleaseIpamPoolAllocation Prelude.Text
releaseIpamPoolAllocation_cidr = Lens.lens (\ReleaseIpamPoolAllocation' {cidr} -> cidr) (\s@ReleaseIpamPoolAllocation' {} a -> s {cidr = a} :: ReleaseIpamPoolAllocation)

-- | The ID of the allocation.
releaseIpamPoolAllocation_ipamPoolAllocationId :: Lens.Lens' ReleaseIpamPoolAllocation Prelude.Text
releaseIpamPoolAllocation_ipamPoolAllocationId = Lens.lens (\ReleaseIpamPoolAllocation' {ipamPoolAllocationId} -> ipamPoolAllocationId) (\s@ReleaseIpamPoolAllocation' {} a -> s {ipamPoolAllocationId = a} :: ReleaseIpamPoolAllocation)

instance Core.AWSRequest ReleaseIpamPoolAllocation where
  type
    AWSResponse ReleaseIpamPoolAllocation =
      ReleaseIpamPoolAllocationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ReleaseIpamPoolAllocationResponse'
            Prelude.<$> (x Data..@? "success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReleaseIpamPoolAllocation where
  hashWithSalt _salt ReleaseIpamPoolAllocation' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` ipamPoolAllocationId

instance Prelude.NFData ReleaseIpamPoolAllocation where
  rnf ReleaseIpamPoolAllocation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf ipamPoolAllocationId

instance Data.ToHeaders ReleaseIpamPoolAllocation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReleaseIpamPoolAllocation where
  toPath = Prelude.const "/"

instance Data.ToQuery ReleaseIpamPoolAllocation where
  toQuery ReleaseIpamPoolAllocation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReleaseIpamPoolAllocation" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamPoolId" Data.=: ipamPoolId,
        "Cidr" Data.=: cidr,
        "IpamPoolAllocationId" Data.=: ipamPoolAllocationId
      ]

-- | /See:/ 'newReleaseIpamPoolAllocationResponse' smart constructor.
data ReleaseIpamPoolAllocationResponse = ReleaseIpamPoolAllocationResponse'
  { -- | Indicates if the release was successful.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseIpamPoolAllocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'releaseIpamPoolAllocationResponse_success' - Indicates if the release was successful.
--
-- 'httpStatus', 'releaseIpamPoolAllocationResponse_httpStatus' - The response's http status code.
newReleaseIpamPoolAllocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReleaseIpamPoolAllocationResponse
newReleaseIpamPoolAllocationResponse pHttpStatus_ =
  ReleaseIpamPoolAllocationResponse'
    { success =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates if the release was successful.
releaseIpamPoolAllocationResponse_success :: Lens.Lens' ReleaseIpamPoolAllocationResponse (Prelude.Maybe Prelude.Bool)
releaseIpamPoolAllocationResponse_success = Lens.lens (\ReleaseIpamPoolAllocationResponse' {success} -> success) (\s@ReleaseIpamPoolAllocationResponse' {} a -> s {success = a} :: ReleaseIpamPoolAllocationResponse)

-- | The response's http status code.
releaseIpamPoolAllocationResponse_httpStatus :: Lens.Lens' ReleaseIpamPoolAllocationResponse Prelude.Int
releaseIpamPoolAllocationResponse_httpStatus = Lens.lens (\ReleaseIpamPoolAllocationResponse' {httpStatus} -> httpStatus) (\s@ReleaseIpamPoolAllocationResponse' {} a -> s {httpStatus = a} :: ReleaseIpamPoolAllocationResponse)

instance
  Prelude.NFData
    ReleaseIpamPoolAllocationResponse
  where
  rnf ReleaseIpamPoolAllocationResponse' {..} =
    Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
