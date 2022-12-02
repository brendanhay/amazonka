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
-- Module      : Amazonka.EC2.ProvisionPublicIpv4PoolCidr
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provision a CIDR to a public IPv4 pool.
--
-- For more information about IPAM, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.ProvisionPublicIpv4PoolCidr
  ( -- * Creating a Request
    ProvisionPublicIpv4PoolCidr (..),
    newProvisionPublicIpv4PoolCidr,

    -- * Request Lenses
    provisionPublicIpv4PoolCidr_dryRun,
    provisionPublicIpv4PoolCidr_ipamPoolId,
    provisionPublicIpv4PoolCidr_poolId,
    provisionPublicIpv4PoolCidr_netmaskLength,

    -- * Destructuring the Response
    ProvisionPublicIpv4PoolCidrResponse (..),
    newProvisionPublicIpv4PoolCidrResponse,

    -- * Response Lenses
    provisionPublicIpv4PoolCidrResponse_poolAddressRange,
    provisionPublicIpv4PoolCidrResponse_poolId,
    provisionPublicIpv4PoolCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newProvisionPublicIpv4PoolCidr' smart constructor.
data ProvisionPublicIpv4PoolCidr = ProvisionPublicIpv4PoolCidr'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the IPAM pool you would like to use to allocate this CIDR.
    ipamPoolId :: Prelude.Text,
    -- | The ID of the public IPv4 pool you would like to use for this CIDR.
    poolId :: Prelude.Text,
    -- | The netmask length of the CIDR you would like to allocate to the public
    -- IPv4 pool.
    netmaskLength :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionPublicIpv4PoolCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'provisionPublicIpv4PoolCidr_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamPoolId', 'provisionPublicIpv4PoolCidr_ipamPoolId' - The ID of the IPAM pool you would like to use to allocate this CIDR.
--
-- 'poolId', 'provisionPublicIpv4PoolCidr_poolId' - The ID of the public IPv4 pool you would like to use for this CIDR.
--
-- 'netmaskLength', 'provisionPublicIpv4PoolCidr_netmaskLength' - The netmask length of the CIDR you would like to allocate to the public
-- IPv4 pool.
newProvisionPublicIpv4PoolCidr ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  -- | 'poolId'
  Prelude.Text ->
  -- | 'netmaskLength'
  Prelude.Int ->
  ProvisionPublicIpv4PoolCidr
newProvisionPublicIpv4PoolCidr
  pIpamPoolId_
  pPoolId_
  pNetmaskLength_ =
    ProvisionPublicIpv4PoolCidr'
      { dryRun =
          Prelude.Nothing,
        ipamPoolId = pIpamPoolId_,
        poolId = pPoolId_,
        netmaskLength = pNetmaskLength_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
provisionPublicIpv4PoolCidr_dryRun :: Lens.Lens' ProvisionPublicIpv4PoolCidr (Prelude.Maybe Prelude.Bool)
provisionPublicIpv4PoolCidr_dryRun = Lens.lens (\ProvisionPublicIpv4PoolCidr' {dryRun} -> dryRun) (\s@ProvisionPublicIpv4PoolCidr' {} a -> s {dryRun = a} :: ProvisionPublicIpv4PoolCidr)

-- | The ID of the IPAM pool you would like to use to allocate this CIDR.
provisionPublicIpv4PoolCidr_ipamPoolId :: Lens.Lens' ProvisionPublicIpv4PoolCidr Prelude.Text
provisionPublicIpv4PoolCidr_ipamPoolId = Lens.lens (\ProvisionPublicIpv4PoolCidr' {ipamPoolId} -> ipamPoolId) (\s@ProvisionPublicIpv4PoolCidr' {} a -> s {ipamPoolId = a} :: ProvisionPublicIpv4PoolCidr)

-- | The ID of the public IPv4 pool you would like to use for this CIDR.
provisionPublicIpv4PoolCidr_poolId :: Lens.Lens' ProvisionPublicIpv4PoolCidr Prelude.Text
provisionPublicIpv4PoolCidr_poolId = Lens.lens (\ProvisionPublicIpv4PoolCidr' {poolId} -> poolId) (\s@ProvisionPublicIpv4PoolCidr' {} a -> s {poolId = a} :: ProvisionPublicIpv4PoolCidr)

-- | The netmask length of the CIDR you would like to allocate to the public
-- IPv4 pool.
provisionPublicIpv4PoolCidr_netmaskLength :: Lens.Lens' ProvisionPublicIpv4PoolCidr Prelude.Int
provisionPublicIpv4PoolCidr_netmaskLength = Lens.lens (\ProvisionPublicIpv4PoolCidr' {netmaskLength} -> netmaskLength) (\s@ProvisionPublicIpv4PoolCidr' {} a -> s {netmaskLength = a} :: ProvisionPublicIpv4PoolCidr)

instance Core.AWSRequest ProvisionPublicIpv4PoolCidr where
  type
    AWSResponse ProvisionPublicIpv4PoolCidr =
      ProvisionPublicIpv4PoolCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ProvisionPublicIpv4PoolCidrResponse'
            Prelude.<$> (x Data..@? "poolAddressRange")
            Prelude.<*> (x Data..@? "poolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ProvisionPublicIpv4PoolCidr where
  hashWithSalt _salt ProvisionPublicIpv4PoolCidr' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` netmaskLength

instance Prelude.NFData ProvisionPublicIpv4PoolCidr where
  rnf ProvisionPublicIpv4PoolCidr' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf netmaskLength

instance Data.ToHeaders ProvisionPublicIpv4PoolCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ProvisionPublicIpv4PoolCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery ProvisionPublicIpv4PoolCidr where
  toQuery ProvisionPublicIpv4PoolCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ProvisionPublicIpv4PoolCidr" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamPoolId" Data.=: ipamPoolId,
        "PoolId" Data.=: poolId,
        "NetmaskLength" Data.=: netmaskLength
      ]

-- | /See:/ 'newProvisionPublicIpv4PoolCidrResponse' smart constructor.
data ProvisionPublicIpv4PoolCidrResponse = ProvisionPublicIpv4PoolCidrResponse'
  { -- | Information about the address range of the public IPv4 pool.
    poolAddressRange :: Prelude.Maybe PublicIpv4PoolRange,
    -- | The ID of the pool that you want to provision the CIDR to.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionPublicIpv4PoolCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolAddressRange', 'provisionPublicIpv4PoolCidrResponse_poolAddressRange' - Information about the address range of the public IPv4 pool.
--
-- 'poolId', 'provisionPublicIpv4PoolCidrResponse_poolId' - The ID of the pool that you want to provision the CIDR to.
--
-- 'httpStatus', 'provisionPublicIpv4PoolCidrResponse_httpStatus' - The response's http status code.
newProvisionPublicIpv4PoolCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ProvisionPublicIpv4PoolCidrResponse
newProvisionPublicIpv4PoolCidrResponse pHttpStatus_ =
  ProvisionPublicIpv4PoolCidrResponse'
    { poolAddressRange =
        Prelude.Nothing,
      poolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range of the public IPv4 pool.
provisionPublicIpv4PoolCidrResponse_poolAddressRange :: Lens.Lens' ProvisionPublicIpv4PoolCidrResponse (Prelude.Maybe PublicIpv4PoolRange)
provisionPublicIpv4PoolCidrResponse_poolAddressRange = Lens.lens (\ProvisionPublicIpv4PoolCidrResponse' {poolAddressRange} -> poolAddressRange) (\s@ProvisionPublicIpv4PoolCidrResponse' {} a -> s {poolAddressRange = a} :: ProvisionPublicIpv4PoolCidrResponse)

-- | The ID of the pool that you want to provision the CIDR to.
provisionPublicIpv4PoolCidrResponse_poolId :: Lens.Lens' ProvisionPublicIpv4PoolCidrResponse (Prelude.Maybe Prelude.Text)
provisionPublicIpv4PoolCidrResponse_poolId = Lens.lens (\ProvisionPublicIpv4PoolCidrResponse' {poolId} -> poolId) (\s@ProvisionPublicIpv4PoolCidrResponse' {} a -> s {poolId = a} :: ProvisionPublicIpv4PoolCidrResponse)

-- | The response's http status code.
provisionPublicIpv4PoolCidrResponse_httpStatus :: Lens.Lens' ProvisionPublicIpv4PoolCidrResponse Prelude.Int
provisionPublicIpv4PoolCidrResponse_httpStatus = Lens.lens (\ProvisionPublicIpv4PoolCidrResponse' {httpStatus} -> httpStatus) (\s@ProvisionPublicIpv4PoolCidrResponse' {} a -> s {httpStatus = a} :: ProvisionPublicIpv4PoolCidrResponse)

instance
  Prelude.NFData
    ProvisionPublicIpv4PoolCidrResponse
  where
  rnf ProvisionPublicIpv4PoolCidrResponse' {..} =
    Prelude.rnf poolAddressRange
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf httpStatus
