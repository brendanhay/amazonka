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
-- Module      : Amazonka.EC2.DeprovisionPublicIpv4PoolCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprovision a CIDR from a public IPv4 pool.
module Amazonka.EC2.DeprovisionPublicIpv4PoolCidr
  ( -- * Creating a Request
    DeprovisionPublicIpv4PoolCidr (..),
    newDeprovisionPublicIpv4PoolCidr,

    -- * Request Lenses
    deprovisionPublicIpv4PoolCidr_dryRun,
    deprovisionPublicIpv4PoolCidr_poolId,
    deprovisionPublicIpv4PoolCidr_cidr,

    -- * Destructuring the Response
    DeprovisionPublicIpv4PoolCidrResponse (..),
    newDeprovisionPublicIpv4PoolCidrResponse,

    -- * Response Lenses
    deprovisionPublicIpv4PoolCidrResponse_deprovisionedAddresses,
    deprovisionPublicIpv4PoolCidrResponse_poolId,
    deprovisionPublicIpv4PoolCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeprovisionPublicIpv4PoolCidr' smart constructor.
data DeprovisionPublicIpv4PoolCidr = DeprovisionPublicIpv4PoolCidr'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pool that you want to deprovision the CIDR from.
    poolId :: Prelude.Text,
    -- | The CIDR you want to deprovision from the pool. Enter the CIDR you want
    -- to deprovision with a netmask of @\/32@. You must rerun this command for
    -- each IP address in the CIDR range. If your CIDR is a @\/24@, you will
    -- have to run this command to deprovision each of the 256 IP addresses in
    -- the @\/24@ CIDR.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionPublicIpv4PoolCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deprovisionPublicIpv4PoolCidr_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'poolId', 'deprovisionPublicIpv4PoolCidr_poolId' - The ID of the pool that you want to deprovision the CIDR from.
--
-- 'cidr', 'deprovisionPublicIpv4PoolCidr_cidr' - The CIDR you want to deprovision from the pool. Enter the CIDR you want
-- to deprovision with a netmask of @\/32@. You must rerun this command for
-- each IP address in the CIDR range. If your CIDR is a @\/24@, you will
-- have to run this command to deprovision each of the 256 IP addresses in
-- the @\/24@ CIDR.
newDeprovisionPublicIpv4PoolCidr ::
  -- | 'poolId'
  Prelude.Text ->
  -- | 'cidr'
  Prelude.Text ->
  DeprovisionPublicIpv4PoolCidr
newDeprovisionPublicIpv4PoolCidr pPoolId_ pCidr_ =
  DeprovisionPublicIpv4PoolCidr'
    { dryRun =
        Prelude.Nothing,
      poolId = pPoolId_,
      cidr = pCidr_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deprovisionPublicIpv4PoolCidr_dryRun :: Lens.Lens' DeprovisionPublicIpv4PoolCidr (Prelude.Maybe Prelude.Bool)
deprovisionPublicIpv4PoolCidr_dryRun = Lens.lens (\DeprovisionPublicIpv4PoolCidr' {dryRun} -> dryRun) (\s@DeprovisionPublicIpv4PoolCidr' {} a -> s {dryRun = a} :: DeprovisionPublicIpv4PoolCidr)

-- | The ID of the pool that you want to deprovision the CIDR from.
deprovisionPublicIpv4PoolCidr_poolId :: Lens.Lens' DeprovisionPublicIpv4PoolCidr Prelude.Text
deprovisionPublicIpv4PoolCidr_poolId = Lens.lens (\DeprovisionPublicIpv4PoolCidr' {poolId} -> poolId) (\s@DeprovisionPublicIpv4PoolCidr' {} a -> s {poolId = a} :: DeprovisionPublicIpv4PoolCidr)

-- | The CIDR you want to deprovision from the pool. Enter the CIDR you want
-- to deprovision with a netmask of @\/32@. You must rerun this command for
-- each IP address in the CIDR range. If your CIDR is a @\/24@, you will
-- have to run this command to deprovision each of the 256 IP addresses in
-- the @\/24@ CIDR.
deprovisionPublicIpv4PoolCidr_cidr :: Lens.Lens' DeprovisionPublicIpv4PoolCidr Prelude.Text
deprovisionPublicIpv4PoolCidr_cidr = Lens.lens (\DeprovisionPublicIpv4PoolCidr' {cidr} -> cidr) (\s@DeprovisionPublicIpv4PoolCidr' {} a -> s {cidr = a} :: DeprovisionPublicIpv4PoolCidr)

instance
  Core.AWSRequest
    DeprovisionPublicIpv4PoolCidr
  where
  type
    AWSResponse DeprovisionPublicIpv4PoolCidr =
      DeprovisionPublicIpv4PoolCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeprovisionPublicIpv4PoolCidrResponse'
            Prelude.<$> ( x
                            Data..@? "deprovisionedAddressSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "poolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeprovisionPublicIpv4PoolCidr
  where
  hashWithSalt _salt DeprovisionPublicIpv4PoolCidr' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` cidr

instance Prelude.NFData DeprovisionPublicIpv4PoolCidr where
  rnf DeprovisionPublicIpv4PoolCidr' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf cidr

instance Data.ToHeaders DeprovisionPublicIpv4PoolCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeprovisionPublicIpv4PoolCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery DeprovisionPublicIpv4PoolCidr where
  toQuery DeprovisionPublicIpv4PoolCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeprovisionPublicIpv4PoolCidr" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PoolId" Data.=: poolId,
        "Cidr" Data.=: cidr
      ]

-- | /See:/ 'newDeprovisionPublicIpv4PoolCidrResponse' smart constructor.
data DeprovisionPublicIpv4PoolCidrResponse = DeprovisionPublicIpv4PoolCidrResponse'
  { -- | The deprovisioned CIDRs.
    deprovisionedAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the pool that you deprovisioned the CIDR from.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionPublicIpv4PoolCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprovisionedAddresses', 'deprovisionPublicIpv4PoolCidrResponse_deprovisionedAddresses' - The deprovisioned CIDRs.
--
-- 'poolId', 'deprovisionPublicIpv4PoolCidrResponse_poolId' - The ID of the pool that you deprovisioned the CIDR from.
--
-- 'httpStatus', 'deprovisionPublicIpv4PoolCidrResponse_httpStatus' - The response's http status code.
newDeprovisionPublicIpv4PoolCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprovisionPublicIpv4PoolCidrResponse
newDeprovisionPublicIpv4PoolCidrResponse pHttpStatus_ =
  DeprovisionPublicIpv4PoolCidrResponse'
    { deprovisionedAddresses =
        Prelude.Nothing,
      poolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deprovisioned CIDRs.
deprovisionPublicIpv4PoolCidrResponse_deprovisionedAddresses :: Lens.Lens' DeprovisionPublicIpv4PoolCidrResponse (Prelude.Maybe [Prelude.Text])
deprovisionPublicIpv4PoolCidrResponse_deprovisionedAddresses = Lens.lens (\DeprovisionPublicIpv4PoolCidrResponse' {deprovisionedAddresses} -> deprovisionedAddresses) (\s@DeprovisionPublicIpv4PoolCidrResponse' {} a -> s {deprovisionedAddresses = a} :: DeprovisionPublicIpv4PoolCidrResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the pool that you deprovisioned the CIDR from.
deprovisionPublicIpv4PoolCidrResponse_poolId :: Lens.Lens' DeprovisionPublicIpv4PoolCidrResponse (Prelude.Maybe Prelude.Text)
deprovisionPublicIpv4PoolCidrResponse_poolId = Lens.lens (\DeprovisionPublicIpv4PoolCidrResponse' {poolId} -> poolId) (\s@DeprovisionPublicIpv4PoolCidrResponse' {} a -> s {poolId = a} :: DeprovisionPublicIpv4PoolCidrResponse)

-- | The response's http status code.
deprovisionPublicIpv4PoolCidrResponse_httpStatus :: Lens.Lens' DeprovisionPublicIpv4PoolCidrResponse Prelude.Int
deprovisionPublicIpv4PoolCidrResponse_httpStatus = Lens.lens (\DeprovisionPublicIpv4PoolCidrResponse' {httpStatus} -> httpStatus) (\s@DeprovisionPublicIpv4PoolCidrResponse' {} a -> s {httpStatus = a} :: DeprovisionPublicIpv4PoolCidrResponse)

instance
  Prelude.NFData
    DeprovisionPublicIpv4PoolCidrResponse
  where
  rnf DeprovisionPublicIpv4PoolCidrResponse' {..} =
    Prelude.rnf deprovisionedAddresses
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf httpStatus
