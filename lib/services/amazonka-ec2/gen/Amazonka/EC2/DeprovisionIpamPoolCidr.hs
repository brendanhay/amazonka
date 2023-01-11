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
-- Module      : Amazonka.EC2.DeprovisionIpamPoolCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprovision a CIDR provisioned from an IPAM pool. If you deprovision a
-- CIDR from a pool that has a source pool, the CIDR is recycled back into
-- the source pool. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/depro-pool-cidr-ipam.html Deprovision pool CIDRs>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.DeprovisionIpamPoolCidr
  ( -- * Creating a Request
    DeprovisionIpamPoolCidr (..),
    newDeprovisionIpamPoolCidr,

    -- * Request Lenses
    deprovisionIpamPoolCidr_cidr,
    deprovisionIpamPoolCidr_dryRun,
    deprovisionIpamPoolCidr_ipamPoolId,

    -- * Destructuring the Response
    DeprovisionIpamPoolCidrResponse (..),
    newDeprovisionIpamPoolCidrResponse,

    -- * Response Lenses
    deprovisionIpamPoolCidrResponse_ipamPoolCidr,
    deprovisionIpamPoolCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeprovisionIpamPoolCidr' smart constructor.
data DeprovisionIpamPoolCidr = DeprovisionIpamPoolCidr'
  { -- | The CIDR which you want to deprovision from the pool.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pool that has the CIDR you want to deprovision.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionIpamPoolCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'deprovisionIpamPoolCidr_cidr' - The CIDR which you want to deprovision from the pool.
--
-- 'dryRun', 'deprovisionIpamPoolCidr_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamPoolId', 'deprovisionIpamPoolCidr_ipamPoolId' - The ID of the pool that has the CIDR you want to deprovision.
newDeprovisionIpamPoolCidr ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  DeprovisionIpamPoolCidr
newDeprovisionIpamPoolCidr pIpamPoolId_ =
  DeprovisionIpamPoolCidr'
    { cidr = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | The CIDR which you want to deprovision from the pool.
deprovisionIpamPoolCidr_cidr :: Lens.Lens' DeprovisionIpamPoolCidr (Prelude.Maybe Prelude.Text)
deprovisionIpamPoolCidr_cidr = Lens.lens (\DeprovisionIpamPoolCidr' {cidr} -> cidr) (\s@DeprovisionIpamPoolCidr' {} a -> s {cidr = a} :: DeprovisionIpamPoolCidr)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deprovisionIpamPoolCidr_dryRun :: Lens.Lens' DeprovisionIpamPoolCidr (Prelude.Maybe Prelude.Bool)
deprovisionIpamPoolCidr_dryRun = Lens.lens (\DeprovisionIpamPoolCidr' {dryRun} -> dryRun) (\s@DeprovisionIpamPoolCidr' {} a -> s {dryRun = a} :: DeprovisionIpamPoolCidr)

-- | The ID of the pool that has the CIDR you want to deprovision.
deprovisionIpamPoolCidr_ipamPoolId :: Lens.Lens' DeprovisionIpamPoolCidr Prelude.Text
deprovisionIpamPoolCidr_ipamPoolId = Lens.lens (\DeprovisionIpamPoolCidr' {ipamPoolId} -> ipamPoolId) (\s@DeprovisionIpamPoolCidr' {} a -> s {ipamPoolId = a} :: DeprovisionIpamPoolCidr)

instance Core.AWSRequest DeprovisionIpamPoolCidr where
  type
    AWSResponse DeprovisionIpamPoolCidr =
      DeprovisionIpamPoolCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeprovisionIpamPoolCidrResponse'
            Prelude.<$> (x Data..@? "ipamPoolCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeprovisionIpamPoolCidr where
  hashWithSalt _salt DeprovisionIpamPoolCidr' {..} =
    _salt `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData DeprovisionIpamPoolCidr where
  rnf DeprovisionIpamPoolCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamPoolId

instance Data.ToHeaders DeprovisionIpamPoolCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeprovisionIpamPoolCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery DeprovisionIpamPoolCidr where
  toQuery DeprovisionIpamPoolCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeprovisionIpamPoolCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Cidr" Data.=: cidr,
        "DryRun" Data.=: dryRun,
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newDeprovisionIpamPoolCidrResponse' smart constructor.
data DeprovisionIpamPoolCidrResponse = DeprovisionIpamPoolCidrResponse'
  { -- | The deprovisioned pool CIDR.
    ipamPoolCidr :: Prelude.Maybe IpamPoolCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprovisionIpamPoolCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPoolCidr', 'deprovisionIpamPoolCidrResponse_ipamPoolCidr' - The deprovisioned pool CIDR.
--
-- 'httpStatus', 'deprovisionIpamPoolCidrResponse_httpStatus' - The response's http status code.
newDeprovisionIpamPoolCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprovisionIpamPoolCidrResponse
newDeprovisionIpamPoolCidrResponse pHttpStatus_ =
  DeprovisionIpamPoolCidrResponse'
    { ipamPoolCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deprovisioned pool CIDR.
deprovisionIpamPoolCidrResponse_ipamPoolCidr :: Lens.Lens' DeprovisionIpamPoolCidrResponse (Prelude.Maybe IpamPoolCidr)
deprovisionIpamPoolCidrResponse_ipamPoolCidr = Lens.lens (\DeprovisionIpamPoolCidrResponse' {ipamPoolCidr} -> ipamPoolCidr) (\s@DeprovisionIpamPoolCidrResponse' {} a -> s {ipamPoolCidr = a} :: DeprovisionIpamPoolCidrResponse)

-- | The response's http status code.
deprovisionIpamPoolCidrResponse_httpStatus :: Lens.Lens' DeprovisionIpamPoolCidrResponse Prelude.Int
deprovisionIpamPoolCidrResponse_httpStatus = Lens.lens (\DeprovisionIpamPoolCidrResponse' {httpStatus} -> httpStatus) (\s@DeprovisionIpamPoolCidrResponse' {} a -> s {httpStatus = a} :: DeprovisionIpamPoolCidrResponse)

instance
  Prelude.NFData
    DeprovisionIpamPoolCidrResponse
  where
  rnf DeprovisionIpamPoolCidrResponse' {..} =
    Prelude.rnf ipamPoolCidr
      `Prelude.seq` Prelude.rnf httpStatus
