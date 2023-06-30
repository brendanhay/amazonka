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
-- Module      : Amazonka.EC2.ProvisionIpamPoolCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provision a CIDR to an IPAM pool. You can use this action to provision
-- new CIDRs to a top-level pool or to transfer a CIDR from a top-level
-- pool to a pool within it.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/prov-cidr-ipam.html Provision CIDRs to pools>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.ProvisionIpamPoolCidr
  ( -- * Creating a Request
    ProvisionIpamPoolCidr (..),
    newProvisionIpamPoolCidr,

    -- * Request Lenses
    provisionIpamPoolCidr_cidr,
    provisionIpamPoolCidr_cidrAuthorizationContext,
    provisionIpamPoolCidr_dryRun,
    provisionIpamPoolCidr_ipamPoolId,

    -- * Destructuring the Response
    ProvisionIpamPoolCidrResponse (..),
    newProvisionIpamPoolCidrResponse,

    -- * Response Lenses
    provisionIpamPoolCidrResponse_ipamPoolCidr,
    provisionIpamPoolCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newProvisionIpamPoolCidr' smart constructor.
data ProvisionIpamPoolCidr = ProvisionIpamPoolCidr'
  { -- | The CIDR you want to assign to the IPAM pool.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | A signed document that proves that you are authorized to bring a
    -- specified IP address range to Amazon using BYOIP. This option applies to
    -- public pools only.
    cidrAuthorizationContext :: Prelude.Maybe IpamCidrAuthorizationContext,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the IPAM pool to which you want to assign a CIDR.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionIpamPoolCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'provisionIpamPoolCidr_cidr' - The CIDR you want to assign to the IPAM pool.
--
-- 'cidrAuthorizationContext', 'provisionIpamPoolCidr_cidrAuthorizationContext' - A signed document that proves that you are authorized to bring a
-- specified IP address range to Amazon using BYOIP. This option applies to
-- public pools only.
--
-- 'dryRun', 'provisionIpamPoolCidr_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamPoolId', 'provisionIpamPoolCidr_ipamPoolId' - The ID of the IPAM pool to which you want to assign a CIDR.
newProvisionIpamPoolCidr ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  ProvisionIpamPoolCidr
newProvisionIpamPoolCidr pIpamPoolId_ =
  ProvisionIpamPoolCidr'
    { cidr = Prelude.Nothing,
      cidrAuthorizationContext = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | The CIDR you want to assign to the IPAM pool.
provisionIpamPoolCidr_cidr :: Lens.Lens' ProvisionIpamPoolCidr (Prelude.Maybe Prelude.Text)
provisionIpamPoolCidr_cidr = Lens.lens (\ProvisionIpamPoolCidr' {cidr} -> cidr) (\s@ProvisionIpamPoolCidr' {} a -> s {cidr = a} :: ProvisionIpamPoolCidr)

-- | A signed document that proves that you are authorized to bring a
-- specified IP address range to Amazon using BYOIP. This option applies to
-- public pools only.
provisionIpamPoolCidr_cidrAuthorizationContext :: Lens.Lens' ProvisionIpamPoolCidr (Prelude.Maybe IpamCidrAuthorizationContext)
provisionIpamPoolCidr_cidrAuthorizationContext = Lens.lens (\ProvisionIpamPoolCidr' {cidrAuthorizationContext} -> cidrAuthorizationContext) (\s@ProvisionIpamPoolCidr' {} a -> s {cidrAuthorizationContext = a} :: ProvisionIpamPoolCidr)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
provisionIpamPoolCidr_dryRun :: Lens.Lens' ProvisionIpamPoolCidr (Prelude.Maybe Prelude.Bool)
provisionIpamPoolCidr_dryRun = Lens.lens (\ProvisionIpamPoolCidr' {dryRun} -> dryRun) (\s@ProvisionIpamPoolCidr' {} a -> s {dryRun = a} :: ProvisionIpamPoolCidr)

-- | The ID of the IPAM pool to which you want to assign a CIDR.
provisionIpamPoolCidr_ipamPoolId :: Lens.Lens' ProvisionIpamPoolCidr Prelude.Text
provisionIpamPoolCidr_ipamPoolId = Lens.lens (\ProvisionIpamPoolCidr' {ipamPoolId} -> ipamPoolId) (\s@ProvisionIpamPoolCidr' {} a -> s {ipamPoolId = a} :: ProvisionIpamPoolCidr)

instance Core.AWSRequest ProvisionIpamPoolCidr where
  type
    AWSResponse ProvisionIpamPoolCidr =
      ProvisionIpamPoolCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ProvisionIpamPoolCidrResponse'
            Prelude.<$> (x Data..@? "ipamPoolCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ProvisionIpamPoolCidr where
  hashWithSalt _salt ProvisionIpamPoolCidr' {..} =
    _salt
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` cidrAuthorizationContext
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData ProvisionIpamPoolCidr where
  rnf ProvisionIpamPoolCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf cidrAuthorizationContext
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamPoolId

instance Data.ToHeaders ProvisionIpamPoolCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ProvisionIpamPoolCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery ProvisionIpamPoolCidr where
  toQuery ProvisionIpamPoolCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ProvisionIpamPoolCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Cidr" Data.=: cidr,
        "CidrAuthorizationContext"
          Data.=: cidrAuthorizationContext,
        "DryRun" Data.=: dryRun,
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newProvisionIpamPoolCidrResponse' smart constructor.
data ProvisionIpamPoolCidrResponse = ProvisionIpamPoolCidrResponse'
  { -- | Information about the provisioned CIDR.
    ipamPoolCidr :: Prelude.Maybe IpamPoolCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionIpamPoolCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPoolCidr', 'provisionIpamPoolCidrResponse_ipamPoolCidr' - Information about the provisioned CIDR.
--
-- 'httpStatus', 'provisionIpamPoolCidrResponse_httpStatus' - The response's http status code.
newProvisionIpamPoolCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ProvisionIpamPoolCidrResponse
newProvisionIpamPoolCidrResponse pHttpStatus_ =
  ProvisionIpamPoolCidrResponse'
    { ipamPoolCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioned CIDR.
provisionIpamPoolCidrResponse_ipamPoolCidr :: Lens.Lens' ProvisionIpamPoolCidrResponse (Prelude.Maybe IpamPoolCidr)
provisionIpamPoolCidrResponse_ipamPoolCidr = Lens.lens (\ProvisionIpamPoolCidrResponse' {ipamPoolCidr} -> ipamPoolCidr) (\s@ProvisionIpamPoolCidrResponse' {} a -> s {ipamPoolCidr = a} :: ProvisionIpamPoolCidrResponse)

-- | The response's http status code.
provisionIpamPoolCidrResponse_httpStatus :: Lens.Lens' ProvisionIpamPoolCidrResponse Prelude.Int
provisionIpamPoolCidrResponse_httpStatus = Lens.lens (\ProvisionIpamPoolCidrResponse' {httpStatus} -> httpStatus) (\s@ProvisionIpamPoolCidrResponse' {} a -> s {httpStatus = a} :: ProvisionIpamPoolCidrResponse)

instance Prelude.NFData ProvisionIpamPoolCidrResponse where
  rnf ProvisionIpamPoolCidrResponse' {..} =
    Prelude.rnf ipamPoolCidr
      `Prelude.seq` Prelude.rnf httpStatus
