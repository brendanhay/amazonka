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
-- Module      : Amazonka.EC2.CreatePublicIpv4Pool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public IPv4 address pool. A public IPv4 pool is an EC2 IP
-- address pool required for the public IPv4 CIDRs that you own and bring
-- to Amazon Web Services to manage with IPAM. IPv6 addresses you bring to
-- Amazon Web Services, however, use IPAM pools only. To monitor the status
-- of pool creation, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribePublicIpv4Pools.html DescribePublicIpv4Pools>.
module Amazonka.EC2.CreatePublicIpv4Pool
  ( -- * Creating a Request
    CreatePublicIpv4Pool (..),
    newCreatePublicIpv4Pool,

    -- * Request Lenses
    createPublicIpv4Pool_dryRun,
    createPublicIpv4Pool_tagSpecifications,

    -- * Destructuring the Response
    CreatePublicIpv4PoolResponse (..),
    newCreatePublicIpv4PoolResponse,

    -- * Response Lenses
    createPublicIpv4PoolResponse_poolId,
    createPublicIpv4PoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePublicIpv4Pool' smart constructor.
data CreatePublicIpv4Pool = CreatePublicIpv4Pool'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicIpv4Pool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createPublicIpv4Pool_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createPublicIpv4Pool_tagSpecifications' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
newCreatePublicIpv4Pool ::
  CreatePublicIpv4Pool
newCreatePublicIpv4Pool =
  CreatePublicIpv4Pool'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
createPublicIpv4Pool_dryRun :: Lens.Lens' CreatePublicIpv4Pool (Prelude.Maybe Prelude.Bool)
createPublicIpv4Pool_dryRun = Lens.lens (\CreatePublicIpv4Pool' {dryRun} -> dryRun) (\s@CreatePublicIpv4Pool' {} a -> s {dryRun = a} :: CreatePublicIpv4Pool)

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
createPublicIpv4Pool_tagSpecifications :: Lens.Lens' CreatePublicIpv4Pool (Prelude.Maybe [TagSpecification])
createPublicIpv4Pool_tagSpecifications = Lens.lens (\CreatePublicIpv4Pool' {tagSpecifications} -> tagSpecifications) (\s@CreatePublicIpv4Pool' {} a -> s {tagSpecifications = a} :: CreatePublicIpv4Pool) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreatePublicIpv4Pool where
  type
    AWSResponse CreatePublicIpv4Pool =
      CreatePublicIpv4PoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePublicIpv4PoolResponse'
            Prelude.<$> (x Data..@? "poolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePublicIpv4Pool where
  hashWithSalt _salt CreatePublicIpv4Pool' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreatePublicIpv4Pool where
  rnf CreatePublicIpv4Pool' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreatePublicIpv4Pool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePublicIpv4Pool where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePublicIpv4Pool where
  toQuery CreatePublicIpv4Pool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePublicIpv4Pool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreatePublicIpv4PoolResponse' smart constructor.
data CreatePublicIpv4PoolResponse = CreatePublicIpv4PoolResponse'
  { -- | The ID of the public IPv4 pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicIpv4PoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolId', 'createPublicIpv4PoolResponse_poolId' - The ID of the public IPv4 pool.
--
-- 'httpStatus', 'createPublicIpv4PoolResponse_httpStatus' - The response's http status code.
newCreatePublicIpv4PoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePublicIpv4PoolResponse
newCreatePublicIpv4PoolResponse pHttpStatus_ =
  CreatePublicIpv4PoolResponse'
    { poolId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the public IPv4 pool.
createPublicIpv4PoolResponse_poolId :: Lens.Lens' CreatePublicIpv4PoolResponse (Prelude.Maybe Prelude.Text)
createPublicIpv4PoolResponse_poolId = Lens.lens (\CreatePublicIpv4PoolResponse' {poolId} -> poolId) (\s@CreatePublicIpv4PoolResponse' {} a -> s {poolId = a} :: CreatePublicIpv4PoolResponse)

-- | The response's http status code.
createPublicIpv4PoolResponse_httpStatus :: Lens.Lens' CreatePublicIpv4PoolResponse Prelude.Int
createPublicIpv4PoolResponse_httpStatus = Lens.lens (\CreatePublicIpv4PoolResponse' {httpStatus} -> httpStatus) (\s@CreatePublicIpv4PoolResponse' {} a -> s {httpStatus = a} :: CreatePublicIpv4PoolResponse)

instance Prelude.NFData CreatePublicIpv4PoolResponse where
  rnf CreatePublicIpv4PoolResponse' {..} =
    Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf httpStatus
