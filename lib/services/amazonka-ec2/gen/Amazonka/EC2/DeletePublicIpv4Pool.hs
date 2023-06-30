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
-- Module      : Amazonka.EC2.DeletePublicIpv4Pool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a public IPv4 pool. A public IPv4 pool is an EC2 IP address pool
-- required for the public IPv4 CIDRs that you own and bring to Amazon Web
-- Services to manage with IPAM. IPv6 addresses you bring to Amazon Web
-- Services, however, use IPAM pools only.
module Amazonka.EC2.DeletePublicIpv4Pool
  ( -- * Creating a Request
    DeletePublicIpv4Pool (..),
    newDeletePublicIpv4Pool,

    -- * Request Lenses
    deletePublicIpv4Pool_dryRun,
    deletePublicIpv4Pool_poolId,

    -- * Destructuring the Response
    DeletePublicIpv4PoolResponse (..),
    newDeletePublicIpv4PoolResponse,

    -- * Response Lenses
    deletePublicIpv4PoolResponse_returnValue,
    deletePublicIpv4PoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePublicIpv4Pool' smart constructor.
data DeletePublicIpv4Pool = DeletePublicIpv4Pool'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the public IPv4 pool you want to delete.
    poolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicIpv4Pool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deletePublicIpv4Pool_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'poolId', 'deletePublicIpv4Pool_poolId' - The ID of the public IPv4 pool you want to delete.
newDeletePublicIpv4Pool ::
  -- | 'poolId'
  Prelude.Text ->
  DeletePublicIpv4Pool
newDeletePublicIpv4Pool pPoolId_ =
  DeletePublicIpv4Pool'
    { dryRun = Prelude.Nothing,
      poolId = pPoolId_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deletePublicIpv4Pool_dryRun :: Lens.Lens' DeletePublicIpv4Pool (Prelude.Maybe Prelude.Bool)
deletePublicIpv4Pool_dryRun = Lens.lens (\DeletePublicIpv4Pool' {dryRun} -> dryRun) (\s@DeletePublicIpv4Pool' {} a -> s {dryRun = a} :: DeletePublicIpv4Pool)

-- | The ID of the public IPv4 pool you want to delete.
deletePublicIpv4Pool_poolId :: Lens.Lens' DeletePublicIpv4Pool Prelude.Text
deletePublicIpv4Pool_poolId = Lens.lens (\DeletePublicIpv4Pool' {poolId} -> poolId) (\s@DeletePublicIpv4Pool' {} a -> s {poolId = a} :: DeletePublicIpv4Pool)

instance Core.AWSRequest DeletePublicIpv4Pool where
  type
    AWSResponse DeletePublicIpv4Pool =
      DeletePublicIpv4PoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeletePublicIpv4PoolResponse'
            Prelude.<$> (x Data..@? "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePublicIpv4Pool where
  hashWithSalt _salt DeletePublicIpv4Pool' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` poolId

instance Prelude.NFData DeletePublicIpv4Pool where
  rnf DeletePublicIpv4Pool' {..} =
    Prelude.rnf dryRun `Prelude.seq` Prelude.rnf poolId

instance Data.ToHeaders DeletePublicIpv4Pool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePublicIpv4Pool where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePublicIpv4Pool where
  toQuery DeletePublicIpv4Pool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeletePublicIpv4Pool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PoolId" Data.=: poolId
      ]

-- | /See:/ 'newDeletePublicIpv4PoolResponse' smart constructor.
data DeletePublicIpv4PoolResponse = DeletePublicIpv4PoolResponse'
  { -- | Information about the result of deleting the public IPv4 pool.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicIpv4PoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'deletePublicIpv4PoolResponse_returnValue' - Information about the result of deleting the public IPv4 pool.
--
-- 'httpStatus', 'deletePublicIpv4PoolResponse_httpStatus' - The response's http status code.
newDeletePublicIpv4PoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePublicIpv4PoolResponse
newDeletePublicIpv4PoolResponse pHttpStatus_ =
  DeletePublicIpv4PoolResponse'
    { returnValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of deleting the public IPv4 pool.
deletePublicIpv4PoolResponse_returnValue :: Lens.Lens' DeletePublicIpv4PoolResponse (Prelude.Maybe Prelude.Bool)
deletePublicIpv4PoolResponse_returnValue = Lens.lens (\DeletePublicIpv4PoolResponse' {returnValue} -> returnValue) (\s@DeletePublicIpv4PoolResponse' {} a -> s {returnValue = a} :: DeletePublicIpv4PoolResponse)

-- | The response's http status code.
deletePublicIpv4PoolResponse_httpStatus :: Lens.Lens' DeletePublicIpv4PoolResponse Prelude.Int
deletePublicIpv4PoolResponse_httpStatus = Lens.lens (\DeletePublicIpv4PoolResponse' {httpStatus} -> httpStatus) (\s@DeletePublicIpv4PoolResponse' {} a -> s {httpStatus = a} :: DeletePublicIpv4PoolResponse)

instance Prelude.NFData DeletePublicIpv4PoolResponse where
  rnf DeletePublicIpv4PoolResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
