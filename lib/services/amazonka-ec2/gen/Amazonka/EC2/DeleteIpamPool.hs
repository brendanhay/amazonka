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
-- Module      : Amazonka.EC2.DeleteIpamPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an IPAM pool.
--
-- You cannot delete an IPAM pool if there are allocations in it or CIDRs
-- provisioned to it. To release allocations, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReleaseIpamPoolAllocation.html ReleaseIpamPoolAllocation>.
-- To deprovision pool CIDRs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeprovisionIpamPoolCidr.html DeprovisionIpamPoolCidr>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/delete-pool-ipam.html Delete a pool>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.DeleteIpamPool
  ( -- * Creating a Request
    DeleteIpamPool (..),
    newDeleteIpamPool,

    -- * Request Lenses
    deleteIpamPool_dryRun,
    deleteIpamPool_ipamPoolId,

    -- * Destructuring the Response
    DeleteIpamPoolResponse (..),
    newDeleteIpamPoolResponse,

    -- * Response Lenses
    deleteIpamPoolResponse_ipamPool,
    deleteIpamPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIpamPool' smart constructor.
data DeleteIpamPool = DeleteIpamPool'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pool to delete.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteIpamPool_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamPoolId', 'deleteIpamPool_ipamPoolId' - The ID of the pool to delete.
newDeleteIpamPool ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  DeleteIpamPool
newDeleteIpamPool pIpamPoolId_ =
  DeleteIpamPool'
    { dryRun = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deleteIpamPool_dryRun :: Lens.Lens' DeleteIpamPool (Prelude.Maybe Prelude.Bool)
deleteIpamPool_dryRun = Lens.lens (\DeleteIpamPool' {dryRun} -> dryRun) (\s@DeleteIpamPool' {} a -> s {dryRun = a} :: DeleteIpamPool)

-- | The ID of the pool to delete.
deleteIpamPool_ipamPoolId :: Lens.Lens' DeleteIpamPool Prelude.Text
deleteIpamPool_ipamPoolId = Lens.lens (\DeleteIpamPool' {ipamPoolId} -> ipamPoolId) (\s@DeleteIpamPool' {} a -> s {ipamPoolId = a} :: DeleteIpamPool)

instance Core.AWSRequest DeleteIpamPool where
  type
    AWSResponse DeleteIpamPool =
      DeleteIpamPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteIpamPoolResponse'
            Prelude.<$> (x Data..@? "ipamPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpamPool where
  hashWithSalt _salt DeleteIpamPool' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData DeleteIpamPool where
  rnf DeleteIpamPool' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf ipamPoolId

instance Data.ToHeaders DeleteIpamPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteIpamPool where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIpamPool where
  toQuery DeleteIpamPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteIpamPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newDeleteIpamPoolResponse' smart constructor.
data DeleteIpamPoolResponse = DeleteIpamPoolResponse'
  { -- | Information about the results of the deletion.
    ipamPool :: Prelude.Maybe IpamPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPool', 'deleteIpamPoolResponse_ipamPool' - Information about the results of the deletion.
--
-- 'httpStatus', 'deleteIpamPoolResponse_httpStatus' - The response's http status code.
newDeleteIpamPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpamPoolResponse
newDeleteIpamPoolResponse pHttpStatus_ =
  DeleteIpamPoolResponse'
    { ipamPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the results of the deletion.
deleteIpamPoolResponse_ipamPool :: Lens.Lens' DeleteIpamPoolResponse (Prelude.Maybe IpamPool)
deleteIpamPoolResponse_ipamPool = Lens.lens (\DeleteIpamPoolResponse' {ipamPool} -> ipamPool) (\s@DeleteIpamPoolResponse' {} a -> s {ipamPool = a} :: DeleteIpamPoolResponse)

-- | The response's http status code.
deleteIpamPoolResponse_httpStatus :: Lens.Lens' DeleteIpamPoolResponse Prelude.Int
deleteIpamPoolResponse_httpStatus = Lens.lens (\DeleteIpamPoolResponse' {httpStatus} -> httpStatus) (\s@DeleteIpamPoolResponse' {} a -> s {httpStatus = a} :: DeleteIpamPoolResponse)

instance Prelude.NFData DeleteIpamPoolResponse where
  rnf DeleteIpamPoolResponse' {..} =
    Prelude.rnf ipamPool `Prelude.seq`
      Prelude.rnf httpStatus
