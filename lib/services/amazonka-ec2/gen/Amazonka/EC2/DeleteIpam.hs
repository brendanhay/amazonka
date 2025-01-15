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
-- Module      : Amazonka.EC2.DeleteIpam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an IPAM. Deleting an IPAM removes all monitored data associated
-- with the IPAM including the historical data for CIDRs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/delete-ipam.html Delete an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.DeleteIpam
  ( -- * Creating a Request
    DeleteIpam (..),
    newDeleteIpam,

    -- * Request Lenses
    deleteIpam_cascade,
    deleteIpam_dryRun,
    deleteIpam_ipamId,

    -- * Destructuring the Response
    DeleteIpamResponse (..),
    newDeleteIpamResponse,

    -- * Response Lenses
    deleteIpamResponse_ipam,
    deleteIpamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIpam' smart constructor.
data DeleteIpam = DeleteIpam'
  { -- | Enables you to quickly delete an IPAM, private scopes, pools in private
    -- scopes, and any allocations in the pools in private scopes. You cannot
    -- delete the IPAM with this option if there is a pool in your public
    -- scope. If you use this option, IPAM does the following:
    --
    -- -   Deallocates any CIDRs allocated to VPC resources (such as VPCs) in
    --     pools in private scopes.
    --
    --     No VPC resources are deleted as a result of enabling this option.
    --     The CIDR associated with the resource will no longer be allocated
    --     from an IPAM pool, but the CIDR itself will remain unchanged.
    --
    -- -   Deprovisions all IPv4 CIDRs provisioned to IPAM pools in private
    --     scopes.
    --
    -- -   Deletes all IPAM pools in private scopes.
    --
    -- -   Deletes all non-default private scopes in the IPAM.
    --
    -- -   Deletes the default public and private scopes and the IPAM.
    cascade :: Prelude.Maybe Prelude.Bool,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the IPAM to delete.
    ipamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cascade', 'deleteIpam_cascade' - Enables you to quickly delete an IPAM, private scopes, pools in private
-- scopes, and any allocations in the pools in private scopes. You cannot
-- delete the IPAM with this option if there is a pool in your public
-- scope. If you use this option, IPAM does the following:
--
-- -   Deallocates any CIDRs allocated to VPC resources (such as VPCs) in
--     pools in private scopes.
--
--     No VPC resources are deleted as a result of enabling this option.
--     The CIDR associated with the resource will no longer be allocated
--     from an IPAM pool, but the CIDR itself will remain unchanged.
--
-- -   Deprovisions all IPv4 CIDRs provisioned to IPAM pools in private
--     scopes.
--
-- -   Deletes all IPAM pools in private scopes.
--
-- -   Deletes all non-default private scopes in the IPAM.
--
-- -   Deletes the default public and private scopes and the IPAM.
--
-- 'dryRun', 'deleteIpam_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamId', 'deleteIpam_ipamId' - The ID of the IPAM to delete.
newDeleteIpam ::
  -- | 'ipamId'
  Prelude.Text ->
  DeleteIpam
newDeleteIpam pIpamId_ =
  DeleteIpam'
    { cascade = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipamId = pIpamId_
    }

-- | Enables you to quickly delete an IPAM, private scopes, pools in private
-- scopes, and any allocations in the pools in private scopes. You cannot
-- delete the IPAM with this option if there is a pool in your public
-- scope. If you use this option, IPAM does the following:
--
-- -   Deallocates any CIDRs allocated to VPC resources (such as VPCs) in
--     pools in private scopes.
--
--     No VPC resources are deleted as a result of enabling this option.
--     The CIDR associated with the resource will no longer be allocated
--     from an IPAM pool, but the CIDR itself will remain unchanged.
--
-- -   Deprovisions all IPv4 CIDRs provisioned to IPAM pools in private
--     scopes.
--
-- -   Deletes all IPAM pools in private scopes.
--
-- -   Deletes all non-default private scopes in the IPAM.
--
-- -   Deletes the default public and private scopes and the IPAM.
deleteIpam_cascade :: Lens.Lens' DeleteIpam (Prelude.Maybe Prelude.Bool)
deleteIpam_cascade = Lens.lens (\DeleteIpam' {cascade} -> cascade) (\s@DeleteIpam' {} a -> s {cascade = a} :: DeleteIpam)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deleteIpam_dryRun :: Lens.Lens' DeleteIpam (Prelude.Maybe Prelude.Bool)
deleteIpam_dryRun = Lens.lens (\DeleteIpam' {dryRun} -> dryRun) (\s@DeleteIpam' {} a -> s {dryRun = a} :: DeleteIpam)

-- | The ID of the IPAM to delete.
deleteIpam_ipamId :: Lens.Lens' DeleteIpam Prelude.Text
deleteIpam_ipamId = Lens.lens (\DeleteIpam' {ipamId} -> ipamId) (\s@DeleteIpam' {} a -> s {ipamId = a} :: DeleteIpam)

instance Core.AWSRequest DeleteIpam where
  type AWSResponse DeleteIpam = DeleteIpamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteIpamResponse'
            Prelude.<$> (x Data..@? "ipam")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpam where
  hashWithSalt _salt DeleteIpam' {..} =
    _salt
      `Prelude.hashWithSalt` cascade
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamId

instance Prelude.NFData DeleteIpam where
  rnf DeleteIpam' {..} =
    Prelude.rnf cascade `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf ipamId

instance Data.ToHeaders DeleteIpam where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteIpam where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIpam where
  toQuery DeleteIpam' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteIpam" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Cascade" Data.=: cascade,
        "DryRun" Data.=: dryRun,
        "IpamId" Data.=: ipamId
      ]

-- | /See:/ 'newDeleteIpamResponse' smart constructor.
data DeleteIpamResponse = DeleteIpamResponse'
  { -- | Information about the results of the deletion.
    ipam :: Prelude.Maybe Ipam,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipam', 'deleteIpamResponse_ipam' - Information about the results of the deletion.
--
-- 'httpStatus', 'deleteIpamResponse_httpStatus' - The response's http status code.
newDeleteIpamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpamResponse
newDeleteIpamResponse pHttpStatus_ =
  DeleteIpamResponse'
    { ipam = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the results of the deletion.
deleteIpamResponse_ipam :: Lens.Lens' DeleteIpamResponse (Prelude.Maybe Ipam)
deleteIpamResponse_ipam = Lens.lens (\DeleteIpamResponse' {ipam} -> ipam) (\s@DeleteIpamResponse' {} a -> s {ipam = a} :: DeleteIpamResponse)

-- | The response's http status code.
deleteIpamResponse_httpStatus :: Lens.Lens' DeleteIpamResponse Prelude.Int
deleteIpamResponse_httpStatus = Lens.lens (\DeleteIpamResponse' {httpStatus} -> httpStatus) (\s@DeleteIpamResponse' {} a -> s {httpStatus = a} :: DeleteIpamResponse)

instance Prelude.NFData DeleteIpamResponse where
  rnf DeleteIpamResponse' {..} =
    Prelude.rnf ipam `Prelude.seq`
      Prelude.rnf httpStatus
