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
-- Module      : Amazonka.EC2.DeleteIpamScope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the scope for an IPAM. You cannot delete the default scopes.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/delete-scope-ipam.html Delete a scope>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.DeleteIpamScope
  ( -- * Creating a Request
    DeleteIpamScope (..),
    newDeleteIpamScope,

    -- * Request Lenses
    deleteIpamScope_dryRun,
    deleteIpamScope_ipamScopeId,

    -- * Destructuring the Response
    DeleteIpamScopeResponse (..),
    newDeleteIpamScopeResponse,

    -- * Response Lenses
    deleteIpamScopeResponse_ipamScope,
    deleteIpamScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIpamScope' smart constructor.
data DeleteIpamScope = DeleteIpamScope'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the scope to delete.
    ipamScopeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteIpamScope_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamScopeId', 'deleteIpamScope_ipamScopeId' - The ID of the scope to delete.
newDeleteIpamScope ::
  -- | 'ipamScopeId'
  Prelude.Text ->
  DeleteIpamScope
newDeleteIpamScope pIpamScopeId_ =
  DeleteIpamScope'
    { dryRun = Prelude.Nothing,
      ipamScopeId = pIpamScopeId_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deleteIpamScope_dryRun :: Lens.Lens' DeleteIpamScope (Prelude.Maybe Prelude.Bool)
deleteIpamScope_dryRun = Lens.lens (\DeleteIpamScope' {dryRun} -> dryRun) (\s@DeleteIpamScope' {} a -> s {dryRun = a} :: DeleteIpamScope)

-- | The ID of the scope to delete.
deleteIpamScope_ipamScopeId :: Lens.Lens' DeleteIpamScope Prelude.Text
deleteIpamScope_ipamScopeId = Lens.lens (\DeleteIpamScope' {ipamScopeId} -> ipamScopeId) (\s@DeleteIpamScope' {} a -> s {ipamScopeId = a} :: DeleteIpamScope)

instance Core.AWSRequest DeleteIpamScope where
  type
    AWSResponse DeleteIpamScope =
      DeleteIpamScopeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteIpamScopeResponse'
            Prelude.<$> (x Data..@? "ipamScope")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpamScope where
  hashWithSalt _salt DeleteIpamScope' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamScopeId

instance Prelude.NFData DeleteIpamScope where
  rnf DeleteIpamScope' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamScopeId

instance Data.ToHeaders DeleteIpamScope where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteIpamScope where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIpamScope where
  toQuery DeleteIpamScope' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteIpamScope" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamScopeId" Data.=: ipamScopeId
      ]

-- | /See:/ 'newDeleteIpamScopeResponse' smart constructor.
data DeleteIpamScopeResponse = DeleteIpamScopeResponse'
  { -- | Information about the results of the deletion.
    ipamScope :: Prelude.Maybe IpamScope,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamScope', 'deleteIpamScopeResponse_ipamScope' - Information about the results of the deletion.
--
-- 'httpStatus', 'deleteIpamScopeResponse_httpStatus' - The response's http status code.
newDeleteIpamScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpamScopeResponse
newDeleteIpamScopeResponse pHttpStatus_ =
  DeleteIpamScopeResponse'
    { ipamScope =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the results of the deletion.
deleteIpamScopeResponse_ipamScope :: Lens.Lens' DeleteIpamScopeResponse (Prelude.Maybe IpamScope)
deleteIpamScopeResponse_ipamScope = Lens.lens (\DeleteIpamScopeResponse' {ipamScope} -> ipamScope) (\s@DeleteIpamScopeResponse' {} a -> s {ipamScope = a} :: DeleteIpamScopeResponse)

-- | The response's http status code.
deleteIpamScopeResponse_httpStatus :: Lens.Lens' DeleteIpamScopeResponse Prelude.Int
deleteIpamScopeResponse_httpStatus = Lens.lens (\DeleteIpamScopeResponse' {httpStatus} -> httpStatus) (\s@DeleteIpamScopeResponse' {} a -> s {httpStatus = a} :: DeleteIpamScopeResponse)

instance Prelude.NFData DeleteIpamScopeResponse where
  rnf DeleteIpamScopeResponse' {..} =
    Prelude.rnf ipamScope
      `Prelude.seq` Prelude.rnf httpStatus
