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
-- Module      : Amazonka.WorkMail.DeleteImpersonationRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an impersonation role for the given WorkMail organization.
module Amazonka.WorkMail.DeleteImpersonationRole
  ( -- * Creating a Request
    DeleteImpersonationRole (..),
    newDeleteImpersonationRole,

    -- * Request Lenses
    deleteImpersonationRole_organizationId,
    deleteImpersonationRole_impersonationRoleId,

    -- * Destructuring the Response
    DeleteImpersonationRoleResponse (..),
    newDeleteImpersonationRoleResponse,

    -- * Response Lenses
    deleteImpersonationRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteImpersonationRole' smart constructor.
data DeleteImpersonationRole = DeleteImpersonationRole'
  { -- | The WorkMail organization from which to delete the impersonation role.
    organizationId :: Prelude.Text,
    -- | The ID of the impersonation role to delete.
    impersonationRoleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteImpersonationRole_organizationId' - The WorkMail organization from which to delete the impersonation role.
--
-- 'impersonationRoleId', 'deleteImpersonationRole_impersonationRoleId' - The ID of the impersonation role to delete.
newDeleteImpersonationRole ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'impersonationRoleId'
  Prelude.Text ->
  DeleteImpersonationRole
newDeleteImpersonationRole
  pOrganizationId_
  pImpersonationRoleId_ =
    DeleteImpersonationRole'
      { organizationId =
          pOrganizationId_,
        impersonationRoleId = pImpersonationRoleId_
      }

-- | The WorkMail organization from which to delete the impersonation role.
deleteImpersonationRole_organizationId :: Lens.Lens' DeleteImpersonationRole Prelude.Text
deleteImpersonationRole_organizationId = Lens.lens (\DeleteImpersonationRole' {organizationId} -> organizationId) (\s@DeleteImpersonationRole' {} a -> s {organizationId = a} :: DeleteImpersonationRole)

-- | The ID of the impersonation role to delete.
deleteImpersonationRole_impersonationRoleId :: Lens.Lens' DeleteImpersonationRole Prelude.Text
deleteImpersonationRole_impersonationRoleId = Lens.lens (\DeleteImpersonationRole' {impersonationRoleId} -> impersonationRoleId) (\s@DeleteImpersonationRole' {} a -> s {impersonationRoleId = a} :: DeleteImpersonationRole)

instance Core.AWSRequest DeleteImpersonationRole where
  type
    AWSResponse DeleteImpersonationRole =
      DeleteImpersonationRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImpersonationRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImpersonationRole where
  hashWithSalt _salt DeleteImpersonationRole' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` impersonationRoleId

instance Prelude.NFData DeleteImpersonationRole where
  rnf DeleteImpersonationRole' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf impersonationRoleId

instance Data.ToHeaders DeleteImpersonationRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteImpersonationRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImpersonationRole where
  toJSON DeleteImpersonationRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ("ImpersonationRoleId" Data..= impersonationRoleId)
          ]
      )

instance Data.ToPath DeleteImpersonationRole where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImpersonationRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImpersonationRoleResponse' smart constructor.
data DeleteImpersonationRoleResponse = DeleteImpersonationRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImpersonationRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteImpersonationRoleResponse_httpStatus' - The response's http status code.
newDeleteImpersonationRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImpersonationRoleResponse
newDeleteImpersonationRoleResponse pHttpStatus_ =
  DeleteImpersonationRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteImpersonationRoleResponse_httpStatus :: Lens.Lens' DeleteImpersonationRoleResponse Prelude.Int
deleteImpersonationRoleResponse_httpStatus = Lens.lens (\DeleteImpersonationRoleResponse' {httpStatus} -> httpStatus) (\s@DeleteImpersonationRoleResponse' {} a -> s {httpStatus = a} :: DeleteImpersonationRoleResponse)

instance
  Prelude.NFData
    DeleteImpersonationRoleResponse
  where
  rnf DeleteImpersonationRoleResponse' {..} =
    Prelude.rnf httpStatus
