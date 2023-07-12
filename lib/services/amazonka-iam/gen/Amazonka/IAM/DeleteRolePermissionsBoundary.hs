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
-- Module      : Amazonka.IAM.DeleteRolePermissionsBoundary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM role.
--
-- Deleting the permissions boundary for a role might increase its
-- permissions. For example, it might allow anyone who assumes the role to
-- perform all the actions granted in its permissions policies.
module Amazonka.IAM.DeleteRolePermissionsBoundary
  ( -- * Creating a Request
    DeleteRolePermissionsBoundary (..),
    newDeleteRolePermissionsBoundary,

    -- * Request Lenses
    deleteRolePermissionsBoundary_roleName,

    -- * Destructuring the Response
    DeleteRolePermissionsBoundaryResponse (..),
    newDeleteRolePermissionsBoundaryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRolePermissionsBoundary' smart constructor.
data DeleteRolePermissionsBoundary = DeleteRolePermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM role from which you want to
    -- remove the permissions boundary.
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRolePermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'deleteRolePermissionsBoundary_roleName' - The name (friendly name, not ARN) of the IAM role from which you want to
-- remove the permissions boundary.
newDeleteRolePermissionsBoundary ::
  -- | 'roleName'
  Prelude.Text ->
  DeleteRolePermissionsBoundary
newDeleteRolePermissionsBoundary pRoleName_ =
  DeleteRolePermissionsBoundary'
    { roleName =
        pRoleName_
    }

-- | The name (friendly name, not ARN) of the IAM role from which you want to
-- remove the permissions boundary.
deleteRolePermissionsBoundary_roleName :: Lens.Lens' DeleteRolePermissionsBoundary Prelude.Text
deleteRolePermissionsBoundary_roleName = Lens.lens (\DeleteRolePermissionsBoundary' {roleName} -> roleName) (\s@DeleteRolePermissionsBoundary' {} a -> s {roleName = a} :: DeleteRolePermissionsBoundary)

instance
  Core.AWSRequest
    DeleteRolePermissionsBoundary
  where
  type
    AWSResponse DeleteRolePermissionsBoundary =
      DeleteRolePermissionsBoundaryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRolePermissionsBoundaryResponse'

instance
  Prelude.Hashable
    DeleteRolePermissionsBoundary
  where
  hashWithSalt _salt DeleteRolePermissionsBoundary' {..} =
    _salt `Prelude.hashWithSalt` roleName

instance Prelude.NFData DeleteRolePermissionsBoundary where
  rnf DeleteRolePermissionsBoundary' {..} =
    Prelude.rnf roleName

instance Data.ToHeaders DeleteRolePermissionsBoundary where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteRolePermissionsBoundary where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRolePermissionsBoundary where
  toQuery DeleteRolePermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteRolePermissionsBoundary" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName
      ]

-- | /See:/ 'newDeleteRolePermissionsBoundaryResponse' smart constructor.
data DeleteRolePermissionsBoundaryResponse = DeleteRolePermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRolePermissionsBoundaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRolePermissionsBoundaryResponse ::
  DeleteRolePermissionsBoundaryResponse
newDeleteRolePermissionsBoundaryResponse =
  DeleteRolePermissionsBoundaryResponse'

instance
  Prelude.NFData
    DeleteRolePermissionsBoundaryResponse
  where
  rnf _ = ()
