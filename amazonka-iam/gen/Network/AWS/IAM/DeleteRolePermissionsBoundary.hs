{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.DeleteRolePermissionsBoundary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM role.
--
-- Deleting the permissions boundary for a role might increase its
-- permissions. For example, it might allow anyone who assumes the role to
-- perform all the actions granted in its permissions policies.
module Network.AWS.IAM.DeleteRolePermissionsBoundary
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRolePermissionsBoundary' smart constructor.
data DeleteRolePermissionsBoundary = DeleteRolePermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM role from which you want to
    -- remove the permissions boundary.
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteRolePermissionsBoundary
  where
  type
    Rs DeleteRolePermissionsBoundary =
      DeleteRolePermissionsBoundaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteRolePermissionsBoundaryResponse'

instance
  Prelude.Hashable
    DeleteRolePermissionsBoundary

instance Prelude.NFData DeleteRolePermissionsBoundary

instance
  Prelude.ToHeaders
    DeleteRolePermissionsBoundary
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRolePermissionsBoundary where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteRolePermissionsBoundary
  where
  toQuery DeleteRolePermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteRolePermissionsBoundary" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName
      ]

-- | /See:/ 'newDeleteRolePermissionsBoundaryResponse' smart constructor.
data DeleteRolePermissionsBoundaryResponse = DeleteRolePermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
