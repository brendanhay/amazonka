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
-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified role. The role must not have any policies
-- attached. For more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with roles>.
--
-- Make sure that you do not have any Amazon EC2 instances running with the
-- role you are about to delete. Deleting a role or instance profile that
-- is associated with a running instance will break any applications
-- running on the instance.
module Network.AWS.IAM.DeleteRole
  ( -- * Creating a Request
    DeleteRole (..),
    newDeleteRole,

    -- * Request Lenses
    deleteRole_roleName,

    -- * Destructuring the Response
    DeleteRoleResponse (..),
    newDeleteRoleResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRole' smart constructor.
data DeleteRole = DeleteRole'
  { -- | The name of the role to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'deleteRole_roleName' - The name of the role to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteRole ::
  -- | 'roleName'
  Prelude.Text ->
  DeleteRole
newDeleteRole pRoleName_ =
  DeleteRole' {roleName = pRoleName_}

-- | The name of the role to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteRole_roleName :: Lens.Lens' DeleteRole Prelude.Text
deleteRole_roleName = Lens.lens (\DeleteRole' {roleName} -> roleName) (\s@DeleteRole' {} a -> s {roleName = a} :: DeleteRole)

instance Prelude.AWSRequest DeleteRole where
  type Rs DeleteRole = DeleteRoleResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteRoleResponse'

instance Prelude.Hashable DeleteRole

instance Prelude.NFData DeleteRole

instance Prelude.ToHeaders DeleteRole where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRole where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRole where
  toQuery DeleteRole' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteRole" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName
      ]

-- | /See:/ 'newDeleteRoleResponse' smart constructor.
data DeleteRoleResponse = DeleteRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRoleResponse ::
  DeleteRoleResponse
newDeleteRoleResponse = DeleteRoleResponse'

instance Prelude.NFData DeleteRoleResponse
