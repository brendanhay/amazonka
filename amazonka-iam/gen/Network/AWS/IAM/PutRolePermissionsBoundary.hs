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
-- Module      : Network.AWS.IAM.PutRolePermissionsBoundary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM role\'s
-- permissions boundary. You can use an AWS managed policy or a customer
-- managed policy to set the boundary for a role. Use the boundary to
-- control the maximum permissions that the role can have. Setting a
-- permissions boundary is an advanced feature that can affect the
-- permissions for the role.
--
-- You cannot set the boundary for a service-linked role.
--
-- Policies used as permissions boundaries do not provide permissions. You
-- must also attach a permissions policy to the role. To learn how the
-- effective permissions for a role are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON policy evaluation logic>
-- in the IAM User Guide.
module Network.AWS.IAM.PutRolePermissionsBoundary
  ( -- * Creating a Request
    PutRolePermissionsBoundary (..),
    newPutRolePermissionsBoundary,

    -- * Request Lenses
    putRolePermissionsBoundary_roleName,
    putRolePermissionsBoundary_permissionsBoundary,

    -- * Destructuring the Response
    PutRolePermissionsBoundaryResponse (..),
    newPutRolePermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRolePermissionsBoundary' smart constructor.
data PutRolePermissionsBoundary = PutRolePermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM role for which you want to
    -- set the permissions boundary.
    roleName :: Prelude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for
    -- the role.
    permissionsBoundary :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRolePermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'putRolePermissionsBoundary_roleName' - The name (friendly name, not ARN) of the IAM role for which you want to
-- set the permissions boundary.
--
-- 'permissionsBoundary', 'putRolePermissionsBoundary_permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for
-- the role.
newPutRolePermissionsBoundary ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'permissionsBoundary'
  Prelude.Text ->
  PutRolePermissionsBoundary
newPutRolePermissionsBoundary
  pRoleName_
  pPermissionsBoundary_ =
    PutRolePermissionsBoundary'
      { roleName = pRoleName_,
        permissionsBoundary = pPermissionsBoundary_
      }

-- | The name (friendly name, not ARN) of the IAM role for which you want to
-- set the permissions boundary.
putRolePermissionsBoundary_roleName :: Lens.Lens' PutRolePermissionsBoundary Prelude.Text
putRolePermissionsBoundary_roleName = Lens.lens (\PutRolePermissionsBoundary' {roleName} -> roleName) (\s@PutRolePermissionsBoundary' {} a -> s {roleName = a} :: PutRolePermissionsBoundary)

-- | The ARN of the policy that is used to set the permissions boundary for
-- the role.
putRolePermissionsBoundary_permissionsBoundary :: Lens.Lens' PutRolePermissionsBoundary Prelude.Text
putRolePermissionsBoundary_permissionsBoundary = Lens.lens (\PutRolePermissionsBoundary' {permissionsBoundary} -> permissionsBoundary) (\s@PutRolePermissionsBoundary' {} a -> s {permissionsBoundary = a} :: PutRolePermissionsBoundary)

instance
  Prelude.AWSRequest
    PutRolePermissionsBoundary
  where
  type
    Rs PutRolePermissionsBoundary =
      PutRolePermissionsBoundaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      PutRolePermissionsBoundaryResponse'

instance Prelude.Hashable PutRolePermissionsBoundary

instance Prelude.NFData PutRolePermissionsBoundary

instance Prelude.ToHeaders PutRolePermissionsBoundary where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutRolePermissionsBoundary where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRolePermissionsBoundary where
  toQuery PutRolePermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutRolePermissionsBoundary" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "PermissionsBoundary" Prelude.=: permissionsBoundary
      ]

-- | /See:/ 'newPutRolePermissionsBoundaryResponse' smart constructor.
data PutRolePermissionsBoundaryResponse = PutRolePermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRolePermissionsBoundaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRolePermissionsBoundaryResponse ::
  PutRolePermissionsBoundaryResponse
newPutRolePermissionsBoundaryResponse =
  PutRolePermissionsBoundaryResponse'

instance
  Prelude.NFData
    PutRolePermissionsBoundaryResponse
