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
-- Module      : Amazonka.IAM.PutRolePermissionsBoundary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM role\'s
-- permissions boundary. You can use an Amazon Web Services managed policy
-- or a customer managed policy to set the boundary for a role. Use the
-- boundary to control the maximum permissions that the role can have.
-- Setting a permissions boundary is an advanced feature that can affect
-- the permissions for the role.
--
-- You cannot set the boundary for a service-linked role.
--
-- Policies used as permissions boundaries do not provide permissions. You
-- must also attach a permissions policy to the role. To learn how the
-- effective permissions for a role are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON policy evaluation logic>
-- in the IAM User Guide.
module Amazonka.IAM.PutRolePermissionsBoundary
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRolePermissionsBoundary' smart constructor.
data PutRolePermissionsBoundary = PutRolePermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM role for which you want to
    -- set the permissions boundary.
    roleName :: Prelude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for
    -- the role.
    permissionsBoundary :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PutRolePermissionsBoundary where
  type
    AWSResponse PutRolePermissionsBoundary =
      PutRolePermissionsBoundaryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      PutRolePermissionsBoundaryResponse'

instance Prelude.Hashable PutRolePermissionsBoundary where
  hashWithSalt _salt PutRolePermissionsBoundary' {..} =
    _salt
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` permissionsBoundary

instance Prelude.NFData PutRolePermissionsBoundary where
  rnf PutRolePermissionsBoundary' {..} =
    Prelude.rnf roleName `Prelude.seq`
      Prelude.rnf permissionsBoundary

instance Data.ToHeaders PutRolePermissionsBoundary where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutRolePermissionsBoundary where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRolePermissionsBoundary where
  toQuery PutRolePermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutRolePermissionsBoundary" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName,
        "PermissionsBoundary" Data.=: permissionsBoundary
      ]

-- | /See:/ 'newPutRolePermissionsBoundaryResponse' smart constructor.
data PutRolePermissionsBoundaryResponse = PutRolePermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
