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
-- Module      : Network.AWS.IAM.UpdateRoleDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use UpdateRole instead.
--
-- Modifies only the description of a role. This operation performs the
-- same function as the @Description@ parameter in the @UpdateRole@
-- operation.
module Network.AWS.IAM.UpdateRoleDescription
  ( -- * Creating a Request
    UpdateRoleDescription (..),
    newUpdateRoleDescription,

    -- * Request Lenses
    updateRoleDescription_roleName,
    updateRoleDescription_description,

    -- * Destructuring the Response
    UpdateRoleDescriptionResponse (..),
    newUpdateRoleDescriptionResponse,

    -- * Response Lenses
    updateRoleDescriptionResponse_role,
    updateRoleDescriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRoleDescription' smart constructor.
data UpdateRoleDescription = UpdateRoleDescription'
  { -- | The name of the role that you want to modify.
    roleName :: Prelude.Text,
    -- | The new description that you want to apply to the specified role.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoleDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'updateRoleDescription_roleName' - The name of the role that you want to modify.
--
-- 'description', 'updateRoleDescription_description' - The new description that you want to apply to the specified role.
newUpdateRoleDescription ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  UpdateRoleDescription
newUpdateRoleDescription pRoleName_ pDescription_ =
  UpdateRoleDescription'
    { roleName = pRoleName_,
      description = pDescription_
    }

-- | The name of the role that you want to modify.
updateRoleDescription_roleName :: Lens.Lens' UpdateRoleDescription Prelude.Text
updateRoleDescription_roleName = Lens.lens (\UpdateRoleDescription' {roleName} -> roleName) (\s@UpdateRoleDescription' {} a -> s {roleName = a} :: UpdateRoleDescription)

-- | The new description that you want to apply to the specified role.
updateRoleDescription_description :: Lens.Lens' UpdateRoleDescription Prelude.Text
updateRoleDescription_description = Lens.lens (\UpdateRoleDescription' {description} -> description) (\s@UpdateRoleDescription' {} a -> s {description = a} :: UpdateRoleDescription)

instance Core.AWSRequest UpdateRoleDescription where
  type
    AWSResponse UpdateRoleDescription =
      UpdateRoleDescriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateRoleDescriptionResult"
      ( \s h x ->
          UpdateRoleDescriptionResponse'
            Prelude.<$> (x Core..@? "Role")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoleDescription

instance Prelude.NFData UpdateRoleDescription

instance Core.ToHeaders UpdateRoleDescription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateRoleDescription where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRoleDescription where
  toQuery UpdateRoleDescription' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("UpdateRoleDescription" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Core.=: roleName,
        "Description" Core.=: description
      ]

-- | /See:/ 'newUpdateRoleDescriptionResponse' smart constructor.
data UpdateRoleDescriptionResponse = UpdateRoleDescriptionResponse'
  { -- | A structure that contains details about the modified role.
    role' :: Prelude.Maybe Role,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoleDescriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'updateRoleDescriptionResponse_role' - A structure that contains details about the modified role.
--
-- 'httpStatus', 'updateRoleDescriptionResponse_httpStatus' - The response's http status code.
newUpdateRoleDescriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoleDescriptionResponse
newUpdateRoleDescriptionResponse pHttpStatus_ =
  UpdateRoleDescriptionResponse'
    { role' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the modified role.
updateRoleDescriptionResponse_role :: Lens.Lens' UpdateRoleDescriptionResponse (Prelude.Maybe Role)
updateRoleDescriptionResponse_role = Lens.lens (\UpdateRoleDescriptionResponse' {role'} -> role') (\s@UpdateRoleDescriptionResponse' {} a -> s {role' = a} :: UpdateRoleDescriptionResponse)

-- | The response's http status code.
updateRoleDescriptionResponse_httpStatus :: Lens.Lens' UpdateRoleDescriptionResponse Prelude.Int
updateRoleDescriptionResponse_httpStatus = Lens.lens (\UpdateRoleDescriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateRoleDescriptionResponse' {} a -> s {httpStatus = a} :: UpdateRoleDescriptionResponse)

instance Prelude.NFData UpdateRoleDescriptionResponse
