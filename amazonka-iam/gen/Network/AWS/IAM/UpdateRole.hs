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
-- Module      : Network.AWS.IAM.UpdateRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description or maximum session duration setting of a role.
module Network.AWS.IAM.UpdateRole
  ( -- * Creating a Request
    UpdateRole (..),
    newUpdateRole,

    -- * Request Lenses
    updateRole_maxSessionDuration,
    updateRole_description,
    updateRole_roleName,

    -- * Destructuring the Response
    UpdateRoleResponse (..),
    newUpdateRoleResponse,

    -- * Response Lenses
    updateRoleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRole' smart constructor.
data UpdateRole = UpdateRole'
  { -- | The maximum session duration (in seconds) that you want to set for the
    -- specified role. If you do not specify a value for this setting, the
    -- default maximum of one hour is applied. This setting can have a value
    -- from 1 hour to 12 hours.
    --
    -- Anyone who assumes the role from the AWS CLI or API can use the
    -- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
    -- to request a longer session. The @MaxSessionDuration@ setting determines
    -- the maximum duration that can be requested using the @DurationSeconds@
    -- parameter. If users don\'t specify a value for the @DurationSeconds@
    -- parameter, their security credentials are valid for one hour by default.
    -- This applies when you use the @AssumeRole*@ API operations or the
    -- @assume-role*@ CLI operations but does not apply when you use those
    -- operations to create a console URL. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
    -- in the /IAM User Guide/.
    maxSessionDuration :: Prelude.Maybe Prelude.Natural,
    -- | The new description that you want to apply to the specified role.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the role that you want to modify.
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSessionDuration', 'updateRole_maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the
-- specified role. If you do not specify a value for this setting, the
-- default maximum of one hour is applied. This setting can have a value
-- from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the
-- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
-- to request a longer session. The @MaxSessionDuration@ setting determines
-- the maximum duration that can be requested using the @DurationSeconds@
-- parameter. If users don\'t specify a value for the @DurationSeconds@
-- parameter, their security credentials are valid for one hour by default.
-- This applies when you use the @AssumeRole*@ API operations or the
-- @assume-role*@ CLI operations but does not apply when you use those
-- operations to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
-- in the /IAM User Guide/.
--
-- 'description', 'updateRole_description' - The new description that you want to apply to the specified role.
--
-- 'roleName', 'updateRole_roleName' - The name of the role that you want to modify.
newUpdateRole ::
  -- | 'roleName'
  Prelude.Text ->
  UpdateRole
newUpdateRole pRoleName_ =
  UpdateRole'
    { maxSessionDuration = Prelude.Nothing,
      description = Prelude.Nothing,
      roleName = pRoleName_
    }

-- | The maximum session duration (in seconds) that you want to set for the
-- specified role. If you do not specify a value for this setting, the
-- default maximum of one hour is applied. This setting can have a value
-- from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the
-- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
-- to request a longer session. The @MaxSessionDuration@ setting determines
-- the maximum duration that can be requested using the @DurationSeconds@
-- parameter. If users don\'t specify a value for the @DurationSeconds@
-- parameter, their security credentials are valid for one hour by default.
-- This applies when you use the @AssumeRole*@ API operations or the
-- @assume-role*@ CLI operations but does not apply when you use those
-- operations to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
-- in the /IAM User Guide/.
updateRole_maxSessionDuration :: Lens.Lens' UpdateRole (Prelude.Maybe Prelude.Natural)
updateRole_maxSessionDuration = Lens.lens (\UpdateRole' {maxSessionDuration} -> maxSessionDuration) (\s@UpdateRole' {} a -> s {maxSessionDuration = a} :: UpdateRole)

-- | The new description that you want to apply to the specified role.
updateRole_description :: Lens.Lens' UpdateRole (Prelude.Maybe Prelude.Text)
updateRole_description = Lens.lens (\UpdateRole' {description} -> description) (\s@UpdateRole' {} a -> s {description = a} :: UpdateRole)

-- | The name of the role that you want to modify.
updateRole_roleName :: Lens.Lens' UpdateRole Prelude.Text
updateRole_roleName = Lens.lens (\UpdateRole' {roleName} -> roleName) (\s@UpdateRole' {} a -> s {roleName = a} :: UpdateRole)

instance Core.AWSRequest UpdateRole where
  type AWSResponse UpdateRole = UpdateRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateRoleResult"
      ( \s h x ->
          UpdateRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRole

instance Prelude.NFData UpdateRole

instance Core.ToHeaders UpdateRole where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateRole where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRole where
  toQuery UpdateRole' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("UpdateRole" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxSessionDuration" Core.=: maxSessionDuration,
        "Description" Core.=: description,
        "RoleName" Core.=: roleName
      ]

-- | /See:/ 'newUpdateRoleResponse' smart constructor.
data UpdateRoleResponse = UpdateRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRoleResponse_httpStatus' - The response's http status code.
newUpdateRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoleResponse
newUpdateRoleResponse pHttpStatus_ =
  UpdateRoleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRoleResponse_httpStatus :: Lens.Lens' UpdateRoleResponse Prelude.Int
updateRoleResponse_httpStatus = Lens.lens (\UpdateRoleResponse' {httpStatus} -> httpStatus) (\s@UpdateRoleResponse' {} a -> s {httpStatus = a} :: UpdateRoleResponse)

instance Prelude.NFData UpdateRoleResponse
