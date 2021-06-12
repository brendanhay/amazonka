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
-- Module      : Network.AWS.DeviceFarm.UpdateInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an existing private device instance profile.
module Network.AWS.DeviceFarm.UpdateInstanceProfile
  ( -- * Creating a Request
    UpdateInstanceProfile (..),
    newUpdateInstanceProfile,

    -- * Request Lenses
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_name,
    updateInstanceProfile_description,
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_arn,

    -- * Destructuring the Response
    UpdateInstanceProfileResponse (..),
    newUpdateInstanceProfileResponse,

    -- * Response Lenses
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateInstanceProfile' smart constructor.
data UpdateInstanceProfile = UpdateInstanceProfile'
  { -- | An array of strings that specifies the list of app packages that should
    -- not be cleaned up from the device after a test run is over.
    --
    -- The list of packages is only considered if you set @packageCleanup@ to
    -- @true@.
    excludeAppPackagesFromCleanup :: Core.Maybe [Core.Text],
    -- | The updated name for your instance profile.
    name :: Core.Maybe Core.Text,
    -- | The updated description for your instance profile.
    description :: Core.Maybe Core.Text,
    -- | The updated choice for whether you want to reboot the device after use.
    -- The default value is @true@.
    rebootAfterUse :: Core.Maybe Core.Bool,
    -- | The updated choice for whether you want to specify package cleanup. The
    -- default value is @false@ for private devices.
    packageCleanup :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeAppPackagesFromCleanup', 'updateInstanceProfile_excludeAppPackagesFromCleanup' - An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to
-- @true@.
--
-- 'name', 'updateInstanceProfile_name' - The updated name for your instance profile.
--
-- 'description', 'updateInstanceProfile_description' - The updated description for your instance profile.
--
-- 'rebootAfterUse', 'updateInstanceProfile_rebootAfterUse' - The updated choice for whether you want to reboot the device after use.
-- The default value is @true@.
--
-- 'packageCleanup', 'updateInstanceProfile_packageCleanup' - The updated choice for whether you want to specify package cleanup. The
-- default value is @false@ for private devices.
--
-- 'arn', 'updateInstanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile.
newUpdateInstanceProfile ::
  -- | 'arn'
  Core.Text ->
  UpdateInstanceProfile
newUpdateInstanceProfile pArn_ =
  UpdateInstanceProfile'
    { excludeAppPackagesFromCleanup =
        Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      rebootAfterUse = Core.Nothing,
      packageCleanup = Core.Nothing,
      arn = pArn_
    }

-- | An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to
-- @true@.
updateInstanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' UpdateInstanceProfile (Core.Maybe [Core.Text])
updateInstanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\UpdateInstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@UpdateInstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: UpdateInstanceProfile) Core.. Lens.mapping Lens._Coerce

-- | The updated name for your instance profile.
updateInstanceProfile_name :: Lens.Lens' UpdateInstanceProfile (Core.Maybe Core.Text)
updateInstanceProfile_name = Lens.lens (\UpdateInstanceProfile' {name} -> name) (\s@UpdateInstanceProfile' {} a -> s {name = a} :: UpdateInstanceProfile)

-- | The updated description for your instance profile.
updateInstanceProfile_description :: Lens.Lens' UpdateInstanceProfile (Core.Maybe Core.Text)
updateInstanceProfile_description = Lens.lens (\UpdateInstanceProfile' {description} -> description) (\s@UpdateInstanceProfile' {} a -> s {description = a} :: UpdateInstanceProfile)

-- | The updated choice for whether you want to reboot the device after use.
-- The default value is @true@.
updateInstanceProfile_rebootAfterUse :: Lens.Lens' UpdateInstanceProfile (Core.Maybe Core.Bool)
updateInstanceProfile_rebootAfterUse = Lens.lens (\UpdateInstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@UpdateInstanceProfile' {} a -> s {rebootAfterUse = a} :: UpdateInstanceProfile)

-- | The updated choice for whether you want to specify package cleanup. The
-- default value is @false@ for private devices.
updateInstanceProfile_packageCleanup :: Lens.Lens' UpdateInstanceProfile (Core.Maybe Core.Bool)
updateInstanceProfile_packageCleanup = Lens.lens (\UpdateInstanceProfile' {packageCleanup} -> packageCleanup) (\s@UpdateInstanceProfile' {} a -> s {packageCleanup = a} :: UpdateInstanceProfile)

-- | The Amazon Resource Name (ARN) of the instance profile.
updateInstanceProfile_arn :: Lens.Lens' UpdateInstanceProfile Core.Text
updateInstanceProfile_arn = Lens.lens (\UpdateInstanceProfile' {arn} -> arn) (\s@UpdateInstanceProfile' {} a -> s {arn = a} :: UpdateInstanceProfile)

instance Core.AWSRequest UpdateInstanceProfile where
  type
    AWSResponse UpdateInstanceProfile =
      UpdateInstanceProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInstanceProfileResponse'
            Core.<$> (x Core..?> "instanceProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateInstanceProfile

instance Core.NFData UpdateInstanceProfile

instance Core.ToHeaders UpdateInstanceProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateInstanceProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateInstanceProfile where
  toJSON UpdateInstanceProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("excludeAppPackagesFromCleanup" Core..=)
              Core.<$> excludeAppPackagesFromCleanup,
            ("name" Core..=) Core.<$> name,
            ("description" Core..=) Core.<$> description,
            ("rebootAfterUse" Core..=) Core.<$> rebootAfterUse,
            ("packageCleanup" Core..=) Core.<$> packageCleanup,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateInstanceProfile where
  toPath = Core.const "/"

instance Core.ToQuery UpdateInstanceProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateInstanceProfileResponse' smart constructor.
data UpdateInstanceProfileResponse = UpdateInstanceProfileResponse'
  { -- | An object that contains information about your instance profile.
    instanceProfile :: Core.Maybe InstanceProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfile', 'updateInstanceProfileResponse_instanceProfile' - An object that contains information about your instance profile.
--
-- 'httpStatus', 'updateInstanceProfileResponse_httpStatus' - The response's http status code.
newUpdateInstanceProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateInstanceProfileResponse
newUpdateInstanceProfileResponse pHttpStatus_ =
  UpdateInstanceProfileResponse'
    { instanceProfile =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your instance profile.
updateInstanceProfileResponse_instanceProfile :: Lens.Lens' UpdateInstanceProfileResponse (Core.Maybe InstanceProfile)
updateInstanceProfileResponse_instanceProfile = Lens.lens (\UpdateInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@UpdateInstanceProfileResponse' {} a -> s {instanceProfile = a} :: UpdateInstanceProfileResponse)

-- | The response's http status code.
updateInstanceProfileResponse_httpStatus :: Lens.Lens' UpdateInstanceProfileResponse Core.Int
updateInstanceProfileResponse_httpStatus = Lens.lens (\UpdateInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateInstanceProfileResponse' {} a -> s {httpStatus = a} :: UpdateInstanceProfileResponse)

instance Core.NFData UpdateInstanceProfileResponse
