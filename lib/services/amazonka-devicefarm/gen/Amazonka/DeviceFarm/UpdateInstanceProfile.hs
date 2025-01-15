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
-- Module      : Amazonka.DeviceFarm.UpdateInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an existing private device instance profile.
module Amazonka.DeviceFarm.UpdateInstanceProfile
  ( -- * Creating a Request
    UpdateInstanceProfile (..),
    newUpdateInstanceProfile,

    -- * Request Lenses
    updateInstanceProfile_description,
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_name,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_arn,

    -- * Destructuring the Response
    UpdateInstanceProfileResponse (..),
    newUpdateInstanceProfileResponse,

    -- * Response Lenses
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInstanceProfile' smart constructor.
data UpdateInstanceProfile = UpdateInstanceProfile'
  { -- | The updated description for your instance profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specifies the list of app packages that should
    -- not be cleaned up from the device after a test run is over.
    --
    -- The list of packages is only considered if you set @packageCleanup@ to
    -- @true@.
    excludeAppPackagesFromCleanup :: Prelude.Maybe [Prelude.Text],
    -- | The updated name for your instance profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated choice for whether you want to specify package cleanup. The
    -- default value is @false@ for private devices.
    packageCleanup :: Prelude.Maybe Prelude.Bool,
    -- | The updated choice for whether you want to reboot the device after use.
    -- The default value is @true@.
    rebootAfterUse :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateInstanceProfile_description' - The updated description for your instance profile.
--
-- 'excludeAppPackagesFromCleanup', 'updateInstanceProfile_excludeAppPackagesFromCleanup' - An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to
-- @true@.
--
-- 'name', 'updateInstanceProfile_name' - The updated name for your instance profile.
--
-- 'packageCleanup', 'updateInstanceProfile_packageCleanup' - The updated choice for whether you want to specify package cleanup. The
-- default value is @false@ for private devices.
--
-- 'rebootAfterUse', 'updateInstanceProfile_rebootAfterUse' - The updated choice for whether you want to reboot the device after use.
-- The default value is @true@.
--
-- 'arn', 'updateInstanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile.
newUpdateInstanceProfile ::
  -- | 'arn'
  Prelude.Text ->
  UpdateInstanceProfile
newUpdateInstanceProfile pArn_ =
  UpdateInstanceProfile'
    { description =
        Prelude.Nothing,
      excludeAppPackagesFromCleanup = Prelude.Nothing,
      name = Prelude.Nothing,
      packageCleanup = Prelude.Nothing,
      rebootAfterUse = Prelude.Nothing,
      arn = pArn_
    }

-- | The updated description for your instance profile.
updateInstanceProfile_description :: Lens.Lens' UpdateInstanceProfile (Prelude.Maybe Prelude.Text)
updateInstanceProfile_description = Lens.lens (\UpdateInstanceProfile' {description} -> description) (\s@UpdateInstanceProfile' {} a -> s {description = a} :: UpdateInstanceProfile)

-- | An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to
-- @true@.
updateInstanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' UpdateInstanceProfile (Prelude.Maybe [Prelude.Text])
updateInstanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\UpdateInstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@UpdateInstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: UpdateInstanceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The updated name for your instance profile.
updateInstanceProfile_name :: Lens.Lens' UpdateInstanceProfile (Prelude.Maybe Prelude.Text)
updateInstanceProfile_name = Lens.lens (\UpdateInstanceProfile' {name} -> name) (\s@UpdateInstanceProfile' {} a -> s {name = a} :: UpdateInstanceProfile)

-- | The updated choice for whether you want to specify package cleanup. The
-- default value is @false@ for private devices.
updateInstanceProfile_packageCleanup :: Lens.Lens' UpdateInstanceProfile (Prelude.Maybe Prelude.Bool)
updateInstanceProfile_packageCleanup = Lens.lens (\UpdateInstanceProfile' {packageCleanup} -> packageCleanup) (\s@UpdateInstanceProfile' {} a -> s {packageCleanup = a} :: UpdateInstanceProfile)

-- | The updated choice for whether you want to reboot the device after use.
-- The default value is @true@.
updateInstanceProfile_rebootAfterUse :: Lens.Lens' UpdateInstanceProfile (Prelude.Maybe Prelude.Bool)
updateInstanceProfile_rebootAfterUse = Lens.lens (\UpdateInstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@UpdateInstanceProfile' {} a -> s {rebootAfterUse = a} :: UpdateInstanceProfile)

-- | The Amazon Resource Name (ARN) of the instance profile.
updateInstanceProfile_arn :: Lens.Lens' UpdateInstanceProfile Prelude.Text
updateInstanceProfile_arn = Lens.lens (\UpdateInstanceProfile' {arn} -> arn) (\s@UpdateInstanceProfile' {} a -> s {arn = a} :: UpdateInstanceProfile)

instance Core.AWSRequest UpdateInstanceProfile where
  type
    AWSResponse UpdateInstanceProfile =
      UpdateInstanceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInstanceProfileResponse'
            Prelude.<$> (x Data..?> "instanceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInstanceProfile where
  hashWithSalt _salt UpdateInstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` excludeAppPackagesFromCleanup
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` packageCleanup
      `Prelude.hashWithSalt` rebootAfterUse
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateInstanceProfile where
  rnf UpdateInstanceProfile' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf excludeAppPackagesFromCleanup `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf packageCleanup `Prelude.seq`
            Prelude.rnf rebootAfterUse `Prelude.seq`
              Prelude.rnf arn

instance Data.ToHeaders UpdateInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.UpdateInstanceProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInstanceProfile where
  toJSON UpdateInstanceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("excludeAppPackagesFromCleanup" Data..=)
              Prelude.<$> excludeAppPackagesFromCleanup,
            ("name" Data..=) Prelude.<$> name,
            ("packageCleanup" Data..=)
              Prelude.<$> packageCleanup,
            ("rebootAfterUse" Data..=)
              Prelude.<$> rebootAfterUse,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateInstanceProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceProfileResponse' smart constructor.
data UpdateInstanceProfileResponse = UpdateInstanceProfileResponse'
  { -- | An object that contains information about your instance profile.
    instanceProfile :: Prelude.Maybe InstanceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateInstanceProfileResponse
newUpdateInstanceProfileResponse pHttpStatus_ =
  UpdateInstanceProfileResponse'
    { instanceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your instance profile.
updateInstanceProfileResponse_instanceProfile :: Lens.Lens' UpdateInstanceProfileResponse (Prelude.Maybe InstanceProfile)
updateInstanceProfileResponse_instanceProfile = Lens.lens (\UpdateInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@UpdateInstanceProfileResponse' {} a -> s {instanceProfile = a} :: UpdateInstanceProfileResponse)

-- | The response's http status code.
updateInstanceProfileResponse_httpStatus :: Lens.Lens' UpdateInstanceProfileResponse Prelude.Int
updateInstanceProfileResponse_httpStatus = Lens.lens (\UpdateInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateInstanceProfileResponse' {} a -> s {httpStatus = a} :: UpdateInstanceProfileResponse)

instance Prelude.NFData UpdateInstanceProfileResponse where
  rnf UpdateInstanceProfileResponse' {..} =
    Prelude.rnf instanceProfile `Prelude.seq`
      Prelude.rnf httpStatus
