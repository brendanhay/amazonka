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
-- Module      : Amazonka.DeviceFarm.CreateInstanceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile that can be applied to one or more private fleet
-- device instances.
module Amazonka.DeviceFarm.CreateInstanceProfile
  ( -- * Creating a Request
    CreateInstanceProfile (..),
    newCreateInstanceProfile,

    -- * Request Lenses
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_description,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_name,

    -- * Destructuring the Response
    CreateInstanceProfileResponse (..),
    newCreateInstanceProfileResponse,

    -- * Response Lenses
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { -- | An array of strings that specifies the list of app packages that should
    -- not be cleaned up from the device after a test run.
    --
    -- The list of packages is considered only if you set @packageCleanup@ to
    -- @true@.
    excludeAppPackagesFromCleanup :: Prelude.Maybe [Prelude.Text],
    -- | The description of your instance profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, Device Farm removes app packages after a test run.
    -- The default value is @false@ for private devices.
    packageCleanup :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, Device Farm reboots the instance after a test run.
    -- The default value is @true@.
    rebootAfterUse :: Prelude.Maybe Prelude.Bool,
    -- | The name of your instance profile.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeAppPackagesFromCleanup', 'createInstanceProfile_excludeAppPackagesFromCleanup' - An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
--
-- 'description', 'createInstanceProfile_description' - The description of your instance profile.
--
-- 'packageCleanup', 'createInstanceProfile_packageCleanup' - When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
--
-- 'rebootAfterUse', 'createInstanceProfile_rebootAfterUse' - When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
--
-- 'name', 'createInstanceProfile_name' - The name of your instance profile.
newCreateInstanceProfile ::
  -- | 'name'
  Prelude.Text ->
  CreateInstanceProfile
newCreateInstanceProfile pName_ =
  CreateInstanceProfile'
    { excludeAppPackagesFromCleanup =
        Prelude.Nothing,
      description = Prelude.Nothing,
      packageCleanup = Prelude.Nothing,
      rebootAfterUse = Prelude.Nothing,
      name = pName_
    }

-- | An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
createInstanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe [Prelude.Text])
createInstanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\CreateInstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@CreateInstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: CreateInstanceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The description of your instance profile.
createInstanceProfile_description :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Text)
createInstanceProfile_description = Lens.lens (\CreateInstanceProfile' {description} -> description) (\s@CreateInstanceProfile' {} a -> s {description = a} :: CreateInstanceProfile)

-- | When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
createInstanceProfile_packageCleanup :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Bool)
createInstanceProfile_packageCleanup = Lens.lens (\CreateInstanceProfile' {packageCleanup} -> packageCleanup) (\s@CreateInstanceProfile' {} a -> s {packageCleanup = a} :: CreateInstanceProfile)

-- | When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
createInstanceProfile_rebootAfterUse :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Bool)
createInstanceProfile_rebootAfterUse = Lens.lens (\CreateInstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@CreateInstanceProfile' {} a -> s {rebootAfterUse = a} :: CreateInstanceProfile)

-- | The name of your instance profile.
createInstanceProfile_name :: Lens.Lens' CreateInstanceProfile Prelude.Text
createInstanceProfile_name = Lens.lens (\CreateInstanceProfile' {name} -> name) (\s@CreateInstanceProfile' {} a -> s {name = a} :: CreateInstanceProfile)

instance Core.AWSRequest CreateInstanceProfile where
  type
    AWSResponse CreateInstanceProfile =
      CreateInstanceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceProfileResponse'
            Prelude.<$> (x Core..?> "instanceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstanceProfile where
  hashWithSalt _salt CreateInstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` excludeAppPackagesFromCleanup
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` packageCleanup
      `Prelude.hashWithSalt` rebootAfterUse
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateInstanceProfile where
  rnf CreateInstanceProfile' {..} =
    Prelude.rnf excludeAppPackagesFromCleanup
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf packageCleanup
      `Prelude.seq` Prelude.rnf rebootAfterUse
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateInstanceProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateInstanceProfile where
  toJSON CreateInstanceProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("excludeAppPackagesFromCleanup" Core..=)
              Prelude.<$> excludeAppPackagesFromCleanup,
            ("description" Core..=) Prelude.<$> description,
            ("packageCleanup" Core..=)
              Prelude.<$> packageCleanup,
            ("rebootAfterUse" Core..=)
              Prelude.<$> rebootAfterUse,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateInstanceProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { -- | An object that contains information about your instance profile.
    instanceProfile :: Prelude.Maybe InstanceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfile', 'createInstanceProfileResponse_instanceProfile' - An object that contains information about your instance profile.
--
-- 'httpStatus', 'createInstanceProfileResponse_httpStatus' - The response's http status code.
newCreateInstanceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceProfileResponse
newCreateInstanceProfileResponse pHttpStatus_ =
  CreateInstanceProfileResponse'
    { instanceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your instance profile.
createInstanceProfileResponse_instanceProfile :: Lens.Lens' CreateInstanceProfileResponse (Prelude.Maybe InstanceProfile)
createInstanceProfileResponse_instanceProfile = Lens.lens (\CreateInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@CreateInstanceProfileResponse' {} a -> s {instanceProfile = a} :: CreateInstanceProfileResponse)

-- | The response's http status code.
createInstanceProfileResponse_httpStatus :: Lens.Lens' CreateInstanceProfileResponse Prelude.Int
createInstanceProfileResponse_httpStatus = Lens.lens (\CreateInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceProfileResponse' {} a -> s {httpStatus = a} :: CreateInstanceProfileResponse)

instance Prelude.NFData CreateInstanceProfileResponse where
  rnf CreateInstanceProfileResponse' {..} =
    Prelude.rnf instanceProfile
      `Prelude.seq` Prelude.rnf httpStatus
