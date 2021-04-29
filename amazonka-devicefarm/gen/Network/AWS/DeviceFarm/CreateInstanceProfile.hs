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
-- Module      : Network.AWS.DeviceFarm.CreateInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile that can be applied to one or more private fleet
-- device instances.
module Network.AWS.DeviceFarm.CreateInstanceProfile
  ( -- * Creating a Request
    CreateInstanceProfile (..),
    newCreateInstanceProfile,

    -- * Request Lenses
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_description,
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_name,

    -- * Destructuring the Response
    CreateInstanceProfileResponse (..),
    newCreateInstanceProfileResponse,

    -- * Response Lenses
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- | When set to @true@, Device Farm reboots the instance after a test run.
    -- The default value is @true@.
    rebootAfterUse :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, Device Farm removes app packages after a test run.
    -- The default value is @false@ for private devices.
    packageCleanup :: Prelude.Maybe Prelude.Bool,
    -- | The name of your instance profile.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'rebootAfterUse', 'createInstanceProfile_rebootAfterUse' - When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
--
-- 'packageCleanup', 'createInstanceProfile_packageCleanup' - When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
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
      rebootAfterUse = Prelude.Nothing,
      packageCleanup = Prelude.Nothing,
      name = pName_
    }

-- | An array of strings that specifies the list of app packages that should
-- not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
createInstanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe [Prelude.Text])
createInstanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\CreateInstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@CreateInstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: CreateInstanceProfile) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of your instance profile.
createInstanceProfile_description :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Text)
createInstanceProfile_description = Lens.lens (\CreateInstanceProfile' {description} -> description) (\s@CreateInstanceProfile' {} a -> s {description = a} :: CreateInstanceProfile)

-- | When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
createInstanceProfile_rebootAfterUse :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Bool)
createInstanceProfile_rebootAfterUse = Lens.lens (\CreateInstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@CreateInstanceProfile' {} a -> s {rebootAfterUse = a} :: CreateInstanceProfile)

-- | When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
createInstanceProfile_packageCleanup :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Bool)
createInstanceProfile_packageCleanup = Lens.lens (\CreateInstanceProfile' {packageCleanup} -> packageCleanup) (\s@CreateInstanceProfile' {} a -> s {packageCleanup = a} :: CreateInstanceProfile)

-- | The name of your instance profile.
createInstanceProfile_name :: Lens.Lens' CreateInstanceProfile Prelude.Text
createInstanceProfile_name = Lens.lens (\CreateInstanceProfile' {name} -> name) (\s@CreateInstanceProfile' {} a -> s {name = a} :: CreateInstanceProfile)

instance Prelude.AWSRequest CreateInstanceProfile where
  type
    Rs CreateInstanceProfile =
      CreateInstanceProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceProfileResponse'
            Prelude.<$> (x Prelude..?> "instanceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstanceProfile

instance Prelude.NFData CreateInstanceProfile

instance Prelude.ToHeaders CreateInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.CreateInstanceProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateInstanceProfile where
  toJSON CreateInstanceProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("excludeAppPackagesFromCleanup" Prelude..=)
              Prelude.<$> excludeAppPackagesFromCleanup,
            ("description" Prelude..=) Prelude.<$> description,
            ("rebootAfterUse" Prelude..=)
              Prelude.<$> rebootAfterUse,
            ("packageCleanup" Prelude..=)
              Prelude.<$> packageCleanup,
            Prelude.Just ("name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { -- | An object that contains information about your instance profile.
    instanceProfile :: Prelude.Maybe InstanceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateInstanceProfileResponse
