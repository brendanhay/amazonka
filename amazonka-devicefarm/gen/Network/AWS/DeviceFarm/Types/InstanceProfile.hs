{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceProfile where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the instance profile.
--
-- /See:/ 'newInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { -- | An array of strings containing the list of app packages that should not
    -- be cleaned up from the device after a test run completes.
    --
    -- The list of packages is considered only if you set @packageCleanup@ to
    -- @true@.
    excludeAppPackagesFromCleanup :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Core.Maybe Core.Text,
    -- | The name of the instance profile.
    name :: Core.Maybe Core.Text,
    -- | The description of the instance profile.
    description :: Core.Maybe Core.Text,
    -- | When set to @true@, Device Farm reboots the instance after a test run.
    -- The default value is @true@.
    rebootAfterUse :: Core.Maybe Core.Bool,
    -- | When set to @true@, Device Farm removes app packages after a test run.
    -- The default value is @false@ for private devices.
    packageCleanup :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeAppPackagesFromCleanup', 'instanceProfile_excludeAppPackagesFromCleanup' - An array of strings containing the list of app packages that should not
-- be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
--
-- 'arn', 'instanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'instanceProfile_name' - The name of the instance profile.
--
-- 'description', 'instanceProfile_description' - The description of the instance profile.
--
-- 'rebootAfterUse', 'instanceProfile_rebootAfterUse' - When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
--
-- 'packageCleanup', 'instanceProfile_packageCleanup' - When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
newInstanceProfile ::
  InstanceProfile
newInstanceProfile =
  InstanceProfile'
    { excludeAppPackagesFromCleanup =
        Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      rebootAfterUse = Core.Nothing,
      packageCleanup = Core.Nothing
    }

-- | An array of strings containing the list of app packages that should not
-- be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
instanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' InstanceProfile (Core.Maybe [Core.Text])
instanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\InstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@InstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: InstanceProfile) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the instance profile.
instanceProfile_arn :: Lens.Lens' InstanceProfile (Core.Maybe Core.Text)
instanceProfile_arn = Lens.lens (\InstanceProfile' {arn} -> arn) (\s@InstanceProfile' {} a -> s {arn = a} :: InstanceProfile)

-- | The name of the instance profile.
instanceProfile_name :: Lens.Lens' InstanceProfile (Core.Maybe Core.Text)
instanceProfile_name = Lens.lens (\InstanceProfile' {name} -> name) (\s@InstanceProfile' {} a -> s {name = a} :: InstanceProfile)

-- | The description of the instance profile.
instanceProfile_description :: Lens.Lens' InstanceProfile (Core.Maybe Core.Text)
instanceProfile_description = Lens.lens (\InstanceProfile' {description} -> description) (\s@InstanceProfile' {} a -> s {description = a} :: InstanceProfile)

-- | When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
instanceProfile_rebootAfterUse :: Lens.Lens' InstanceProfile (Core.Maybe Core.Bool)
instanceProfile_rebootAfterUse = Lens.lens (\InstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@InstanceProfile' {} a -> s {rebootAfterUse = a} :: InstanceProfile)

-- | When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
instanceProfile_packageCleanup :: Lens.Lens' InstanceProfile (Core.Maybe Core.Bool)
instanceProfile_packageCleanup = Lens.lens (\InstanceProfile' {packageCleanup} -> packageCleanup) (\s@InstanceProfile' {} a -> s {packageCleanup = a} :: InstanceProfile)

instance Core.FromJSON InstanceProfile where
  parseJSON =
    Core.withObject
      "InstanceProfile"
      ( \x ->
          InstanceProfile'
            Core.<$> ( x Core..:? "excludeAppPackagesFromCleanup"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "rebootAfterUse")
            Core.<*> (x Core..:? "packageCleanup")
      )

instance Core.Hashable InstanceProfile

instance Core.NFData InstanceProfile
