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
-- Module      : Amazonka.DeviceFarm.Types.InstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.InstanceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the instance profile.
--
-- /See:/ 'newInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, Device Farm reboots the instance after a test run.
    -- The default value is @true@.
    rebootAfterUse :: Prelude.Maybe Prelude.Bool,
    -- | The name of the instance profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, Device Farm removes app packages after a test run.
    -- The default value is @false@ for private devices.
    packageCleanup :: Prelude.Maybe Prelude.Bool,
    -- | An array of strings containing the list of app packages that should not
    -- be cleaned up from the device after a test run completes.
    --
    -- The list of packages is considered only if you set @packageCleanup@ to
    -- @true@.
    excludeAppPackagesFromCleanup :: Prelude.Maybe [Prelude.Text],
    -- | The description of the instance profile.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'instanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'rebootAfterUse', 'instanceProfile_rebootAfterUse' - When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
--
-- 'name', 'instanceProfile_name' - The name of the instance profile.
--
-- 'packageCleanup', 'instanceProfile_packageCleanup' - When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
--
-- 'excludeAppPackagesFromCleanup', 'instanceProfile_excludeAppPackagesFromCleanup' - An array of strings containing the list of app packages that should not
-- be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
--
-- 'description', 'instanceProfile_description' - The description of the instance profile.
newInstanceProfile ::
  InstanceProfile
newInstanceProfile =
  InstanceProfile'
    { arn = Prelude.Nothing,
      rebootAfterUse = Prelude.Nothing,
      name = Prelude.Nothing,
      packageCleanup = Prelude.Nothing,
      excludeAppPackagesFromCleanup = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
instanceProfile_arn :: Lens.Lens' InstanceProfile (Prelude.Maybe Prelude.Text)
instanceProfile_arn = Lens.lens (\InstanceProfile' {arn} -> arn) (\s@InstanceProfile' {} a -> s {arn = a} :: InstanceProfile)

-- | When set to @true@, Device Farm reboots the instance after a test run.
-- The default value is @true@.
instanceProfile_rebootAfterUse :: Lens.Lens' InstanceProfile (Prelude.Maybe Prelude.Bool)
instanceProfile_rebootAfterUse = Lens.lens (\InstanceProfile' {rebootAfterUse} -> rebootAfterUse) (\s@InstanceProfile' {} a -> s {rebootAfterUse = a} :: InstanceProfile)

-- | The name of the instance profile.
instanceProfile_name :: Lens.Lens' InstanceProfile (Prelude.Maybe Prelude.Text)
instanceProfile_name = Lens.lens (\InstanceProfile' {name} -> name) (\s@InstanceProfile' {} a -> s {name = a} :: InstanceProfile)

-- | When set to @true@, Device Farm removes app packages after a test run.
-- The default value is @false@ for private devices.
instanceProfile_packageCleanup :: Lens.Lens' InstanceProfile (Prelude.Maybe Prelude.Bool)
instanceProfile_packageCleanup = Lens.lens (\InstanceProfile' {packageCleanup} -> packageCleanup) (\s@InstanceProfile' {} a -> s {packageCleanup = a} :: InstanceProfile)

-- | An array of strings containing the list of app packages that should not
-- be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to
-- @true@.
instanceProfile_excludeAppPackagesFromCleanup :: Lens.Lens' InstanceProfile (Prelude.Maybe [Prelude.Text])
instanceProfile_excludeAppPackagesFromCleanup = Lens.lens (\InstanceProfile' {excludeAppPackagesFromCleanup} -> excludeAppPackagesFromCleanup) (\s@InstanceProfile' {} a -> s {excludeAppPackagesFromCleanup = a} :: InstanceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The description of the instance profile.
instanceProfile_description :: Lens.Lens' InstanceProfile (Prelude.Maybe Prelude.Text)
instanceProfile_description = Lens.lens (\InstanceProfile' {description} -> description) (\s@InstanceProfile' {} a -> s {description = a} :: InstanceProfile)

instance Core.FromJSON InstanceProfile where
  parseJSON =
    Core.withObject
      "InstanceProfile"
      ( \x ->
          InstanceProfile'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "rebootAfterUse")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "packageCleanup")
            Prelude.<*> ( x Core..:? "excludeAppPackagesFromCleanup"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable InstanceProfile

instance Prelude.NFData InstanceProfile
