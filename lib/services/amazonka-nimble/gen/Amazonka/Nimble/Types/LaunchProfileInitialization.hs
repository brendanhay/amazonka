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
-- Module      : Amazonka.Nimble.Types.LaunchProfileInitialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileInitialization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory
import Amazonka.Nimble.Types.LaunchProfileInitializationScript
import Amazonka.Nimble.Types.LaunchProfilePlatform
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newLaunchProfileInitialization' smart constructor.
data LaunchProfileInitialization = LaunchProfileInitialization'
  { -- | The platform of the launch platform, either WINDOWS or LINUX.
    platform :: Prelude.Maybe LaunchProfilePlatform,
    -- | A LaunchProfileInitializationActiveDirectory resource.
    activeDirectory :: Prelude.Maybe LaunchProfileInitializationActiveDirectory,
    -- | The launch purpose.
    launchPurpose :: Prelude.Maybe Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name for the launch profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The user initializtion scripts.
    userInitializationScripts :: Prelude.Maybe [LaunchProfileInitializationScript],
    -- | The system initializtion scripts.
    systemInitializationScripts :: Prelude.Maybe [LaunchProfileInitializationScript]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileInitialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'launchProfileInitialization_platform' - The platform of the launch platform, either WINDOWS or LINUX.
--
-- 'activeDirectory', 'launchProfileInitialization_activeDirectory' - A LaunchProfileInitializationActiveDirectory resource.
--
-- 'launchPurpose', 'launchProfileInitialization_launchPurpose' - The launch purpose.
--
-- 'launchProfileId', 'launchProfileInitialization_launchProfileId' - The launch profile ID.
--
-- 'ec2SecurityGroupIds', 'launchProfileInitialization_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'name', 'launchProfileInitialization_name' - The name for the launch profile.
--
-- 'launchProfileProtocolVersion', 'launchProfileInitialization_launchProfileProtocolVersion' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'userInitializationScripts', 'launchProfileInitialization_userInitializationScripts' - The user initializtion scripts.
--
-- 'systemInitializationScripts', 'launchProfileInitialization_systemInitializationScripts' - The system initializtion scripts.
newLaunchProfileInitialization ::
  LaunchProfileInitialization
newLaunchProfileInitialization =
  LaunchProfileInitialization'
    { platform =
        Prelude.Nothing,
      activeDirectory = Prelude.Nothing,
      launchPurpose = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      name = Prelude.Nothing,
      launchProfileProtocolVersion = Prelude.Nothing,
      userInitializationScripts = Prelude.Nothing,
      systemInitializationScripts = Prelude.Nothing
    }

-- | The platform of the launch platform, either WINDOWS or LINUX.
launchProfileInitialization_platform :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe LaunchProfilePlatform)
launchProfileInitialization_platform = Lens.lens (\LaunchProfileInitialization' {platform} -> platform) (\s@LaunchProfileInitialization' {} a -> s {platform = a} :: LaunchProfileInitialization)

-- | A LaunchProfileInitializationActiveDirectory resource.
launchProfileInitialization_activeDirectory :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe LaunchProfileInitializationActiveDirectory)
launchProfileInitialization_activeDirectory = Lens.lens (\LaunchProfileInitialization' {activeDirectory} -> activeDirectory) (\s@LaunchProfileInitialization' {} a -> s {activeDirectory = a} :: LaunchProfileInitialization)

-- | The launch purpose.
launchProfileInitialization_launchPurpose :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchPurpose = Lens.lens (\LaunchProfileInitialization' {launchPurpose} -> launchPurpose) (\s@LaunchProfileInitialization' {} a -> s {launchPurpose = a} :: LaunchProfileInitialization)

-- | The launch profile ID.
launchProfileInitialization_launchProfileId :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchProfileId = Lens.lens (\LaunchProfileInitialization' {launchProfileId} -> launchProfileId) (\s@LaunchProfileInitialization' {} a -> s {launchProfileId = a} :: LaunchProfileInitialization)

-- | The EC2 security groups that control access to the studio component.
launchProfileInitialization_ec2SecurityGroupIds :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchProfileInitialization_ec2SecurityGroupIds = Lens.lens (\LaunchProfileInitialization' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@LaunchProfileInitialization' {} a -> s {ec2SecurityGroupIds = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

-- | The name for the launch profile.
launchProfileInitialization_name :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_name = Lens.lens (\LaunchProfileInitialization' {name} -> name) (\s@LaunchProfileInitialization' {} a -> s {name = a} :: LaunchProfileInitialization)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
launchProfileInitialization_launchProfileProtocolVersion :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchProfileProtocolVersion = Lens.lens (\LaunchProfileInitialization' {launchProfileProtocolVersion} -> launchProfileProtocolVersion) (\s@LaunchProfileInitialization' {} a -> s {launchProfileProtocolVersion = a} :: LaunchProfileInitialization)

-- | The user initializtion scripts.
launchProfileInitialization_userInitializationScripts :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe [LaunchProfileInitializationScript])
launchProfileInitialization_userInitializationScripts = Lens.lens (\LaunchProfileInitialization' {userInitializationScripts} -> userInitializationScripts) (\s@LaunchProfileInitialization' {} a -> s {userInitializationScripts = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

-- | The system initializtion scripts.
launchProfileInitialization_systemInitializationScripts :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe [LaunchProfileInitializationScript])
launchProfileInitialization_systemInitializationScripts = Lens.lens (\LaunchProfileInitialization' {systemInitializationScripts} -> systemInitializationScripts) (\s@LaunchProfileInitialization' {} a -> s {systemInitializationScripts = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LaunchProfileInitialization where
  parseJSON =
    Core.withObject
      "LaunchProfileInitialization"
      ( \x ->
          LaunchProfileInitialization'
            Prelude.<$> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "activeDirectory")
            Prelude.<*> (x Core..:? "launchPurpose")
            Prelude.<*> (x Core..:? "launchProfileId")
            Prelude.<*> (x Core..:? "ec2SecurityGroupIds")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "launchProfileProtocolVersion")
            Prelude.<*> ( x Core..:? "userInitializationScripts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "systemInitializationScripts"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LaunchProfileInitialization where
  hashWithSalt salt' LaunchProfileInitialization' {..} =
    salt'
      `Prelude.hashWithSalt` systemInitializationScripts
      `Prelude.hashWithSalt` userInitializationScripts
      `Prelude.hashWithSalt` launchProfileProtocolVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` launchPurpose
      `Prelude.hashWithSalt` activeDirectory
      `Prelude.hashWithSalt` platform

instance Prelude.NFData LaunchProfileInitialization where
  rnf LaunchProfileInitialization' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf systemInitializationScripts
      `Prelude.seq` Prelude.rnf userInitializationScripts
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf launchPurpose
      `Prelude.seq` Prelude.rnf activeDirectory
