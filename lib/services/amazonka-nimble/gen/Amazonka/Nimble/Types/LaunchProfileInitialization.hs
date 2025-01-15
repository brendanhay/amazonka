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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileInitialization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory
import Amazonka.Nimble.Types.LaunchProfileInitializationScript
import Amazonka.Nimble.Types.LaunchProfilePlatform
import qualified Amazonka.Prelude as Prelude

-- | A launch profile initialization contains information required for a
-- workstation or server to connect to a launch profile.
--
-- This includes scripts, endpoints, security groups, subnets, and other
-- configuration.
--
-- /See:/ 'newLaunchProfileInitialization' smart constructor.
data LaunchProfileInitialization = LaunchProfileInitialization'
  { -- | A @LaunchProfileInitializationActiveDirectory@ resource.
    activeDirectory :: Prelude.Maybe LaunchProfileInitializationActiveDirectory,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The launch purpose.
    launchPurpose :: Prelude.Maybe Prelude.Text,
    -- | The name for the launch profile.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The platform of the launch platform, either Windows or Linux.
    platform :: Prelude.Maybe LaunchProfilePlatform,
    -- | The system initializtion scripts.
    systemInitializationScripts :: Prelude.Maybe [LaunchProfileInitializationScript],
    -- | The user initializtion scripts.
    userInitializationScripts :: Prelude.Maybe [LaunchProfileInitializationScript]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileInitialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectory', 'launchProfileInitialization_activeDirectory' - A @LaunchProfileInitializationActiveDirectory@ resource.
--
-- 'ec2SecurityGroupIds', 'launchProfileInitialization_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'launchProfileId', 'launchProfileInitialization_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'launchProfileProtocolVersion', 'launchProfileInitialization_launchProfileProtocolVersion' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'launchPurpose', 'launchProfileInitialization_launchPurpose' - The launch purpose.
--
-- 'name', 'launchProfileInitialization_name' - The name for the launch profile.
--
-- 'platform', 'launchProfileInitialization_platform' - The platform of the launch platform, either Windows or Linux.
--
-- 'systemInitializationScripts', 'launchProfileInitialization_systemInitializationScripts' - The system initializtion scripts.
--
-- 'userInitializationScripts', 'launchProfileInitialization_userInitializationScripts' - The user initializtion scripts.
newLaunchProfileInitialization ::
  LaunchProfileInitialization
newLaunchProfileInitialization =
  LaunchProfileInitialization'
    { activeDirectory =
        Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      launchProfileProtocolVersion = Prelude.Nothing,
      launchPurpose = Prelude.Nothing,
      name = Prelude.Nothing,
      platform = Prelude.Nothing,
      systemInitializationScripts = Prelude.Nothing,
      userInitializationScripts = Prelude.Nothing
    }

-- | A @LaunchProfileInitializationActiveDirectory@ resource.
launchProfileInitialization_activeDirectory :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe LaunchProfileInitializationActiveDirectory)
launchProfileInitialization_activeDirectory = Lens.lens (\LaunchProfileInitialization' {activeDirectory} -> activeDirectory) (\s@LaunchProfileInitialization' {} a -> s {activeDirectory = a} :: LaunchProfileInitialization)

-- | The EC2 security groups that control access to the studio component.
launchProfileInitialization_ec2SecurityGroupIds :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchProfileInitialization_ec2SecurityGroupIds = Lens.lens (\LaunchProfileInitialization' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@LaunchProfileInitialization' {} a -> s {ec2SecurityGroupIds = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the launch profile used to control access from the streaming
-- session.
launchProfileInitialization_launchProfileId :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchProfileId = Lens.lens (\LaunchProfileInitialization' {launchProfileId} -> launchProfileId) (\s@LaunchProfileInitialization' {} a -> s {launchProfileId = a} :: LaunchProfileInitialization)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
launchProfileInitialization_launchProfileProtocolVersion :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchProfileProtocolVersion = Lens.lens (\LaunchProfileInitialization' {launchProfileProtocolVersion} -> launchProfileProtocolVersion) (\s@LaunchProfileInitialization' {} a -> s {launchProfileProtocolVersion = a} :: LaunchProfileInitialization)

-- | The launch purpose.
launchProfileInitialization_launchPurpose :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_launchPurpose = Lens.lens (\LaunchProfileInitialization' {launchPurpose} -> launchPurpose) (\s@LaunchProfileInitialization' {} a -> s {launchPurpose = a} :: LaunchProfileInitialization)

-- | The name for the launch profile.
launchProfileInitialization_name :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe Prelude.Text)
launchProfileInitialization_name = Lens.lens (\LaunchProfileInitialization' {name} -> name) (\s@LaunchProfileInitialization' {} a -> s {name = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Data._Sensitive

-- | The platform of the launch platform, either Windows or Linux.
launchProfileInitialization_platform :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe LaunchProfilePlatform)
launchProfileInitialization_platform = Lens.lens (\LaunchProfileInitialization' {platform} -> platform) (\s@LaunchProfileInitialization' {} a -> s {platform = a} :: LaunchProfileInitialization)

-- | The system initializtion scripts.
launchProfileInitialization_systemInitializationScripts :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe [LaunchProfileInitializationScript])
launchProfileInitialization_systemInitializationScripts = Lens.lens (\LaunchProfileInitialization' {systemInitializationScripts} -> systemInitializationScripts) (\s@LaunchProfileInitialization' {} a -> s {systemInitializationScripts = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

-- | The user initializtion scripts.
launchProfileInitialization_userInitializationScripts :: Lens.Lens' LaunchProfileInitialization (Prelude.Maybe [LaunchProfileInitializationScript])
launchProfileInitialization_userInitializationScripts = Lens.lens (\LaunchProfileInitialization' {userInitializationScripts} -> userInitializationScripts) (\s@LaunchProfileInitialization' {} a -> s {userInitializationScripts = a} :: LaunchProfileInitialization) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LaunchProfileInitialization where
  parseJSON =
    Data.withObject
      "LaunchProfileInitialization"
      ( \x ->
          LaunchProfileInitialization'
            Prelude.<$> (x Data..:? "activeDirectory")
            Prelude.<*> (x Data..:? "ec2SecurityGroupIds")
            Prelude.<*> (x Data..:? "launchProfileId")
            Prelude.<*> (x Data..:? "launchProfileProtocolVersion")
            Prelude.<*> (x Data..:? "launchPurpose")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> ( x
                            Data..:? "systemInitializationScripts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "userInitializationScripts"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LaunchProfileInitialization where
  hashWithSalt _salt LaunchProfileInitialization' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectory
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` launchProfileProtocolVersion
      `Prelude.hashWithSalt` launchPurpose
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` systemInitializationScripts
      `Prelude.hashWithSalt` userInitializationScripts

instance Prelude.NFData LaunchProfileInitialization where
  rnf LaunchProfileInitialization' {..} =
    Prelude.rnf activeDirectory `Prelude.seq`
      Prelude.rnf ec2SecurityGroupIds `Prelude.seq`
        Prelude.rnf launchProfileId `Prelude.seq`
          Prelude.rnf launchProfileProtocolVersion `Prelude.seq`
            Prelude.rnf launchPurpose `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf platform `Prelude.seq`
                  Prelude.rnf systemInitializationScripts `Prelude.seq`
                    Prelude.rnf userInitializationScripts
