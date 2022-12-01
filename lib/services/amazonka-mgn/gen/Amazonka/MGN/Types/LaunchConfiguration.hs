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
-- Module      : Amazonka.MGN.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.BootMode
import Amazonka.MGN.Types.LaunchDisposition
import Amazonka.MGN.Types.Licensing
import Amazonka.MGN.Types.PostLaunchActions
import Amazonka.MGN.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | Launch configuration name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration Target instance type right sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Copy Tags during Launch Configuration.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Launch disposition for launch configuration.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Launch configuration EC2 Launch template ID.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration boot mode.
    bootMode :: Prelude.Maybe BootMode,
    -- | Launch configuration Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration OS licensing.
    licensing :: Prelude.Maybe Licensing,
    -- | Copy Private IP during Launch Configuration.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'launchConfiguration_name' - Launch configuration name.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfiguration_targetInstanceTypeRightSizingMethod' - Launch configuration Target instance type right sizing method.
--
-- 'copyTags', 'launchConfiguration_copyTags' - Copy Tags during Launch Configuration.
--
-- 'launchDisposition', 'launchConfiguration_launchDisposition' - Launch disposition for launch configuration.
--
-- 'postLaunchActions', 'launchConfiguration_postLaunchActions' - Undocumented member.
--
-- 'ec2LaunchTemplateID', 'launchConfiguration_ec2LaunchTemplateID' - Launch configuration EC2 Launch template ID.
--
-- 'bootMode', 'launchConfiguration_bootMode' - Launch configuration boot mode.
--
-- 'sourceServerID', 'launchConfiguration_sourceServerID' - Launch configuration Source Server ID.
--
-- 'licensing', 'launchConfiguration_licensing' - Launch configuration OS licensing.
--
-- 'copyPrivateIp', 'launchConfiguration_copyPrivateIp' - Copy Private IP during Launch Configuration.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration'
    { name = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing,
      copyTags = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      postLaunchActions = Prelude.Nothing,
      ec2LaunchTemplateID = Prelude.Nothing,
      bootMode = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      licensing = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing
    }

-- | Launch configuration name.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

-- | Launch configuration Target instance type right sizing method.
launchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfiguration)

-- | Copy Tags during Launch Configuration.
launchConfiguration_copyTags :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyTags = Lens.lens (\LaunchConfiguration' {copyTags} -> copyTags) (\s@LaunchConfiguration' {} a -> s {copyTags = a} :: LaunchConfiguration)

-- | Launch disposition for launch configuration.
launchConfiguration_launchDisposition :: Lens.Lens' LaunchConfiguration (Prelude.Maybe LaunchDisposition)
launchConfiguration_launchDisposition = Lens.lens (\LaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@LaunchConfiguration' {} a -> s {launchDisposition = a} :: LaunchConfiguration)

-- | Undocumented member.
launchConfiguration_postLaunchActions :: Lens.Lens' LaunchConfiguration (Prelude.Maybe PostLaunchActions)
launchConfiguration_postLaunchActions = Lens.lens (\LaunchConfiguration' {postLaunchActions} -> postLaunchActions) (\s@LaunchConfiguration' {} a -> s {postLaunchActions = a} :: LaunchConfiguration)

-- | Launch configuration EC2 Launch template ID.
launchConfiguration_ec2LaunchTemplateID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_ec2LaunchTemplateID = Lens.lens (\LaunchConfiguration' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@LaunchConfiguration' {} a -> s {ec2LaunchTemplateID = a} :: LaunchConfiguration)

-- | Launch configuration boot mode.
launchConfiguration_bootMode :: Lens.Lens' LaunchConfiguration (Prelude.Maybe BootMode)
launchConfiguration_bootMode = Lens.lens (\LaunchConfiguration' {bootMode} -> bootMode) (\s@LaunchConfiguration' {} a -> s {bootMode = a} :: LaunchConfiguration)

-- | Launch configuration Source Server ID.
launchConfiguration_sourceServerID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_sourceServerID = Lens.lens (\LaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@LaunchConfiguration' {} a -> s {sourceServerID = a} :: LaunchConfiguration)

-- | Launch configuration OS licensing.
launchConfiguration_licensing :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Licensing)
launchConfiguration_licensing = Lens.lens (\LaunchConfiguration' {licensing} -> licensing) (\s@LaunchConfiguration' {} a -> s {licensing = a} :: LaunchConfiguration)

-- | Copy Private IP during Launch Configuration.
launchConfiguration_copyPrivateIp :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyPrivateIp = Lens.lens (\LaunchConfiguration' {copyPrivateIp} -> copyPrivateIp) (\s@LaunchConfiguration' {} a -> s {copyPrivateIp = a} :: LaunchConfiguration)

instance Core.FromJSON LaunchConfiguration where
  parseJSON =
    Core.withObject
      "LaunchConfiguration"
      ( \x ->
          LaunchConfiguration'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "targetInstanceTypeRightSizingMethod")
            Prelude.<*> (x Core..:? "copyTags")
            Prelude.<*> (x Core..:? "launchDisposition")
            Prelude.<*> (x Core..:? "postLaunchActions")
            Prelude.<*> (x Core..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Core..:? "bootMode")
            Prelude.<*> (x Core..:? "sourceServerID")
            Prelude.<*> (x Core..:? "licensing")
            Prelude.<*> (x Core..:? "copyPrivateIp")
      )

instance Prelude.Hashable LaunchConfiguration where
  hashWithSalt _salt LaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` ec2LaunchTemplateID
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` copyPrivateIp

instance Prelude.NFData LaunchConfiguration where
  rnf LaunchConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf ec2LaunchTemplateID
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf copyPrivateIp
