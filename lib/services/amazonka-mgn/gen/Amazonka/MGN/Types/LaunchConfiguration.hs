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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.BootMode
import Amazonka.MGN.Types.LaunchDisposition
import Amazonka.MGN.Types.Licensing
import Amazonka.MGN.Types.PostLaunchActions
import Amazonka.MGN.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | Launch configuration boot mode.
    bootMode :: Prelude.Maybe BootMode,
    -- | Copy Private IP during Launch Configuration.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Copy Tags during Launch Configuration.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Launch configuration EC2 Launch template ID.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Enable map auto tagging.
    enableMapAutoTagging :: Prelude.Maybe Prelude.Bool,
    -- | Launch disposition for launch configuration.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Launch configuration OS licensing.
    licensing :: Prelude.Maybe Licensing,
    -- | Map auto tagging MPE ID.
    mapAutoTaggingMpeID :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration name.
    name :: Prelude.Maybe Prelude.Text,
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Launch configuration Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration Target instance type right sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod
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
-- 'bootMode', 'launchConfiguration_bootMode' - Launch configuration boot mode.
--
-- 'copyPrivateIp', 'launchConfiguration_copyPrivateIp' - Copy Private IP during Launch Configuration.
--
-- 'copyTags', 'launchConfiguration_copyTags' - Copy Tags during Launch Configuration.
--
-- 'ec2LaunchTemplateID', 'launchConfiguration_ec2LaunchTemplateID' - Launch configuration EC2 Launch template ID.
--
-- 'enableMapAutoTagging', 'launchConfiguration_enableMapAutoTagging' - Enable map auto tagging.
--
-- 'launchDisposition', 'launchConfiguration_launchDisposition' - Launch disposition for launch configuration.
--
-- 'licensing', 'launchConfiguration_licensing' - Launch configuration OS licensing.
--
-- 'mapAutoTaggingMpeID', 'launchConfiguration_mapAutoTaggingMpeID' - Map auto tagging MPE ID.
--
-- 'name', 'launchConfiguration_name' - Launch configuration name.
--
-- 'postLaunchActions', 'launchConfiguration_postLaunchActions' - Undocumented member.
--
-- 'sourceServerID', 'launchConfiguration_sourceServerID' - Launch configuration Source Server ID.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfiguration_targetInstanceTypeRightSizingMethod' - Launch configuration Target instance type right sizing method.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration'
    { bootMode = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      ec2LaunchTemplateID = Prelude.Nothing,
      enableMapAutoTagging = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      licensing = Prelude.Nothing,
      mapAutoTaggingMpeID = Prelude.Nothing,
      name = Prelude.Nothing,
      postLaunchActions = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing
    }

-- | Launch configuration boot mode.
launchConfiguration_bootMode :: Lens.Lens' LaunchConfiguration (Prelude.Maybe BootMode)
launchConfiguration_bootMode = Lens.lens (\LaunchConfiguration' {bootMode} -> bootMode) (\s@LaunchConfiguration' {} a -> s {bootMode = a} :: LaunchConfiguration)

-- | Copy Private IP during Launch Configuration.
launchConfiguration_copyPrivateIp :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyPrivateIp = Lens.lens (\LaunchConfiguration' {copyPrivateIp} -> copyPrivateIp) (\s@LaunchConfiguration' {} a -> s {copyPrivateIp = a} :: LaunchConfiguration)

-- | Copy Tags during Launch Configuration.
launchConfiguration_copyTags :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyTags = Lens.lens (\LaunchConfiguration' {copyTags} -> copyTags) (\s@LaunchConfiguration' {} a -> s {copyTags = a} :: LaunchConfiguration)

-- | Launch configuration EC2 Launch template ID.
launchConfiguration_ec2LaunchTemplateID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_ec2LaunchTemplateID = Lens.lens (\LaunchConfiguration' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@LaunchConfiguration' {} a -> s {ec2LaunchTemplateID = a} :: LaunchConfiguration)

-- | Enable map auto tagging.
launchConfiguration_enableMapAutoTagging :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_enableMapAutoTagging = Lens.lens (\LaunchConfiguration' {enableMapAutoTagging} -> enableMapAutoTagging) (\s@LaunchConfiguration' {} a -> s {enableMapAutoTagging = a} :: LaunchConfiguration)

-- | Launch disposition for launch configuration.
launchConfiguration_launchDisposition :: Lens.Lens' LaunchConfiguration (Prelude.Maybe LaunchDisposition)
launchConfiguration_launchDisposition = Lens.lens (\LaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@LaunchConfiguration' {} a -> s {launchDisposition = a} :: LaunchConfiguration)

-- | Launch configuration OS licensing.
launchConfiguration_licensing :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Licensing)
launchConfiguration_licensing = Lens.lens (\LaunchConfiguration' {licensing} -> licensing) (\s@LaunchConfiguration' {} a -> s {licensing = a} :: LaunchConfiguration)

-- | Map auto tagging MPE ID.
launchConfiguration_mapAutoTaggingMpeID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_mapAutoTaggingMpeID = Lens.lens (\LaunchConfiguration' {mapAutoTaggingMpeID} -> mapAutoTaggingMpeID) (\s@LaunchConfiguration' {} a -> s {mapAutoTaggingMpeID = a} :: LaunchConfiguration)

-- | Launch configuration name.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

-- | Undocumented member.
launchConfiguration_postLaunchActions :: Lens.Lens' LaunchConfiguration (Prelude.Maybe PostLaunchActions)
launchConfiguration_postLaunchActions = Lens.lens (\LaunchConfiguration' {postLaunchActions} -> postLaunchActions) (\s@LaunchConfiguration' {} a -> s {postLaunchActions = a} :: LaunchConfiguration)

-- | Launch configuration Source Server ID.
launchConfiguration_sourceServerID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_sourceServerID = Lens.lens (\LaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@LaunchConfiguration' {} a -> s {sourceServerID = a} :: LaunchConfiguration)

-- | Launch configuration Target instance type right sizing method.
launchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfiguration)

instance Data.FromJSON LaunchConfiguration where
  parseJSON =
    Data.withObject
      "LaunchConfiguration"
      ( \x ->
          LaunchConfiguration'
            Prelude.<$> (x Data..:? "bootMode")
            Prelude.<*> (x Data..:? "copyPrivateIp")
            Prelude.<*> (x Data..:? "copyTags")
            Prelude.<*> (x Data..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Data..:? "enableMapAutoTagging")
            Prelude.<*> (x Data..:? "launchDisposition")
            Prelude.<*> (x Data..:? "licensing")
            Prelude.<*> (x Data..:? "mapAutoTaggingMpeID")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "postLaunchActions")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "targetInstanceTypeRightSizingMethod")
      )

instance Prelude.Hashable LaunchConfiguration where
  hashWithSalt _salt LaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` ec2LaunchTemplateID
      `Prelude.hashWithSalt` enableMapAutoTagging
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` mapAutoTaggingMpeID
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod

instance Prelude.NFData LaunchConfiguration where
  rnf LaunchConfiguration' {..} =
    Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf ec2LaunchTemplateID
      `Prelude.seq` Prelude.rnf enableMapAutoTagging
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf mapAutoTaggingMpeID
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf
        targetInstanceTypeRightSizingMethod
