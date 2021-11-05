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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MGN.Types.LaunchDisposition
import Amazonka.MGN.Types.Licensing
import Amazonka.MGN.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | Configure EC2 lauch configuration template ID.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Configure launch configuration Target instance type right sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Configure launch dispostion for launch configuration.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Copy Tags during Launch Configuration.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Configure launch configuration name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Configure launch configuration Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Configure launch configuration OS licensing.
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
-- 'ec2LaunchTemplateID', 'launchConfiguration_ec2LaunchTemplateID' - Configure EC2 lauch configuration template ID.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfiguration_targetInstanceTypeRightSizingMethod' - Configure launch configuration Target instance type right sizing method.
--
-- 'launchDisposition', 'launchConfiguration_launchDisposition' - Configure launch dispostion for launch configuration.
--
-- 'copyTags', 'launchConfiguration_copyTags' - Copy Tags during Launch Configuration.
--
-- 'name', 'launchConfiguration_name' - Configure launch configuration name.
--
-- 'sourceServerID', 'launchConfiguration_sourceServerID' - Configure launch configuration Source Server ID.
--
-- 'licensing', 'launchConfiguration_licensing' - Configure launch configuration OS licensing.
--
-- 'copyPrivateIp', 'launchConfiguration_copyPrivateIp' - Copy Private IP during Launch Configuration.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration'
    { ec2LaunchTemplateID =
        Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      licensing = Prelude.Nothing,
      copyPrivateIp = Prelude.Nothing
    }

-- | Configure EC2 lauch configuration template ID.
launchConfiguration_ec2LaunchTemplateID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_ec2LaunchTemplateID = Lens.lens (\LaunchConfiguration' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@LaunchConfiguration' {} a -> s {ec2LaunchTemplateID = a} :: LaunchConfiguration)

-- | Configure launch configuration Target instance type right sizing method.
launchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfiguration)

-- | Configure launch dispostion for launch configuration.
launchConfiguration_launchDisposition :: Lens.Lens' LaunchConfiguration (Prelude.Maybe LaunchDisposition)
launchConfiguration_launchDisposition = Lens.lens (\LaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@LaunchConfiguration' {} a -> s {launchDisposition = a} :: LaunchConfiguration)

-- | Copy Tags during Launch Configuration.
launchConfiguration_copyTags :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyTags = Lens.lens (\LaunchConfiguration' {copyTags} -> copyTags) (\s@LaunchConfiguration' {} a -> s {copyTags = a} :: LaunchConfiguration)

-- | Configure launch configuration name.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

-- | Configure launch configuration Source Server ID.
launchConfiguration_sourceServerID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_sourceServerID = Lens.lens (\LaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@LaunchConfiguration' {} a -> s {sourceServerID = a} :: LaunchConfiguration)

-- | Configure launch configuration OS licensing.
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
            Prelude.<$> (x Core..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Core..:? "targetInstanceTypeRightSizingMethod")
            Prelude.<*> (x Core..:? "launchDisposition")
            Prelude.<*> (x Core..:? "copyTags")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "sourceServerID")
            Prelude.<*> (x Core..:? "licensing")
            Prelude.<*> (x Core..:? "copyPrivateIp")
      )

instance Prelude.Hashable LaunchConfiguration

instance Prelude.NFData LaunchConfiguration
