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
-- Module      : Amazonka.DrS.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.LaunchDisposition
import Amazonka.DrS.Types.Licensing
import Amazonka.DrS.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | Whether we should copy the Private IP of the Source Server to the
    -- Recovery Instance.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Whether we want to copy the tags of the Source Server to the EC2 machine
    -- of the Recovery Instance.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | The EC2 launch template ID of this launch configuration.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | The state of the Recovery Instance in EC2 after the recovery operation.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | The licensing configuration to be used for this launch configuration.
    licensing :: Prelude.Maybe Licensing,
    -- | The name of the launch configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Source Server for this launch configuration.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Whether Elastic Disaster Recovery should try to automatically choose the
    -- instance type that best matches the OS, CPU, and RAM of your Source
    -- Server.
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
-- 'copyPrivateIp', 'launchConfiguration_copyPrivateIp' - Whether we should copy the Private IP of the Source Server to the
-- Recovery Instance.
--
-- 'copyTags', 'launchConfiguration_copyTags' - Whether we want to copy the tags of the Source Server to the EC2 machine
-- of the Recovery Instance.
--
-- 'ec2LaunchTemplateID', 'launchConfiguration_ec2LaunchTemplateID' - The EC2 launch template ID of this launch configuration.
--
-- 'launchDisposition', 'launchConfiguration_launchDisposition' - The state of the Recovery Instance in EC2 after the recovery operation.
--
-- 'licensing', 'launchConfiguration_licensing' - The licensing configuration to be used for this launch configuration.
--
-- 'name', 'launchConfiguration_name' - The name of the launch configuration.
--
-- 'sourceServerID', 'launchConfiguration_sourceServerID' - The ID of the Source Server for this launch configuration.
--
-- 'targetInstanceTypeRightSizingMethod', 'launchConfiguration_targetInstanceTypeRightSizingMethod' - Whether Elastic Disaster Recovery should try to automatically choose the
-- instance type that best matches the OS, CPU, and RAM of your Source
-- Server.
newLaunchConfiguration ::
  LaunchConfiguration
newLaunchConfiguration =
  LaunchConfiguration'
    { copyPrivateIp =
        Prelude.Nothing,
      copyTags = Prelude.Nothing,
      ec2LaunchTemplateID = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      licensing = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing
    }

-- | Whether we should copy the Private IP of the Source Server to the
-- Recovery Instance.
launchConfiguration_copyPrivateIp :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyPrivateIp = Lens.lens (\LaunchConfiguration' {copyPrivateIp} -> copyPrivateIp) (\s@LaunchConfiguration' {} a -> s {copyPrivateIp = a} :: LaunchConfiguration)

-- | Whether we want to copy the tags of the Source Server to the EC2 machine
-- of the Recovery Instance.
launchConfiguration_copyTags :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_copyTags = Lens.lens (\LaunchConfiguration' {copyTags} -> copyTags) (\s@LaunchConfiguration' {} a -> s {copyTags = a} :: LaunchConfiguration)

-- | The EC2 launch template ID of this launch configuration.
launchConfiguration_ec2LaunchTemplateID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_ec2LaunchTemplateID = Lens.lens (\LaunchConfiguration' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@LaunchConfiguration' {} a -> s {ec2LaunchTemplateID = a} :: LaunchConfiguration)

-- | The state of the Recovery Instance in EC2 after the recovery operation.
launchConfiguration_launchDisposition :: Lens.Lens' LaunchConfiguration (Prelude.Maybe LaunchDisposition)
launchConfiguration_launchDisposition = Lens.lens (\LaunchConfiguration' {launchDisposition} -> launchDisposition) (\s@LaunchConfiguration' {} a -> s {launchDisposition = a} :: LaunchConfiguration)

-- | The licensing configuration to be used for this launch configuration.
launchConfiguration_licensing :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Licensing)
launchConfiguration_licensing = Lens.lens (\LaunchConfiguration' {licensing} -> licensing) (\s@LaunchConfiguration' {} a -> s {licensing = a} :: LaunchConfiguration)

-- | The name of the launch configuration.
launchConfiguration_name :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_name = Lens.lens (\LaunchConfiguration' {name} -> name) (\s@LaunchConfiguration' {} a -> s {name = a} :: LaunchConfiguration)

-- | The ID of the Source Server for this launch configuration.
launchConfiguration_sourceServerID :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_sourceServerID = Lens.lens (\LaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@LaunchConfiguration' {} a -> s {sourceServerID = a} :: LaunchConfiguration)

-- | Whether Elastic Disaster Recovery should try to automatically choose the
-- instance type that best matches the OS, CPU, and RAM of your Source
-- Server.
launchConfiguration_targetInstanceTypeRightSizingMethod :: Lens.Lens' LaunchConfiguration (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
launchConfiguration_targetInstanceTypeRightSizingMethod = Lens.lens (\LaunchConfiguration' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@LaunchConfiguration' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: LaunchConfiguration)

instance Data.FromJSON LaunchConfiguration where
  parseJSON =
    Data.withObject
      "LaunchConfiguration"
      ( \x ->
          LaunchConfiguration'
            Prelude.<$> (x Data..:? "copyPrivateIp")
            Prelude.<*> (x Data..:? "copyTags")
            Prelude.<*> (x Data..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Data..:? "launchDisposition")
            Prelude.<*> (x Data..:? "licensing")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "targetInstanceTypeRightSizingMethod")
      )

instance Prelude.Hashable LaunchConfiguration where
  hashWithSalt _salt LaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` copyPrivateIp
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` ec2LaunchTemplateID
      `Prelude.hashWithSalt` launchDisposition
      `Prelude.hashWithSalt` licensing
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod

instance Prelude.NFData LaunchConfiguration where
  rnf LaunchConfiguration' {..} =
    Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf ec2LaunchTemplateID
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
