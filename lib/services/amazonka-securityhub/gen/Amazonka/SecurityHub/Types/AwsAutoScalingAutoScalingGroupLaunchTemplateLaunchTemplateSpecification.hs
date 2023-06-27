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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the launch template to use.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' smart constructor.
data AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification = AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification'
  { -- | The identifier of the launch template. You must specify either
    -- @LaunchTemplateId@ or @LaunchTemplateName@.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. You must specify either
    -- @LaunchTemplateId@ or @LaunchTemplateName@.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | Identifies the version of the launch template. You can specify a version
    -- identifier, or use the values @$Latest@ or @$Default@.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId' - The identifier of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
--
-- 'launchTemplateName', 'awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName' - The name of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
--
-- 'version', 'awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version' - Identifies the version of the launch template. You can specify a version
-- identifier, or use the values @$Latest@ or @$Default@.
newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification ::
  AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification =
  AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing,
      version =
        Prelude.Nothing
    }

-- | The identifier of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId = Lens.lens (\AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification)

-- | The name of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName :: Lens.Lens' AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName = Lens.lens (\AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification)

-- | Identifies the version of the launch template. You can specify a version
-- identifier, or use the values @$Latest@ or @$Default@.
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version :: Lens.Lens' AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version = Lens.lens (\AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {version} -> version) (\s@AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {} a -> s {version = a} :: AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification"
      ( \x ->
          AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification'
            Prelude.<$> (x Data..:? "LaunchTemplateId")
            Prelude.<*> (x Data..:? "LaunchTemplateName")
            Prelude.<*> (x Data..:? "Version")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
  where
  rnf
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {..} =
      Prelude.rnf launchTemplateId
        `Prelude.seq` Prelude.rnf launchTemplateName
        `Prelude.seq` Prelude.rnf version

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
  where
  toJSON
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("LaunchTemplateId" Data..=)
                Prelude.<$> launchTemplateId,
              ("LaunchTemplateName" Data..=)
                Prelude.<$> launchTemplateName,
              ("Version" Data..=) Prelude.<$> version
            ]
        )
