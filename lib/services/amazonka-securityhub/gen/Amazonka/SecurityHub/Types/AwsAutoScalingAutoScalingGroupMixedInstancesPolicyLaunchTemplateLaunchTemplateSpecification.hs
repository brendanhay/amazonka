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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the launch template to use for a mixed instances policy.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification'
  { -- | The identifier of the launch template. You must specify either
    -- @LaunchTemplateId@ or @LaunchTemplateName@.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the version of the launch template. You can specify a version
    -- identifier, or use the values @$Latest@ or @$Default@.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. You must specify either
    -- @LaunchTemplateId@ or @LaunchTemplateName@.
    launchTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId' - The identifier of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
--
-- 'version', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version' - Identifies the version of the launch template. You can specify a version
-- identifier, or use the values @$Latest@ or @$Default@.
--
-- 'launchTemplateName', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName' - The name of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification'
    { launchTemplateId =
        Prelude.Nothing,
      version =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing
    }

-- | The identifier of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification)

-- | Identifies the version of the launch template. You can specify a version
-- identifier, or use the values @$Latest@ or @$Default@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {version} -> version) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {} a -> s {version = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification)

-- | The name of the launch template. You must specify either
-- @LaunchTemplateId@ or @LaunchTemplateName@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification'
            Prelude.<$> (x Data..:? "LaunchTemplateId")
              Prelude.<*> (x Data..:? "Version")
              Prelude.<*> (x Data..:? "LaunchTemplateName")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` version
        `Prelude.hashWithSalt` launchTemplateName

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {..} =
      Prelude.rnf launchTemplateId
        `Prelude.seq` Prelude.rnf version
        `Prelude.seq` Prelude.rnf launchTemplateName

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("LaunchTemplateId" Data..=)
                Prelude.<$> launchTemplateId,
              ("Version" Data..=) Prelude.<$> version,
              ("LaunchTemplateName" Data..=)
                Prelude.<$> launchTemplateName
            ]
        )
