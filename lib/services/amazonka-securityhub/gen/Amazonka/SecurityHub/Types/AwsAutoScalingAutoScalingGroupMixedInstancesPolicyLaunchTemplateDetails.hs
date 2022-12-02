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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails

-- | Describes a launch template and overrides for a mixed instances policy.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails'
  { -- | The launch template to use for a mixed instances policy.
    launchTemplateSpecification :: Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification,
    -- | Property values to use to override the values in the launch template.
    overrides :: Prelude.Maybe [AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateSpecification', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification' - The launch template to use for a mixed instances policy.
--
-- 'overrides', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides' - Property values to use to override the values in the launch template.
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails'
    { launchTemplateSpecification =
        Prelude.Nothing,
      overrides =
        Prelude.Nothing
    }

-- | The launch template to use for a mixed instances policy.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {} a -> s {launchTemplateSpecification = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails)

-- | Property values to use to override the values in the launch template.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails (Prelude.Maybe [AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails])
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {overrides} -> overrides) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {} a -> s {overrides = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails'
            Prelude.<$> (x Data..:? "LaunchTemplateSpecification")
              Prelude.<*> (x Data..:? "Overrides" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {..} =
      _salt
        `Prelude.hashWithSalt` launchTemplateSpecification
        `Prelude.hashWithSalt` overrides

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {..} =
      Prelude.rnf launchTemplateSpecification
        `Prelude.seq` Prelude.rnf overrides

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("LaunchTemplateSpecification" Data..=)
                Prelude.<$> launchTemplateSpecification,
              ("Overrides" Data..=) Prelude.<$> overrides
            ]
        )
