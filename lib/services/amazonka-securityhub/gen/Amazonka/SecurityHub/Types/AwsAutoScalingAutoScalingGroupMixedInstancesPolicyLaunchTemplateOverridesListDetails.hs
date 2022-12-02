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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Property values to use to override the values in the launch template.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails'
  { -- | The instance type. For example, @m3.xlarge@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The number of capacity units provided by the specified instance type in
    -- terms of virtual CPUs, memory, storage, throughput, or other relative
    -- performance characteristic.
    weightedCapacity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType' - The instance type. For example, @m3.xlarge@.
--
-- 'weightedCapacity', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity' - The number of capacity units provided by the specified instance type in
-- terms of virtual CPUs, memory, storage, throughput, or other relative
-- performance characteristic.
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails'
    { instanceType =
        Prelude.Nothing,
      weightedCapacity =
        Prelude.Nothing
    }

-- | The instance type. For example, @m3.xlarge@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {instanceType} -> instanceType) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {} a -> s {instanceType = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails)

-- | The number of capacity units provided by the specified instance type in
-- terms of virtual CPUs, memory, storage, throughput, or other relative
-- performance characteristic.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {weightedCapacity} -> weightedCapacity) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {} a -> s {weightedCapacity = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails'
            Prelude.<$> (x Data..:? "InstanceType")
              Prelude.<*> (x Data..:? "WeightedCapacity")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {..} =
      _salt `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` weightedCapacity

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {..} =
      Prelude.rnf instanceType
        `Prelude.seq` Prelude.rnf weightedCapacity

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("InstanceType" Data..=) Prelude.<$> instanceType,
              ("WeightedCapacity" Data..=)
                Prelude.<$> weightedCapacity
            ]
        )
