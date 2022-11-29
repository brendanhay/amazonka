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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails

-- | Provides details about an auto scaling group.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupDetails = AwsAutoScalingAutoScalingGroupDetails'
  { -- | Indicates when the auto scaling group was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The list of load balancers associated with the group.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | The list of Availability Zones for the automatic scaling group.
    availabilityZones :: Prelude.Maybe [AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails],
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before it checks the health status of an EC2 instance that has come into
    -- service.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | The launch template to use.
    launchTemplate :: Prelude.Maybe AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification,
    -- | The name of the launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The mixed instances policy for the automatic scaling group.
    mixedInstancesPolicy :: Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails,
    -- | The service to use for the health checks. Valid values are @EC2@ or
    -- @ELB@.
    healthCheckType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether capacity rebalancing is enabled.
    capacityRebalance :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'awsAutoScalingAutoScalingGroupDetails_createdTime' - Indicates when the auto scaling group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'loadBalancerNames', 'awsAutoScalingAutoScalingGroupDetails_loadBalancerNames' - The list of load balancers associated with the group.
--
-- 'availabilityZones', 'awsAutoScalingAutoScalingGroupDetails_availabilityZones' - The list of Availability Zones for the automatic scaling group.
--
-- 'healthCheckGracePeriod', 'awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before it checks the health status of an EC2 instance that has come into
-- service.
--
-- 'launchTemplate', 'awsAutoScalingAutoScalingGroupDetails_launchTemplate' - The launch template to use.
--
-- 'launchConfigurationName', 'awsAutoScalingAutoScalingGroupDetails_launchConfigurationName' - The name of the launch configuration.
--
-- 'mixedInstancesPolicy', 'awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy' - The mixed instances policy for the automatic scaling group.
--
-- 'healthCheckType', 'awsAutoScalingAutoScalingGroupDetails_healthCheckType' - The service to use for the health checks. Valid values are @EC2@ or
-- @ELB@.
--
-- 'capacityRebalance', 'awsAutoScalingAutoScalingGroupDetails_capacityRebalance' - Indicates whether capacity rebalancing is enabled.
newAwsAutoScalingAutoScalingGroupDetails ::
  AwsAutoScalingAutoScalingGroupDetails
newAwsAutoScalingAutoScalingGroupDetails =
  AwsAutoScalingAutoScalingGroupDetails'
    { createdTime =
        Prelude.Nothing,
      loadBalancerNames = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      healthCheckGracePeriod =
        Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      launchConfigurationName =
        Prelude.Nothing,
      mixedInstancesPolicy =
        Prelude.Nothing,
      healthCheckType = Prelude.Nothing,
      capacityRebalance = Prelude.Nothing
    }

-- | Indicates when the auto scaling group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsAutoScalingAutoScalingGroupDetails_createdTime :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_createdTime = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {createdTime} -> createdTime) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {createdTime = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The list of load balancers associated with the group.
awsAutoScalingAutoScalingGroupDetails_loadBalancerNames :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe [Prelude.Text])
awsAutoScalingAutoScalingGroupDetails_loadBalancerNames = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {loadBalancerNames} -> loadBalancerNames) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {loadBalancerNames = a} :: AwsAutoScalingAutoScalingGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of Availability Zones for the automatic scaling group.
awsAutoScalingAutoScalingGroupDetails_availabilityZones :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe [AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails])
awsAutoScalingAutoScalingGroupDetails_availabilityZones = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {availabilityZones} -> availabilityZones) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {availabilityZones = a} :: AwsAutoScalingAutoScalingGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before it checks the health status of an EC2 instance that has come into
-- service.
awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {healthCheckGracePeriod = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The launch template to use.
awsAutoScalingAutoScalingGroupDetails_launchTemplate :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification)
awsAutoScalingAutoScalingGroupDetails_launchTemplate = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {launchTemplate} -> launchTemplate) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {launchTemplate = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The name of the launch configuration.
awsAutoScalingAutoScalingGroupDetails_launchConfigurationName :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_launchConfigurationName = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {launchConfigurationName} -> launchConfigurationName) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {launchConfigurationName = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The mixed instances policy for the automatic scaling group.
awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails)
awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {mixedInstancesPolicy = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The service to use for the health checks. Valid values are @EC2@ or
-- @ELB@.
awsAutoScalingAutoScalingGroupDetails_healthCheckType :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_healthCheckType = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {healthCheckType} -> healthCheckType) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {healthCheckType = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | Indicates whether capacity rebalancing is enabled.
awsAutoScalingAutoScalingGroupDetails_capacityRebalance :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingAutoScalingGroupDetails_capacityRebalance = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {capacityRebalance} -> capacityRebalance) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {capacityRebalance = a} :: AwsAutoScalingAutoScalingGroupDetails)

instance
  Core.FromJSON
    AwsAutoScalingAutoScalingGroupDetails
  where
  parseJSON =
    Core.withObject
      "AwsAutoScalingAutoScalingGroupDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupDetails'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> ( x Core..:? "LoadBalancerNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HealthCheckGracePeriod")
            Prelude.<*> (x Core..:? "LaunchTemplate")
            Prelude.<*> (x Core..:? "LaunchConfigurationName")
            Prelude.<*> (x Core..:? "MixedInstancesPolicy")
            Prelude.<*> (x Core..:? "HealthCheckType")
            Prelude.<*> (x Core..:? "CapacityRebalance")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupDetails' {..} =
      _salt `Prelude.hashWithSalt` createdTime
        `Prelude.hashWithSalt` loadBalancerNames
        `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` healthCheckGracePeriod
        `Prelude.hashWithSalt` launchTemplate
        `Prelude.hashWithSalt` launchConfigurationName
        `Prelude.hashWithSalt` mixedInstancesPolicy
        `Prelude.hashWithSalt` healthCheckType
        `Prelude.hashWithSalt` capacityRebalance

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupDetails
  where
  rnf AwsAutoScalingAutoScalingGroupDetails' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf loadBalancerNames
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf healthCheckGracePeriod
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy
      `Prelude.seq` Prelude.rnf healthCheckType
      `Prelude.seq` Prelude.rnf capacityRebalance

instance
  Core.ToJSON
    AwsAutoScalingAutoScalingGroupDetails
  where
  toJSON AwsAutoScalingAutoScalingGroupDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CreatedTime" Core..=) Prelude.<$> createdTime,
            ("LoadBalancerNames" Core..=)
              Prelude.<$> loadBalancerNames,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("HealthCheckGracePeriod" Core..=)
              Prelude.<$> healthCheckGracePeriod,
            ("LaunchTemplate" Core..=)
              Prelude.<$> launchTemplate,
            ("LaunchConfigurationName" Core..=)
              Prelude.<$> launchConfigurationName,
            ("MixedInstancesPolicy" Core..=)
              Prelude.<$> mixedInstancesPolicy,
            ("HealthCheckType" Core..=)
              Prelude.<$> healthCheckType,
            ("CapacityRebalance" Core..=)
              Prelude.<$> capacityRebalance
          ]
      )
