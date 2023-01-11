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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails

-- | Provides details about an auto scaling group.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupDetails = AwsAutoScalingAutoScalingGroupDetails'
  { -- | The list of Availability Zones for the automatic scaling group.
    availabilityZones :: Prelude.Maybe [AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails],
    -- | Indicates whether capacity rebalancing is enabled.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the auto scaling group was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before it checks the health status of an EC2 instance that has come into
    -- service.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | The service to use for the health checks. Valid values are @EC2@ or
    -- @ELB@.
    healthCheckType :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The launch template to use.
    launchTemplate :: Prelude.Maybe AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification,
    -- | The list of load balancers associated with the group.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | The mixed instances policy for the automatic scaling group.
    mixedInstancesPolicy :: Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
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
-- 'availabilityZones', 'awsAutoScalingAutoScalingGroupDetails_availabilityZones' - The list of Availability Zones for the automatic scaling group.
--
-- 'capacityRebalance', 'awsAutoScalingAutoScalingGroupDetails_capacityRebalance' - Indicates whether capacity rebalancing is enabled.
--
-- 'createdTime', 'awsAutoScalingAutoScalingGroupDetails_createdTime' - Indicates when the auto scaling group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'healthCheckGracePeriod', 'awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before it checks the health status of an EC2 instance that has come into
-- service.
--
-- 'healthCheckType', 'awsAutoScalingAutoScalingGroupDetails_healthCheckType' - The service to use for the health checks. Valid values are @EC2@ or
-- @ELB@.
--
-- 'launchConfigurationName', 'awsAutoScalingAutoScalingGroupDetails_launchConfigurationName' - The name of the launch configuration.
--
-- 'launchTemplate', 'awsAutoScalingAutoScalingGroupDetails_launchTemplate' - The launch template to use.
--
-- 'loadBalancerNames', 'awsAutoScalingAutoScalingGroupDetails_loadBalancerNames' - The list of load balancers associated with the group.
--
-- 'mixedInstancesPolicy', 'awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy' - The mixed instances policy for the automatic scaling group.
newAwsAutoScalingAutoScalingGroupDetails ::
  AwsAutoScalingAutoScalingGroupDetails
newAwsAutoScalingAutoScalingGroupDetails =
  AwsAutoScalingAutoScalingGroupDetails'
    { availabilityZones =
        Prelude.Nothing,
      capacityRebalance = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      healthCheckGracePeriod =
        Prelude.Nothing,
      healthCheckType = Prelude.Nothing,
      launchConfigurationName =
        Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      loadBalancerNames = Prelude.Nothing,
      mixedInstancesPolicy =
        Prelude.Nothing
    }

-- | The list of Availability Zones for the automatic scaling group.
awsAutoScalingAutoScalingGroupDetails_availabilityZones :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe [AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails])
awsAutoScalingAutoScalingGroupDetails_availabilityZones = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {availabilityZones} -> availabilityZones) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {availabilityZones = a} :: AwsAutoScalingAutoScalingGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether capacity rebalancing is enabled.
awsAutoScalingAutoScalingGroupDetails_capacityRebalance :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingAutoScalingGroupDetails_capacityRebalance = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {capacityRebalance} -> capacityRebalance) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {capacityRebalance = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | Indicates when the auto scaling group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsAutoScalingAutoScalingGroupDetails_createdTime :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_createdTime = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {createdTime} -> createdTime) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {createdTime = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before it checks the health status of an EC2 instance that has come into
-- service.
awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {healthCheckGracePeriod = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The service to use for the health checks. Valid values are @EC2@ or
-- @ELB@.
awsAutoScalingAutoScalingGroupDetails_healthCheckType :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_healthCheckType = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {healthCheckType} -> healthCheckType) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {healthCheckType = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The name of the launch configuration.
awsAutoScalingAutoScalingGroupDetails_launchConfigurationName :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupDetails_launchConfigurationName = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {launchConfigurationName} -> launchConfigurationName) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {launchConfigurationName = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The launch template to use.
awsAutoScalingAutoScalingGroupDetails_launchTemplate :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification)
awsAutoScalingAutoScalingGroupDetails_launchTemplate = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {launchTemplate} -> launchTemplate) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {launchTemplate = a} :: AwsAutoScalingAutoScalingGroupDetails)

-- | The list of load balancers associated with the group.
awsAutoScalingAutoScalingGroupDetails_loadBalancerNames :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe [Prelude.Text])
awsAutoScalingAutoScalingGroupDetails_loadBalancerNames = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {loadBalancerNames} -> loadBalancerNames) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {loadBalancerNames = a} :: AwsAutoScalingAutoScalingGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The mixed instances policy for the automatic scaling group.
awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy :: Lens.Lens' AwsAutoScalingAutoScalingGroupDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails)
awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy = Lens.lens (\AwsAutoScalingAutoScalingGroupDetails' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AwsAutoScalingAutoScalingGroupDetails' {} a -> s {mixedInstancesPolicy = a} :: AwsAutoScalingAutoScalingGroupDetails)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupDetails'
            Prelude.<$> ( x Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CapacityRebalance")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "HealthCheckGracePeriod")
            Prelude.<*> (x Data..:? "HealthCheckType")
            Prelude.<*> (x Data..:? "LaunchConfigurationName")
            Prelude.<*> (x Data..:? "LaunchTemplate")
            Prelude.<*> ( x Data..:? "LoadBalancerNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MixedInstancesPolicy")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupDetails' {..} =
      _salt `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` capacityRebalance
        `Prelude.hashWithSalt` createdTime
        `Prelude.hashWithSalt` healthCheckGracePeriod
        `Prelude.hashWithSalt` healthCheckType
        `Prelude.hashWithSalt` launchConfigurationName
        `Prelude.hashWithSalt` launchTemplate
        `Prelude.hashWithSalt` loadBalancerNames
        `Prelude.hashWithSalt` mixedInstancesPolicy

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupDetails
  where
  rnf AwsAutoScalingAutoScalingGroupDetails' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf capacityRebalance
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf healthCheckGracePeriod
      `Prelude.seq` Prelude.rnf healthCheckType
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf loadBalancerNames
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupDetails
  where
  toJSON AwsAutoScalingAutoScalingGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("CapacityRebalance" Data..=)
              Prelude.<$> capacityRebalance,
            ("CreatedTime" Data..=) Prelude.<$> createdTime,
            ("HealthCheckGracePeriod" Data..=)
              Prelude.<$> healthCheckGracePeriod,
            ("HealthCheckType" Data..=)
              Prelude.<$> healthCheckType,
            ("LaunchConfigurationName" Data..=)
              Prelude.<$> launchConfigurationName,
            ("LaunchTemplate" Data..=)
              Prelude.<$> launchTemplate,
            ("LoadBalancerNames" Data..=)
              Prelude.<$> loadBalancerNames,
            ("MixedInstancesPolicy" Data..=)
              Prelude.<$> mixedInstancesPolicy
          ]
      )
