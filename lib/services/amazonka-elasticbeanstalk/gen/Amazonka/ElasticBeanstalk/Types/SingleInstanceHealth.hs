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
-- Module      : Amazonka.ElasticBeanstalk.Types.SingleInstanceHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.SingleInstanceHealth where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types.ApplicationMetrics
import Amazonka.ElasticBeanstalk.Types.Deployment
import Amazonka.ElasticBeanstalk.Types.SystemStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed health information about an Amazon EC2 instance in your Elastic
-- Beanstalk environment.
--
-- /See:/ 'newSingleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { -- | The ID of the Amazon EC2 instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Represents the causes, which provide more information about the current
    -- health status.
    causes :: Prelude.Maybe [Prelude.Text],
    -- | Operating system metrics from the instance.
    system :: Prelude.Maybe SystemStatus,
    -- | Request metrics from your application.
    applicationMetrics :: Prelude.Maybe ApplicationMetrics,
    -- | Represents the color indicator that gives you information about the
    -- health of the EC2 instance. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
    color :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The availability zone in which the instance runs.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Returns the health status of the specified instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
    healthStatus :: Prelude.Maybe Prelude.Text,
    -- | Information about the most recent deployment to an instance.
    deployment :: Prelude.Maybe Deployment,
    -- | The time at which the EC2 instance was launched.
    launchedAt :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleInstanceHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'singleInstanceHealth_instanceId' - The ID of the Amazon EC2 instance.
--
-- 'causes', 'singleInstanceHealth_causes' - Represents the causes, which provide more information about the current
-- health status.
--
-- 'system', 'singleInstanceHealth_system' - Operating system metrics from the instance.
--
-- 'applicationMetrics', 'singleInstanceHealth_applicationMetrics' - Request metrics from your application.
--
-- 'color', 'singleInstanceHealth_color' - Represents the color indicator that gives you information about the
-- health of the EC2 instance. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- 'instanceType', 'singleInstanceHealth_instanceType' - The instance\'s type.
--
-- 'availabilityZone', 'singleInstanceHealth_availabilityZone' - The availability zone in which the instance runs.
--
-- 'healthStatus', 'singleInstanceHealth_healthStatus' - Returns the health status of the specified instance. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- 'deployment', 'singleInstanceHealth_deployment' - Information about the most recent deployment to an instance.
--
-- 'launchedAt', 'singleInstanceHealth_launchedAt' - The time at which the EC2 instance was launched.
newSingleInstanceHealth ::
  SingleInstanceHealth
newSingleInstanceHealth =
  SingleInstanceHealth'
    { instanceId = Prelude.Nothing,
      causes = Prelude.Nothing,
      system = Prelude.Nothing,
      applicationMetrics = Prelude.Nothing,
      color = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      deployment = Prelude.Nothing,
      launchedAt = Prelude.Nothing
    }

-- | The ID of the Amazon EC2 instance.
singleInstanceHealth_instanceId :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_instanceId = Lens.lens (\SingleInstanceHealth' {instanceId} -> instanceId) (\s@SingleInstanceHealth' {} a -> s {instanceId = a} :: SingleInstanceHealth)

-- | Represents the causes, which provide more information about the current
-- health status.
singleInstanceHealth_causes :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe [Prelude.Text])
singleInstanceHealth_causes = Lens.lens (\SingleInstanceHealth' {causes} -> causes) (\s@SingleInstanceHealth' {} a -> s {causes = a} :: SingleInstanceHealth) Prelude.. Lens.mapping Lens.coerced

-- | Operating system metrics from the instance.
singleInstanceHealth_system :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe SystemStatus)
singleInstanceHealth_system = Lens.lens (\SingleInstanceHealth' {system} -> system) (\s@SingleInstanceHealth' {} a -> s {system = a} :: SingleInstanceHealth)

-- | Request metrics from your application.
singleInstanceHealth_applicationMetrics :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe ApplicationMetrics)
singleInstanceHealth_applicationMetrics = Lens.lens (\SingleInstanceHealth' {applicationMetrics} -> applicationMetrics) (\s@SingleInstanceHealth' {} a -> s {applicationMetrics = a} :: SingleInstanceHealth)

-- | Represents the color indicator that gives you information about the
-- health of the EC2 instance. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
singleInstanceHealth_color :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_color = Lens.lens (\SingleInstanceHealth' {color} -> color) (\s@SingleInstanceHealth' {} a -> s {color = a} :: SingleInstanceHealth)

-- | The instance\'s type.
singleInstanceHealth_instanceType :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_instanceType = Lens.lens (\SingleInstanceHealth' {instanceType} -> instanceType) (\s@SingleInstanceHealth' {} a -> s {instanceType = a} :: SingleInstanceHealth)

-- | The availability zone in which the instance runs.
singleInstanceHealth_availabilityZone :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_availabilityZone = Lens.lens (\SingleInstanceHealth' {availabilityZone} -> availabilityZone) (\s@SingleInstanceHealth' {} a -> s {availabilityZone = a} :: SingleInstanceHealth)

-- | Returns the health status of the specified instance. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
singleInstanceHealth_healthStatus :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_healthStatus = Lens.lens (\SingleInstanceHealth' {healthStatus} -> healthStatus) (\s@SingleInstanceHealth' {} a -> s {healthStatus = a} :: SingleInstanceHealth)

-- | Information about the most recent deployment to an instance.
singleInstanceHealth_deployment :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Deployment)
singleInstanceHealth_deployment = Lens.lens (\SingleInstanceHealth' {deployment} -> deployment) (\s@SingleInstanceHealth' {} a -> s {deployment = a} :: SingleInstanceHealth)

-- | The time at which the EC2 instance was launched.
singleInstanceHealth_launchedAt :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.UTCTime)
singleInstanceHealth_launchedAt = Lens.lens (\SingleInstanceHealth' {launchedAt} -> launchedAt) (\s@SingleInstanceHealth' {} a -> s {launchedAt = a} :: SingleInstanceHealth) Prelude.. Lens.mapping Core._Time

instance Core.FromXML SingleInstanceHealth where
  parseXML x =
    SingleInstanceHealth'
      Prelude.<$> (x Core..@? "InstanceId")
      Prelude.<*> ( x Core..@? "Causes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "System")
      Prelude.<*> (x Core..@? "ApplicationMetrics")
      Prelude.<*> (x Core..@? "Color")
      Prelude.<*> (x Core..@? "InstanceType")
      Prelude.<*> (x Core..@? "AvailabilityZone")
      Prelude.<*> (x Core..@? "HealthStatus")
      Prelude.<*> (x Core..@? "Deployment")
      Prelude.<*> (x Core..@? "LaunchedAt")

instance Prelude.Hashable SingleInstanceHealth where
  hashWithSalt salt' SingleInstanceHealth' {..} =
    salt' `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` deployment
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` applicationMetrics
      `Prelude.hashWithSalt` system
      `Prelude.hashWithSalt` causes
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SingleInstanceHealth where
  rnf SingleInstanceHealth' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf launchedAt
      `Prelude.seq` Prelude.rnf deployment
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf color
      `Prelude.seq` Prelude.rnf applicationMetrics
      `Prelude.seq` Prelude.rnf system
      `Prelude.seq` Prelude.rnf causes
