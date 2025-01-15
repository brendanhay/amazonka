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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.SingleInstanceHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ApplicationMetrics
import Amazonka.ElasticBeanstalk.Types.Deployment
import Amazonka.ElasticBeanstalk.Types.SystemStatus
import qualified Amazonka.Prelude as Prelude

-- | Detailed health information about an Amazon EC2 instance in your Elastic
-- Beanstalk environment.
--
-- /See:/ 'newSingleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { -- | Request metrics from your application.
    applicationMetrics :: Prelude.Maybe ApplicationMetrics,
    -- | The availability zone in which the instance runs.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Represents the causes, which provide more information about the current
    -- health status.
    causes :: Prelude.Maybe [Prelude.Text],
    -- | Represents the color indicator that gives you information about the
    -- health of the EC2 instance. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
    color :: Prelude.Maybe Prelude.Text,
    -- | Information about the most recent deployment to an instance.
    deployment :: Prelude.Maybe Deployment,
    -- | Returns the health status of the specified instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
    healthStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EC2 instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The time at which the EC2 instance was launched.
    launchedAt :: Prelude.Maybe Data.ISO8601,
    -- | Operating system metrics from the instance.
    system :: Prelude.Maybe SystemStatus
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
-- 'applicationMetrics', 'singleInstanceHealth_applicationMetrics' - Request metrics from your application.
--
-- 'availabilityZone', 'singleInstanceHealth_availabilityZone' - The availability zone in which the instance runs.
--
-- 'causes', 'singleInstanceHealth_causes' - Represents the causes, which provide more information about the current
-- health status.
--
-- 'color', 'singleInstanceHealth_color' - Represents the color indicator that gives you information about the
-- health of the EC2 instance. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- 'deployment', 'singleInstanceHealth_deployment' - Information about the most recent deployment to an instance.
--
-- 'healthStatus', 'singleInstanceHealth_healthStatus' - Returns the health status of the specified instance. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- 'instanceId', 'singleInstanceHealth_instanceId' - The ID of the Amazon EC2 instance.
--
-- 'instanceType', 'singleInstanceHealth_instanceType' - The instance\'s type.
--
-- 'launchedAt', 'singleInstanceHealth_launchedAt' - The time at which the EC2 instance was launched.
--
-- 'system', 'singleInstanceHealth_system' - Operating system metrics from the instance.
newSingleInstanceHealth ::
  SingleInstanceHealth
newSingleInstanceHealth =
  SingleInstanceHealth'
    { applicationMetrics =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      causes = Prelude.Nothing,
      color = Prelude.Nothing,
      deployment = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      launchedAt = Prelude.Nothing,
      system = Prelude.Nothing
    }

-- | Request metrics from your application.
singleInstanceHealth_applicationMetrics :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe ApplicationMetrics)
singleInstanceHealth_applicationMetrics = Lens.lens (\SingleInstanceHealth' {applicationMetrics} -> applicationMetrics) (\s@SingleInstanceHealth' {} a -> s {applicationMetrics = a} :: SingleInstanceHealth)

-- | The availability zone in which the instance runs.
singleInstanceHealth_availabilityZone :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_availabilityZone = Lens.lens (\SingleInstanceHealth' {availabilityZone} -> availabilityZone) (\s@SingleInstanceHealth' {} a -> s {availabilityZone = a} :: SingleInstanceHealth)

-- | Represents the causes, which provide more information about the current
-- health status.
singleInstanceHealth_causes :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe [Prelude.Text])
singleInstanceHealth_causes = Lens.lens (\SingleInstanceHealth' {causes} -> causes) (\s@SingleInstanceHealth' {} a -> s {causes = a} :: SingleInstanceHealth) Prelude.. Lens.mapping Lens.coerced

-- | Represents the color indicator that gives you information about the
-- health of the EC2 instance. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
singleInstanceHealth_color :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_color = Lens.lens (\SingleInstanceHealth' {color} -> color) (\s@SingleInstanceHealth' {} a -> s {color = a} :: SingleInstanceHealth)

-- | Information about the most recent deployment to an instance.
singleInstanceHealth_deployment :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Deployment)
singleInstanceHealth_deployment = Lens.lens (\SingleInstanceHealth' {deployment} -> deployment) (\s@SingleInstanceHealth' {} a -> s {deployment = a} :: SingleInstanceHealth)

-- | Returns the health status of the specified instance. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
singleInstanceHealth_healthStatus :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_healthStatus = Lens.lens (\SingleInstanceHealth' {healthStatus} -> healthStatus) (\s@SingleInstanceHealth' {} a -> s {healthStatus = a} :: SingleInstanceHealth)

-- | The ID of the Amazon EC2 instance.
singleInstanceHealth_instanceId :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_instanceId = Lens.lens (\SingleInstanceHealth' {instanceId} -> instanceId) (\s@SingleInstanceHealth' {} a -> s {instanceId = a} :: SingleInstanceHealth)

-- | The instance\'s type.
singleInstanceHealth_instanceType :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.Text)
singleInstanceHealth_instanceType = Lens.lens (\SingleInstanceHealth' {instanceType} -> instanceType) (\s@SingleInstanceHealth' {} a -> s {instanceType = a} :: SingleInstanceHealth)

-- | The time at which the EC2 instance was launched.
singleInstanceHealth_launchedAt :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe Prelude.UTCTime)
singleInstanceHealth_launchedAt = Lens.lens (\SingleInstanceHealth' {launchedAt} -> launchedAt) (\s@SingleInstanceHealth' {} a -> s {launchedAt = a} :: SingleInstanceHealth) Prelude.. Lens.mapping Data._Time

-- | Operating system metrics from the instance.
singleInstanceHealth_system :: Lens.Lens' SingleInstanceHealth (Prelude.Maybe SystemStatus)
singleInstanceHealth_system = Lens.lens (\SingleInstanceHealth' {system} -> system) (\s@SingleInstanceHealth' {} a -> s {system = a} :: SingleInstanceHealth)

instance Data.FromXML SingleInstanceHealth where
  parseXML x =
    SingleInstanceHealth'
      Prelude.<$> (x Data..@? "ApplicationMetrics")
      Prelude.<*> (x Data..@? "AvailabilityZone")
      Prelude.<*> ( x Data..@? "Causes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Color")
      Prelude.<*> (x Data..@? "Deployment")
      Prelude.<*> (x Data..@? "HealthStatus")
      Prelude.<*> (x Data..@? "InstanceId")
      Prelude.<*> (x Data..@? "InstanceType")
      Prelude.<*> (x Data..@? "LaunchedAt")
      Prelude.<*> (x Data..@? "System")

instance Prelude.Hashable SingleInstanceHealth where
  hashWithSalt _salt SingleInstanceHealth' {..} =
    _salt
      `Prelude.hashWithSalt` applicationMetrics
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` causes
      `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` deployment
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` system

instance Prelude.NFData SingleInstanceHealth where
  rnf SingleInstanceHealth' {..} =
    Prelude.rnf applicationMetrics `Prelude.seq`
      Prelude.rnf availabilityZone `Prelude.seq`
        Prelude.rnf causes `Prelude.seq`
          Prelude.rnf color `Prelude.seq`
            Prelude.rnf deployment `Prelude.seq`
              Prelude.rnf healthStatus `Prelude.seq`
                Prelude.rnf instanceId `Prelude.seq`
                  Prelude.rnf instanceType `Prelude.seq`
                    Prelude.rnf launchedAt `Prelude.seq`
                      Prelude.rnf system
