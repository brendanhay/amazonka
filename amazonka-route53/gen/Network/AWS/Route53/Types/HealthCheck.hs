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
-- Module      : Network.AWS.Route53.Types.HealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheck where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains information about one health check that is
-- associated with the current AWS account.
--
-- /See:/ 'newHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | A complex type that contains information about the CloudWatch alarm that
    -- Amazon Route 53 is monitoring for this health check.
    cloudWatchAlarmConfiguration :: Core.Maybe CloudWatchAlarmConfiguration,
    -- | If the health check was created by another service, the service that
    -- created the health check. When a health check is created by another
    -- service, you can\'t edit or delete it using Amazon Route 53.
    linkedService :: Core.Maybe LinkedService,
    -- | The identifier that Amazon Route 53 assigned to the health check when
    -- you created it. When you add or update a resource record set, you use
    -- this value to specify which health check to use. The value can be up to
    -- 64 characters long.
    id :: Core.Text,
    -- | A unique string that you specified when you created the health check.
    callerReference :: Core.Text,
    -- | A complex type that contains detailed information about one health
    -- check.
    healthCheckConfig :: HealthCheckConfig,
    -- | The version of the health check. You can optionally pass this value in a
    -- call to @UpdateHealthCheck@ to prevent overwriting another change to the
    -- health check.
    healthCheckVersion :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchAlarmConfiguration', 'healthCheck_cloudWatchAlarmConfiguration' - A complex type that contains information about the CloudWatch alarm that
-- Amazon Route 53 is monitoring for this health check.
--
-- 'linkedService', 'healthCheck_linkedService' - If the health check was created by another service, the service that
-- created the health check. When a health check is created by another
-- service, you can\'t edit or delete it using Amazon Route 53.
--
-- 'id', 'healthCheck_id' - The identifier that Amazon Route 53 assigned to the health check when
-- you created it. When you add or update a resource record set, you use
-- this value to specify which health check to use. The value can be up to
-- 64 characters long.
--
-- 'callerReference', 'healthCheck_callerReference' - A unique string that you specified when you created the health check.
--
-- 'healthCheckConfig', 'healthCheck_healthCheckConfig' - A complex type that contains detailed information about one health
-- check.
--
-- 'healthCheckVersion', 'healthCheck_healthCheckVersion' - The version of the health check. You can optionally pass this value in a
-- call to @UpdateHealthCheck@ to prevent overwriting another change to the
-- health check.
newHealthCheck ::
  -- | 'id'
  Core.Text ->
  -- | 'callerReference'
  Core.Text ->
  -- | 'healthCheckConfig'
  HealthCheckConfig ->
  -- | 'healthCheckVersion'
  Core.Natural ->
  HealthCheck
newHealthCheck
  pId_
  pCallerReference_
  pHealthCheckConfig_
  pHealthCheckVersion_ =
    HealthCheck'
      { cloudWatchAlarmConfiguration =
          Core.Nothing,
        linkedService = Core.Nothing,
        id = pId_,
        callerReference = pCallerReference_,
        healthCheckConfig = pHealthCheckConfig_,
        healthCheckVersion = pHealthCheckVersion_
      }

-- | A complex type that contains information about the CloudWatch alarm that
-- Amazon Route 53 is monitoring for this health check.
healthCheck_cloudWatchAlarmConfiguration :: Lens.Lens' HealthCheck (Core.Maybe CloudWatchAlarmConfiguration)
healthCheck_cloudWatchAlarmConfiguration = Lens.lens (\HealthCheck' {cloudWatchAlarmConfiguration} -> cloudWatchAlarmConfiguration) (\s@HealthCheck' {} a -> s {cloudWatchAlarmConfiguration = a} :: HealthCheck)

-- | If the health check was created by another service, the service that
-- created the health check. When a health check is created by another
-- service, you can\'t edit or delete it using Amazon Route 53.
healthCheck_linkedService :: Lens.Lens' HealthCheck (Core.Maybe LinkedService)
healthCheck_linkedService = Lens.lens (\HealthCheck' {linkedService} -> linkedService) (\s@HealthCheck' {} a -> s {linkedService = a} :: HealthCheck)

-- | The identifier that Amazon Route 53 assigned to the health check when
-- you created it. When you add or update a resource record set, you use
-- this value to specify which health check to use. The value can be up to
-- 64 characters long.
healthCheck_id :: Lens.Lens' HealthCheck Core.Text
healthCheck_id = Lens.lens (\HealthCheck' {id} -> id) (\s@HealthCheck' {} a -> s {id = a} :: HealthCheck)

-- | A unique string that you specified when you created the health check.
healthCheck_callerReference :: Lens.Lens' HealthCheck Core.Text
healthCheck_callerReference = Lens.lens (\HealthCheck' {callerReference} -> callerReference) (\s@HealthCheck' {} a -> s {callerReference = a} :: HealthCheck)

-- | A complex type that contains detailed information about one health
-- check.
healthCheck_healthCheckConfig :: Lens.Lens' HealthCheck HealthCheckConfig
healthCheck_healthCheckConfig = Lens.lens (\HealthCheck' {healthCheckConfig} -> healthCheckConfig) (\s@HealthCheck' {} a -> s {healthCheckConfig = a} :: HealthCheck)

-- | The version of the health check. You can optionally pass this value in a
-- call to @UpdateHealthCheck@ to prevent overwriting another change to the
-- health check.
healthCheck_healthCheckVersion :: Lens.Lens' HealthCheck Core.Natural
healthCheck_healthCheckVersion = Lens.lens (\HealthCheck' {healthCheckVersion} -> healthCheckVersion) (\s@HealthCheck' {} a -> s {healthCheckVersion = a} :: HealthCheck)

instance Core.FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      Core.<$> (x Core..@? "CloudWatchAlarmConfiguration")
      Core.<*> (x Core..@? "LinkedService")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "HealthCheckConfig")
      Core.<*> (x Core..@ "HealthCheckVersion")

instance Core.Hashable HealthCheck

instance Core.NFData HealthCheck
