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
-- Module      : Amazonka.VPCLattice.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.HealthCheckConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.HealthCheckProtocolVersion
import Amazonka.VPCLattice.Types.Matcher
import Amazonka.VPCLattice.Types.TargetGroupProtocol

-- | The health check configuration of a target group. Health check
-- configurations aren\'t used for @LAMBDA@ and @ALB@ target groups.
--
-- /See:/ 'newHealthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { -- | Indicates whether health checking is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target. The range is 5–300 seconds. The default is 30
    -- seconds.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time, in seconds, to wait before reporting a target as
    -- unhealthy. The range is 1–120 seconds. The default is 5 seconds.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive successful health checks required before
    -- considering an unhealthy target healthy. The range is 2–10. The default
    -- is 5.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The codes to use when checking for a successful response from a target.
    -- These are called /Success codes/ in the console.
    matcher :: Prelude.Maybe Matcher,
    -- | The destination for health checks on the targets. If the protocol
    -- version is @HTTP\/1.1@ or @HTTP\/2@, specify a valid URI (for example,
    -- @\/path?query@). The default path is @\/@. Health checks are not
    -- supported if the protocol version is @gRPC@, however, you can choose
    -- @HTTP\/1.1@ or @HTTP\/2@ and specify a valid URI.
    path :: Prelude.Maybe Prelude.Text,
    -- | The port used when performing health checks on targets. The default
    -- setting is the port that a target receives traffic on.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The protocol used when performing health checks on targets. The possible
    -- protocols are @HTTP@ and @HTTPS@. The default is @HTTP@.
    protocol :: Prelude.Maybe TargetGroupProtocol,
    -- | The protocol version used when performing health checks on targets. The
    -- possible protocol versions are @HTTP1@ and @HTTP2@.
    protocolVersion :: Prelude.Maybe HealthCheckProtocolVersion,
    -- | The number of consecutive failed health checks required before
    -- considering a target unhealthy. The range is 2–10. The default is 2.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'healthCheckConfig_enabled' - Indicates whether health checking is enabled.
--
-- 'healthCheckIntervalSeconds', 'healthCheckConfig_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target. The range is 5–300 seconds. The default is 30
-- seconds.
--
-- 'healthCheckTimeoutSeconds', 'healthCheckConfig_healthCheckTimeoutSeconds' - The amount of time, in seconds, to wait before reporting a target as
-- unhealthy. The range is 1–120 seconds. The default is 5 seconds.
--
-- 'healthyThresholdCount', 'healthCheckConfig_healthyThresholdCount' - The number of consecutive successful health checks required before
-- considering an unhealthy target healthy. The range is 2–10. The default
-- is 5.
--
-- 'matcher', 'healthCheckConfig_matcher' - The codes to use when checking for a successful response from a target.
-- These are called /Success codes/ in the console.
--
-- 'path', 'healthCheckConfig_path' - The destination for health checks on the targets. If the protocol
-- version is @HTTP\/1.1@ or @HTTP\/2@, specify a valid URI (for example,
-- @\/path?query@). The default path is @\/@. Health checks are not
-- supported if the protocol version is @gRPC@, however, you can choose
-- @HTTP\/1.1@ or @HTTP\/2@ and specify a valid URI.
--
-- 'port', 'healthCheckConfig_port' - The port used when performing health checks on targets. The default
-- setting is the port that a target receives traffic on.
--
-- 'protocol', 'healthCheckConfig_protocol' - The protocol used when performing health checks on targets. The possible
-- protocols are @HTTP@ and @HTTPS@. The default is @HTTP@.
--
-- 'protocolVersion', 'healthCheckConfig_protocolVersion' - The protocol version used when performing health checks on targets. The
-- possible protocol versions are @HTTP1@ and @HTTP2@.
--
-- 'unhealthyThresholdCount', 'healthCheckConfig_unhealthyThresholdCount' - The number of consecutive failed health checks required before
-- considering a target unhealthy. The range is 2–10. The default is 2.
newHealthCheckConfig ::
  HealthCheckConfig
newHealthCheckConfig =
  HealthCheckConfig'
    { enabled = Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      matcher = Prelude.Nothing,
      path = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing
    }

-- | Indicates whether health checking is enabled.
healthCheckConfig_enabled :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Bool)
healthCheckConfig_enabled = Lens.lens (\HealthCheckConfig' {enabled} -> enabled) (\s@HealthCheckConfig' {} a -> s {enabled = a} :: HealthCheckConfig)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target. The range is 5–300 seconds. The default is 30
-- seconds.
healthCheckConfig_healthCheckIntervalSeconds :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_healthCheckIntervalSeconds = Lens.lens (\HealthCheckConfig' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@HealthCheckConfig' {} a -> s {healthCheckIntervalSeconds = a} :: HealthCheckConfig)

-- | The amount of time, in seconds, to wait before reporting a target as
-- unhealthy. The range is 1–120 seconds. The default is 5 seconds.
healthCheckConfig_healthCheckTimeoutSeconds :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_healthCheckTimeoutSeconds = Lens.lens (\HealthCheckConfig' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@HealthCheckConfig' {} a -> s {healthCheckTimeoutSeconds = a} :: HealthCheckConfig)

-- | The number of consecutive successful health checks required before
-- considering an unhealthy target healthy. The range is 2–10. The default
-- is 5.
healthCheckConfig_healthyThresholdCount :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_healthyThresholdCount = Lens.lens (\HealthCheckConfig' {healthyThresholdCount} -> healthyThresholdCount) (\s@HealthCheckConfig' {} a -> s {healthyThresholdCount = a} :: HealthCheckConfig)

-- | The codes to use when checking for a successful response from a target.
-- These are called /Success codes/ in the console.
healthCheckConfig_matcher :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Matcher)
healthCheckConfig_matcher = Lens.lens (\HealthCheckConfig' {matcher} -> matcher) (\s@HealthCheckConfig' {} a -> s {matcher = a} :: HealthCheckConfig)

-- | The destination for health checks on the targets. If the protocol
-- version is @HTTP\/1.1@ or @HTTP\/2@, specify a valid URI (for example,
-- @\/path?query@). The default path is @\/@. Health checks are not
-- supported if the protocol version is @gRPC@, however, you can choose
-- @HTTP\/1.1@ or @HTTP\/2@ and specify a valid URI.
healthCheckConfig_path :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_path = Lens.lens (\HealthCheckConfig' {path} -> path) (\s@HealthCheckConfig' {} a -> s {path = a} :: HealthCheckConfig)

-- | The port used when performing health checks on targets. The default
-- setting is the port that a target receives traffic on.
healthCheckConfig_port :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_port = Lens.lens (\HealthCheckConfig' {port} -> port) (\s@HealthCheckConfig' {} a -> s {port = a} :: HealthCheckConfig)

-- | The protocol used when performing health checks on targets. The possible
-- protocols are @HTTP@ and @HTTPS@. The default is @HTTP@.
healthCheckConfig_protocol :: Lens.Lens' HealthCheckConfig (Prelude.Maybe TargetGroupProtocol)
healthCheckConfig_protocol = Lens.lens (\HealthCheckConfig' {protocol} -> protocol) (\s@HealthCheckConfig' {} a -> s {protocol = a} :: HealthCheckConfig)

-- | The protocol version used when performing health checks on targets. The
-- possible protocol versions are @HTTP1@ and @HTTP2@.
healthCheckConfig_protocolVersion :: Lens.Lens' HealthCheckConfig (Prelude.Maybe HealthCheckProtocolVersion)
healthCheckConfig_protocolVersion = Lens.lens (\HealthCheckConfig' {protocolVersion} -> protocolVersion) (\s@HealthCheckConfig' {} a -> s {protocolVersion = a} :: HealthCheckConfig)

-- | The number of consecutive failed health checks required before
-- considering a target unhealthy. The range is 2–10. The default is 2.
healthCheckConfig_unhealthyThresholdCount :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_unhealthyThresholdCount = Lens.lens (\HealthCheckConfig' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@HealthCheckConfig' {} a -> s {unhealthyThresholdCount = a} :: HealthCheckConfig)

instance Data.FromJSON HealthCheckConfig where
  parseJSON =
    Data.withObject
      "HealthCheckConfig"
      ( \x ->
          HealthCheckConfig'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "healthCheckIntervalSeconds")
            Prelude.<*> (x Data..:? "healthCheckTimeoutSeconds")
            Prelude.<*> (x Data..:? "healthyThresholdCount")
            Prelude.<*> (x Data..:? "matcher")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "protocolVersion")
            Prelude.<*> (x Data..:? "unhealthyThresholdCount")
      )

instance Prelude.Hashable HealthCheckConfig where
  hashWithSalt _salt HealthCheckConfig' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckTimeoutSeconds
      `Prelude.hashWithSalt` healthyThresholdCount
      `Prelude.hashWithSalt` matcher
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` protocolVersion
      `Prelude.hashWithSalt` unhealthyThresholdCount

instance Prelude.NFData HealthCheckConfig where
  rnf HealthCheckConfig' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthCheckTimeoutSeconds
      `Prelude.seq` Prelude.rnf healthyThresholdCount
      `Prelude.seq` Prelude.rnf matcher
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf protocolVersion
      `Prelude.seq` Prelude.rnf unhealthyThresholdCount

instance Data.ToJSON HealthCheckConfig where
  toJSON HealthCheckConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("healthCheckIntervalSeconds" Data..=)
              Prelude.<$> healthCheckIntervalSeconds,
            ("healthCheckTimeoutSeconds" Data..=)
              Prelude.<$> healthCheckTimeoutSeconds,
            ("healthyThresholdCount" Data..=)
              Prelude.<$> healthyThresholdCount,
            ("matcher" Data..=) Prelude.<$> matcher,
            ("path" Data..=) Prelude.<$> path,
            ("port" Data..=) Prelude.<$> port,
            ("protocol" Data..=) Prelude.<$> protocol,
            ("protocolVersion" Data..=)
              Prelude.<$> protocolVersion,
            ("unhealthyThresholdCount" Data..=)
              Prelude.<$> unhealthyThresholdCount
          ]
      )
