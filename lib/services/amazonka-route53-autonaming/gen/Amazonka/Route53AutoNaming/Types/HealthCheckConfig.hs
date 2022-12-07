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
-- Module      : Amazonka.Route53AutoNaming.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.HealthCheckConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.HealthCheckType

-- | /Public DNS and HTTP namespaces only./ A complex type that contains
-- settings for an optional health check. If you specify settings for a
-- health check, Cloud Map associates the health check with the records
-- that you specify in @DnsConfig@.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- Health checks are basic Route 53 health checks that monitor an Amazon
-- Web Services endpoint. For information about pricing for health checks,
-- see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
--
-- Note the following about configuring health checks.
--
-- [A and AAAA records]
--     If @DnsConfig@ includes configurations for both @A@ and @AAAA@
--     records, Cloud Map creates a health check that uses the IPv4 address
--     to check the health of the resource. If the endpoint tthat\'s
--     specified by the IPv4 address is unhealthy, Route 53 considers both
--     the @A@ and @AAAA@ records to be unhealthy.
--
-- [CNAME records]
--     You can\'t specify settings for @HealthCheckConfig@ when the
--     @DNSConfig@ includes @CNAME@ for the value of @Type@. If you do, the
--     @CreateService@ request will fail with an @InvalidInput@ error.
--
-- [Request interval]
--     A Route 53 health checker in each health-checking Amazon Web
--     Services Region sends a health check request to an endpoint every 30
--     seconds. On average, your endpoint receives a health check request
--     about every two seconds. However, health checkers don\'t coordinate
--     with one another. Therefore, you might sometimes see several
--     requests in one second that\'s followed by a few seconds with no
--     health checks at all.
--
-- [Health checking regions]
--     Health checkers perform checks from all Route 53 health-checking
--     Regions. For a list of the current Regions, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions>.
--
-- [Alias records]
--     When you register an instance, if you include the
--     @AWS_ALIAS_DNS_NAME@ attribute, Cloud Map creates a Route 53 alias
--     record. Note the following:
--
--     -   Route 53 automatically sets @EvaluateTargetHealth@ to true for
--         alias records. When @EvaluateTargetHealth@ is true, the alias
--         record inherits the health of the referenced Amazon Web Services
--         resource. such as an ELB load balancer. For more information,
--         see
--         <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth EvaluateTargetHealth>.
--
--     -   If you include @HealthCheckConfig@ and then use the service to
--         register an instance that creates an alias record, Route 53
--         doesn\'t create the health check.
--
-- [Charges for health checks]
--     Health checks are basic Route 53 health checks that monitor an
--     Amazon Web Services endpoint. For information about pricing for
--     health checks, see
--     <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
--
-- /See:/ 'newHealthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { -- | The number of consecutive health checks that an endpoint must pass or
    -- fail for Route 53 to change the current status of the endpoint from
    -- unhealthy to healthy or the other way around. For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
    -- in the /Route 53 Developer Guide/.
    failureThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The path that you want Route 53 to request when performing health
    -- checks. The path can be any value that your endpoint returns an HTTP
    -- status code of a 2xx or 3xx format for when the endpoint is healthy. An
    -- example file is @\/docs\/route53-health-check.html@. Route 53
    -- automatically adds the DNS name for the service. If you don\'t specify a
    -- value for @ResourcePath@, the default value is @\/@.
    --
    -- If you specify @TCP@ for @Type@, you must /not/ specify a value for
    -- @ResourcePath@.
    resourcePath :: Prelude.Maybe Prelude.Text,
    -- | The type of health check that you want to create, which indicates how
    -- Route 53 determines whether an endpoint is healthy.
    --
    -- You can\'t change the value of @Type@ after you create a health check.
    --
    -- You can create the following types of health checks:
    --
    -- -   __HTTP__: Route 53 tries to establish a TCP connection. If
    --     successful, Route 53 submits an HTTP request and waits for an HTTP
    --     status code of 200 or greater and less than 400.
    --
    -- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
    --     successful, Route 53 submits an HTTPS request and waits for an HTTP
    --     status code of 200 or greater and less than 400.
    --
    --     If you specify HTTPS for the value of @Type@, the endpoint must
    --     support TLS v1.0 or later.
    --
    -- -   __TCP__: Route 53 tries to establish a TCP connection.
    --
    --     If you specify @TCP@ for @Type@, don\'t specify a value for
    --     @ResourcePath@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
    -- in the /Route 53 Developer Guide/.
    type' :: HealthCheckType
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
-- 'failureThreshold', 'healthCheckConfig_failureThreshold' - The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or the other way around. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Route 53 Developer Guide/.
--
-- 'resourcePath', 'healthCheckConfig_resourcePath' - The path that you want Route 53 to request when performing health
-- checks. The path can be any value that your endpoint returns an HTTP
-- status code of a 2xx or 3xx format for when the endpoint is healthy. An
-- example file is @\/docs\/route53-health-check.html@. Route 53
-- automatically adds the DNS name for the service. If you don\'t specify a
-- value for @ResourcePath@, the default value is @\/@.
--
-- If you specify @TCP@ for @Type@, you must /not/ specify a value for
-- @ResourcePath@.
--
-- 'type'', 'healthCheckConfig_type' - The type of health check that you want to create, which indicates how
-- Route 53 determines whether an endpoint is healthy.
--
-- You can\'t change the value of @Type@ after you create a health check.
--
-- You can create the following types of health checks:
--
-- -   __HTTP__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
-- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTPS request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
--     If you specify HTTPS for the value of @Type@, the endpoint must
--     support TLS v1.0 or later.
--
-- -   __TCP__: Route 53 tries to establish a TCP connection.
--
--     If you specify @TCP@ for @Type@, don\'t specify a value for
--     @ResourcePath@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Route 53 Developer Guide/.
newHealthCheckConfig ::
  -- | 'type''
  HealthCheckType ->
  HealthCheckConfig
newHealthCheckConfig pType_ =
  HealthCheckConfig'
    { failureThreshold =
        Prelude.Nothing,
      resourcePath = Prelude.Nothing,
      type' = pType_
    }

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or the other way around. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Route 53 Developer Guide/.
healthCheckConfig_failureThreshold :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_failureThreshold = Lens.lens (\HealthCheckConfig' {failureThreshold} -> failureThreshold) (\s@HealthCheckConfig' {} a -> s {failureThreshold = a} :: HealthCheckConfig)

-- | The path that you want Route 53 to request when performing health
-- checks. The path can be any value that your endpoint returns an HTTP
-- status code of a 2xx or 3xx format for when the endpoint is healthy. An
-- example file is @\/docs\/route53-health-check.html@. Route 53
-- automatically adds the DNS name for the service. If you don\'t specify a
-- value for @ResourcePath@, the default value is @\/@.
--
-- If you specify @TCP@ for @Type@, you must /not/ specify a value for
-- @ResourcePath@.
healthCheckConfig_resourcePath :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_resourcePath = Lens.lens (\HealthCheckConfig' {resourcePath} -> resourcePath) (\s@HealthCheckConfig' {} a -> s {resourcePath = a} :: HealthCheckConfig)

-- | The type of health check that you want to create, which indicates how
-- Route 53 determines whether an endpoint is healthy.
--
-- You can\'t change the value of @Type@ after you create a health check.
--
-- You can create the following types of health checks:
--
-- -   __HTTP__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
-- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTPS request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
--     If you specify HTTPS for the value of @Type@, the endpoint must
--     support TLS v1.0 or later.
--
-- -   __TCP__: Route 53 tries to establish a TCP connection.
--
--     If you specify @TCP@ for @Type@, don\'t specify a value for
--     @ResourcePath@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Route 53 Developer Guide/.
healthCheckConfig_type :: Lens.Lens' HealthCheckConfig HealthCheckType
healthCheckConfig_type = Lens.lens (\HealthCheckConfig' {type'} -> type') (\s@HealthCheckConfig' {} a -> s {type' = a} :: HealthCheckConfig)

instance Data.FromJSON HealthCheckConfig where
  parseJSON =
    Data.withObject
      "HealthCheckConfig"
      ( \x ->
          HealthCheckConfig'
            Prelude.<$> (x Data..:? "FailureThreshold")
            Prelude.<*> (x Data..:? "ResourcePath")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable HealthCheckConfig where
  hashWithSalt _salt HealthCheckConfig' {..} =
    _salt `Prelude.hashWithSalt` failureThreshold
      `Prelude.hashWithSalt` resourcePath
      `Prelude.hashWithSalt` type'

instance Prelude.NFData HealthCheckConfig where
  rnf HealthCheckConfig' {..} =
    Prelude.rnf failureThreshold
      `Prelude.seq` Prelude.rnf resourcePath
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON HealthCheckConfig where
  toJSON HealthCheckConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FailureThreshold" Data..=)
              Prelude.<$> failureThreshold,
            ("ResourcePath" Data..=) Prelude.<$> resourcePath,
            Prelude.Just ("Type" Data..= type')
          ]
      )
