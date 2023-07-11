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
-- Module      : Amazonka.Route53AutoNaming.Types.InstanceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.InstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about the instances that you
-- registered by using a specified service.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | A string map that contains the following information:
    --
    -- -   The attributes that are associated with the instance.
    --
    -- -   For each attribute, the applicable value.
    --
    -- Supported attribute keys include the following:
    --
    -- [AWS_ALIAS_DNS_NAME]
    --     For an alias record that routes traffic to an Elastic Load Balancing
    --     load balancer, the DNS name that\'s associated with the load
    --     balancer.
    --
    -- [AWS_EC2_INSTANCE_ID (HTTP namespaces only)]
    --     The Amazon EC2 instance ID for the instance. When the
    --     @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
    --     @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
    --     address.
    --
    -- [AWS_INIT_HEALTH_STATUS]
    --     If the service configuration includes @HealthCheckCustomConfig@, you
    --     can optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial
    --     status of the custom health check, @HEALTHY@ or @UNHEALTHY@. If you
    --     don\'t specify a value for @AWS_INIT_HEALTH_STATUS@, the initial
    --     status is @HEALTHY@.
    --
    -- [AWS_INSTANCE_CNAME]
    --     For a @CNAME@ record, the domain name that Route 53 returns in
    --     response to DNS queries (for example, @example.com@).
    --
    -- [AWS_INSTANCE_IPV4]
    --     For an @A@ record, the IPv4 address that Route 53 returns in
    --     response to DNS queries (for example, @192.0.2.44@).
    --
    -- [AWS_INSTANCE_IPV6]
    --     For an @AAAA@ record, the IPv6 address that Route 53 returns in
    --     response to DNS queries (for example,
    --     @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
    --
    -- [AWS_INSTANCE_PORT]
    --     For an @SRV@ record, the value that Route 53 returns for the port.
    --     In addition, if the service includes @HealthCheckConfig@, the port
    --     on the endpoint that Route 53 sends requests to.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID for an instance that you created by using a specified service.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'instanceSummary_attributes' - A string map that contains the following information:
--
-- -   The attributes that are associated with the instance.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- [AWS_ALIAS_DNS_NAME]
--     For an alias record that routes traffic to an Elastic Load Balancing
--     load balancer, the DNS name that\'s associated with the load
--     balancer.
--
-- [AWS_EC2_INSTANCE_ID (HTTP namespaces only)]
--     The Amazon EC2 instance ID for the instance. When the
--     @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
--     @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
--     address.
--
-- [AWS_INIT_HEALTH_STATUS]
--     If the service configuration includes @HealthCheckCustomConfig@, you
--     can optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial
--     status of the custom health check, @HEALTHY@ or @UNHEALTHY@. If you
--     don\'t specify a value for @AWS_INIT_HEALTH_STATUS@, the initial
--     status is @HEALTHY@.
--
-- [AWS_INSTANCE_CNAME]
--     For a @CNAME@ record, the domain name that Route 53 returns in
--     response to DNS queries (for example, @example.com@).
--
-- [AWS_INSTANCE_IPV4]
--     For an @A@ record, the IPv4 address that Route 53 returns in
--     response to DNS queries (for example, @192.0.2.44@).
--
-- [AWS_INSTANCE_IPV6]
--     For an @AAAA@ record, the IPv6 address that Route 53 returns in
--     response to DNS queries (for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
--
-- [AWS_INSTANCE_PORT]
--     For an @SRV@ record, the value that Route 53 returns for the port.
--     In addition, if the service includes @HealthCheckConfig@, the port
--     on the endpoint that Route 53 sends requests to.
--
-- 'id', 'instanceSummary_id' - The ID for an instance that you created by using a specified service.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { attributes = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A string map that contains the following information:
--
-- -   The attributes that are associated with the instance.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- [AWS_ALIAS_DNS_NAME]
--     For an alias record that routes traffic to an Elastic Load Balancing
--     load balancer, the DNS name that\'s associated with the load
--     balancer.
--
-- [AWS_EC2_INSTANCE_ID (HTTP namespaces only)]
--     The Amazon EC2 instance ID for the instance. When the
--     @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
--     @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
--     address.
--
-- [AWS_INIT_HEALTH_STATUS]
--     If the service configuration includes @HealthCheckCustomConfig@, you
--     can optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial
--     status of the custom health check, @HEALTHY@ or @UNHEALTHY@. If you
--     don\'t specify a value for @AWS_INIT_HEALTH_STATUS@, the initial
--     status is @HEALTHY@.
--
-- [AWS_INSTANCE_CNAME]
--     For a @CNAME@ record, the domain name that Route 53 returns in
--     response to DNS queries (for example, @example.com@).
--
-- [AWS_INSTANCE_IPV4]
--     For an @A@ record, the IPv4 address that Route 53 returns in
--     response to DNS queries (for example, @192.0.2.44@).
--
-- [AWS_INSTANCE_IPV6]
--     For an @AAAA@ record, the IPv6 address that Route 53 returns in
--     response to DNS queries (for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
--
-- [AWS_INSTANCE_PORT]
--     For an @SRV@ record, the value that Route 53 returns for the port.
--     In addition, if the service includes @HealthCheckConfig@, the port
--     on the endpoint that Route 53 sends requests to.
instanceSummary_attributes :: Lens.Lens' InstanceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instanceSummary_attributes = Lens.lens (\InstanceSummary' {attributes} -> attributes) (\s@InstanceSummary' {} a -> s {attributes = a} :: InstanceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID for an instance that you created by using a specified service.
instanceSummary_id :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_id = Lens.lens (\InstanceSummary' {id} -> id) (\s@InstanceSummary' {} a -> s {id = a} :: InstanceSummary)

instance Data.FromJSON InstanceSummary where
  parseJSON =
    Data.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable InstanceSummary where
  hashWithSalt _salt InstanceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` id

instance Prelude.NFData InstanceSummary where
  rnf InstanceSummary' {..} =
    Prelude.rnf attributes `Prelude.seq` Prelude.rnf id
