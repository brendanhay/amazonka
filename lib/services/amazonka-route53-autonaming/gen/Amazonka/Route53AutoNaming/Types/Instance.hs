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
-- Module      : Amazonka.Route53AutoNaming.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about an instance that Cloud
-- Map creates when you submit a @RegisterInstance@ request.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | A string map that contains the following information for the service
    -- that you specify in @ServiceId@:
    --
    -- -   The attributes that apply to the records that are defined in the
    --     service.
    --
    -- -   For each attribute, the applicable value.
    --
    -- Do not include sensitive information in the attributes if the namespace
    -- is discoverable by public DNS queries.
    --
    -- Supported attribute keys include the following:
    --
    -- [AWS_ALIAS_DNS_NAME]
    --     If you want Cloud Map to create a Route 53 alias record that routes
    --     traffic to an Elastic Load Balancing load balancer, specify the DNS
    --     name that\'s associated with the load balancer. For information
    --     about how to get the DNS name, see
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-DNSName AliasTarget->DNSName>
    --     in the /Route 53 API Reference/.
    --
    --     Note the following:
    --
    --     -   The configuration for the service that\'s specified by
    --         @ServiceId@ must include settings for an @A@ record, an @AAAA@
    --         record, or both.
    --
    --     -   In the service that\'s specified by @ServiceId@, the value of
    --         @RoutingPolicy@ must be @WEIGHTED@.
    --
    --     -   If the service that\'s specified by @ServiceId@ includes
    --         @HealthCheckConfig@ settings, Cloud Map creates the health
    --         check, but it won\'t associate the health check with the alias
    --         record.
    --
    --     -   Auto naming currently doesn\'t support creating alias records
    --         that route traffic to Amazon Web Services resources other than
    --         ELB load balancers.
    --
    --     -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
    --         values for any of the @AWS_INSTANCE@ attributes.
    --
    -- [AWS_EC2_INSTANCE_ID]
    --     /HTTP namespaces only./ The Amazon EC2 instance ID for the instance.
    --     The @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
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
    --     If the service configuration includes a @CNAME@ record, the domain
    --     name that you want Route 53 to return in response to DNS queries
    --     (for example, @example.com@).
    --
    --     This value is required if the service specified by @ServiceId@
    --     includes settings for an @CNAME@ record.
    --
    -- [AWS_INSTANCE_IPV4]
    --     If the service configuration includes an @A@ record, the IPv4
    --     address that you want Route 53 to return in response to DNS queries
    --     (for example, @192.0.2.44@).
    --
    --     This value is required if the service specified by @ServiceId@
    --     includes settings for an @A@ record. If the service includes
    --     settings for an @SRV@ record, you must specify a value for
    --     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
    --
    -- [AWS_INSTANCE_IPV6]
    --     If the service configuration includes an @AAAA@ record, the IPv6
    --     address that you want Route 53 to return in response to DNS queries
    --     (for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
    --
    --     This value is required if the service specified by @ServiceId@
    --     includes settings for an @AAAA@ record. If the service includes
    --     settings for an @SRV@ record, you must specify a value for
    --     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
    --
    -- [AWS_INSTANCE_PORT]
    --     If the service includes an @SRV@ record, the value that you want
    --     Route 53 to return for the port.
    --
    --     If the service includes @HealthCheckConfig@, the port on the
    --     endpoint that you want Route 53 to send requests to.
    --
    --     This value is required if you specified settings for an @SRV@ record
    --     or a Route 53 health check when you created the service.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique string that identifies the request and that allows failed
    -- @RegisterInstance@ requests to be retried without the risk of executing
    -- the operation twice. You must use a unique @CreatorRequestId@ string
    -- every time you submit a @RegisterInstance@ request if you\'re
    -- registering additional instances for the same namespace and service.
    -- @CreatorRequestId@ can be any unique string (for example, a date\/time
    -- stamp).
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | An identifier that you want to associate with the instance. Note the
    -- following:
    --
    -- -   If the service that\'s specified by @ServiceId@ includes settings
    --     for an @SRV@ record, the value of @InstanceId@ is automatically
    --     included as part of the value for the @SRV@ record. For more
    --     information, see
    --     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
    --
    -- -   You can use this value to update an existing instance.
    --
    -- -   To register a new instance, you must specify a value that\'s unique
    --     among instances that you register by using the same service.
    --
    -- -   If you specify an existing @InstanceId@ and @ServiceId@, Cloud Map
    --     updates the existing DNS records. If there\'s also an existing
    --     health check, Cloud Map deletes the old health check and creates a
    --     new one.
    --
    --     The health check isn\'t deleted immediately, so it will still appear
    --     for a while if you submit a @ListHealthChecks@ request, for example.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'instance_attributes' - A string map that contains the following information for the service
-- that you specify in @ServiceId@:
--
-- -   The attributes that apply to the records that are defined in the
--     service.
--
-- -   For each attribute, the applicable value.
--
-- Do not include sensitive information in the attributes if the namespace
-- is discoverable by public DNS queries.
--
-- Supported attribute keys include the following:
--
-- [AWS_ALIAS_DNS_NAME]
--     If you want Cloud Map to create a Route 53 alias record that routes
--     traffic to an Elastic Load Balancing load balancer, specify the DNS
--     name that\'s associated with the load balancer. For information
--     about how to get the DNS name, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-DNSName AliasTarget->DNSName>
--     in the /Route 53 API Reference/.
--
--     Note the following:
--
--     -   The configuration for the service that\'s specified by
--         @ServiceId@ must include settings for an @A@ record, an @AAAA@
--         record, or both.
--
--     -   In the service that\'s specified by @ServiceId@, the value of
--         @RoutingPolicy@ must be @WEIGHTED@.
--
--     -   If the service that\'s specified by @ServiceId@ includes
--         @HealthCheckConfig@ settings, Cloud Map creates the health
--         check, but it won\'t associate the health check with the alias
--         record.
--
--     -   Auto naming currently doesn\'t support creating alias records
--         that route traffic to Amazon Web Services resources other than
--         ELB load balancers.
--
--     -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
--         values for any of the @AWS_INSTANCE@ attributes.
--
-- [AWS_EC2_INSTANCE_ID]
--     /HTTP namespaces only./ The Amazon EC2 instance ID for the instance.
--     The @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
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
--     If the service configuration includes a @CNAME@ record, the domain
--     name that you want Route 53 to return in response to DNS queries
--     (for example, @example.com@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @CNAME@ record.
--
-- [AWS_INSTANCE_IPV4]
--     If the service configuration includes an @A@ record, the IPv4
--     address that you want Route 53 to return in response to DNS queries
--     (for example, @192.0.2.44@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @A@ record. If the service includes
--     settings for an @SRV@ record, you must specify a value for
--     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
--
-- [AWS_INSTANCE_IPV6]
--     If the service configuration includes an @AAAA@ record, the IPv6
--     address that you want Route 53 to return in response to DNS queries
--     (for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @AAAA@ record. If the service includes
--     settings for an @SRV@ record, you must specify a value for
--     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
--
-- [AWS_INSTANCE_PORT]
--     If the service includes an @SRV@ record, the value that you want
--     Route 53 to return for the port.
--
--     If the service includes @HealthCheckConfig@, the port on the
--     endpoint that you want Route 53 to send requests to.
--
--     This value is required if you specified settings for an @SRV@ record
--     or a Route 53 health check when you created the service.
--
-- 'creatorRequestId', 'instance_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @RegisterInstance@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CreatorRequestId@ string
-- every time you submit a @RegisterInstance@ request if you\'re
-- registering additional instances for the same namespace and service.
-- @CreatorRequestId@ can be any unique string (for example, a date\/time
-- stamp).
--
-- 'id', 'instance_id' - An identifier that you want to associate with the instance. Note the
-- following:
--
-- -   If the service that\'s specified by @ServiceId@ includes settings
--     for an @SRV@ record, the value of @InstanceId@ is automatically
--     included as part of the value for the @SRV@ record. For more
--     information, see
--     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
--
-- -   You can use this value to update an existing instance.
--
-- -   To register a new instance, you must specify a value that\'s unique
--     among instances that you register by using the same service.
--
-- -   If you specify an existing @InstanceId@ and @ServiceId@, Cloud Map
--     updates the existing DNS records. If there\'s also an existing
--     health check, Cloud Map deletes the old health check and creates a
--     new one.
--
--     The health check isn\'t deleted immediately, so it will still appear
--     for a while if you submit a @ListHealthChecks@ request, for example.
newInstance ::
  -- | 'id'
  Prelude.Text ->
  Instance
newInstance pId_ =
  Instance'
    { attributes = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      id = pId_
    }

-- | A string map that contains the following information for the service
-- that you specify in @ServiceId@:
--
-- -   The attributes that apply to the records that are defined in the
--     service.
--
-- -   For each attribute, the applicable value.
--
-- Do not include sensitive information in the attributes if the namespace
-- is discoverable by public DNS queries.
--
-- Supported attribute keys include the following:
--
-- [AWS_ALIAS_DNS_NAME]
--     If you want Cloud Map to create a Route 53 alias record that routes
--     traffic to an Elastic Load Balancing load balancer, specify the DNS
--     name that\'s associated with the load balancer. For information
--     about how to get the DNS name, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-DNSName AliasTarget->DNSName>
--     in the /Route 53 API Reference/.
--
--     Note the following:
--
--     -   The configuration for the service that\'s specified by
--         @ServiceId@ must include settings for an @A@ record, an @AAAA@
--         record, or both.
--
--     -   In the service that\'s specified by @ServiceId@, the value of
--         @RoutingPolicy@ must be @WEIGHTED@.
--
--     -   If the service that\'s specified by @ServiceId@ includes
--         @HealthCheckConfig@ settings, Cloud Map creates the health
--         check, but it won\'t associate the health check with the alias
--         record.
--
--     -   Auto naming currently doesn\'t support creating alias records
--         that route traffic to Amazon Web Services resources other than
--         ELB load balancers.
--
--     -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
--         values for any of the @AWS_INSTANCE@ attributes.
--
-- [AWS_EC2_INSTANCE_ID]
--     /HTTP namespaces only./ The Amazon EC2 instance ID for the instance.
--     The @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4
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
--     If the service configuration includes a @CNAME@ record, the domain
--     name that you want Route 53 to return in response to DNS queries
--     (for example, @example.com@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @CNAME@ record.
--
-- [AWS_INSTANCE_IPV4]
--     If the service configuration includes an @A@ record, the IPv4
--     address that you want Route 53 to return in response to DNS queries
--     (for example, @192.0.2.44@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @A@ record. If the service includes
--     settings for an @SRV@ record, you must specify a value for
--     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
--
-- [AWS_INSTANCE_IPV6]
--     If the service configuration includes an @AAAA@ record, the IPv6
--     address that you want Route 53 to return in response to DNS queries
--     (for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@).
--
--     This value is required if the service specified by @ServiceId@
--     includes settings for an @AAAA@ record. If the service includes
--     settings for an @SRV@ record, you must specify a value for
--     @AWS_INSTANCE_IPV4@, @AWS_INSTANCE_IPV6@, or both.
--
-- [AWS_INSTANCE_PORT]
--     If the service includes an @SRV@ record, the value that you want
--     Route 53 to return for the port.
--
--     If the service includes @HealthCheckConfig@, the port on the
--     endpoint that you want Route 53 to send requests to.
--
--     This value is required if you specified settings for an @SRV@ record
--     or a Route 53 health check when you created the service.
instance_attributes :: Lens.Lens' Instance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instance_attributes = Lens.lens (\Instance' {attributes} -> attributes) (\s@Instance' {} a -> s {attributes = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | A unique string that identifies the request and that allows failed
-- @RegisterInstance@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CreatorRequestId@ string
-- every time you submit a @RegisterInstance@ request if you\'re
-- registering additional instances for the same namespace and service.
-- @CreatorRequestId@ can be any unique string (for example, a date\/time
-- stamp).
instance_creatorRequestId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_creatorRequestId = Lens.lens (\Instance' {creatorRequestId} -> creatorRequestId) (\s@Instance' {} a -> s {creatorRequestId = a} :: Instance)

-- | An identifier that you want to associate with the instance. Note the
-- following:
--
-- -   If the service that\'s specified by @ServiceId@ includes settings
--     for an @SRV@ record, the value of @InstanceId@ is automatically
--     included as part of the value for the @SRV@ record. For more
--     information, see
--     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
--
-- -   You can use this value to update an existing instance.
--
-- -   To register a new instance, you must specify a value that\'s unique
--     among instances that you register by using the same service.
--
-- -   If you specify an existing @InstanceId@ and @ServiceId@, Cloud Map
--     updates the existing DNS records. If there\'s also an existing
--     health check, Cloud Map deletes the old health check and creates a
--     new one.
--
--     The health check isn\'t deleted immediately, so it will still appear
--     for a while if you submit a @ListHealthChecks@ request, for example.
instance_id :: Lens.Lens' Instance Prelude.Text
instance_id = Lens.lens (\Instance' {id} -> id) (\s@Instance' {} a -> s {id = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` id

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf creatorRequestId `Prelude.seq`
        Prelude.rnf id
