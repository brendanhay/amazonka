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
-- Module      : Amazonka.Route53AutoNaming.Types.DnsRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.DnsRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.RecordType

-- | A complex type that contains information about the Route 53 DNS records
-- that you want Cloud Map to create when you register an instance.
--
-- /See:/ 'newDnsRecord' smart constructor.
data DnsRecord = DnsRecord'
  { -- | The type of the resource, which indicates the type of value that
    -- Route 53 returns in response to DNS queries. You can specify values for
    -- @Type@ in the following combinations:
    --
    -- -   __@A@__
    --
    -- -   __@AAAA@__
    --
    -- -   __@A@__ and __@AAAA@__
    --
    -- -   __@SRV@__
    --
    -- -   __@CNAME@__
    --
    -- If you want Cloud Map to create a Route 53 alias record when you
    -- register an instance, specify @A@ or @AAAA@ for @Type@.
    --
    -- You specify other settings, such as the IP address for @A@ and @AAAA@
    -- records, when you register an instance. For more information, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
    --
    -- The following values are supported:
    --
    -- [A]
    --     Route 53 returns the IP address of the resource in IPv4 format, such
    --     as 192.0.2.44.
    --
    -- [AAAA]
    --     Route 53 returns the IP address of the resource in IPv6 format, such
    --     as 2001:0db8:85a3:0000:0000:abcd:0001:2345.
    --
    -- [CNAME]
    --     Route 53 returns the domain name of the resource, such as
    --     www.example.com. Note the following:
    --
    --     -   You specify the domain name that you want to route traffic to
    --         when you register an instance. For more information, see
    --         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html#cloudmap-RegisterInstance-request-Attributes Attributes>
    --         in the topic
    --         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
    --
    --     -   You must specify @WEIGHTED@ for the value of @RoutingPolicy@.
    --
    --     -   You can\'t specify both @CNAME@ for @Type@ and settings for
    --         @HealthCheckConfig@. If you do, the request will fail with an
    --         @InvalidInput@ error.
    --
    -- [SRV]
    --     Route 53 returns the value for an @SRV@ record. The value for an
    --     @SRV@ record uses the following values:
    --
    --     @priority weight port service-hostname@
    --
    --     Note the following about the values:
    --
    --     -   The values of @priority@ and @weight@ are both set to @1@ and
    --         can\'t be changed.
    --
    --     -   The value of @port@ comes from the value that you specify for
    --         the @AWS_INSTANCE_PORT@ attribute when you submit a
    --         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
    --         request.
    --
    --     -   The value of @service-hostname@ is a concatenation of the
    --         following values:
    --
    --         -   The value that you specify for @InstanceId@ when you
    --             register an instance.
    --
    --         -   The name of the service.
    --
    --         -   The name of the namespace.
    --
    --         For example, if the value of @InstanceId@ is @test@, the name of
    --         the service is @backend@, and the name of the namespace is
    --         @example.com@, the value of @service-hostname@ is the following:
    --
    --         @test.backend.example.com@
    --
    --     If you specify settings for an @SRV@ record, note the following:
    --
    --     -   If you specify values for @AWS_INSTANCE_IPV4@,
    --         @AWS_INSTANCE_IPV6@, or both in the @RegisterInstance@ request,
    --         Cloud Map automatically creates @A@ and\/or @AAAA@ records that
    --         have the same name as the value of @service-hostname@ in the
    --         @SRV@ record. You can ignore these records.
    --
    --     -   If you\'re using a system that requires a specific @SRV@ format,
    --         such as HAProxy, see the
    --         <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html#cloudmap-CreateService-request-Name Name>
    --         element in the documentation about @CreateService@ for
    --         information about how to specify the correct name format.
    type' :: RecordType,
    -- | The amount of time, in seconds, that you want DNS resolvers to cache the
    -- settings for this record.
    --
    -- Alias records don\'t include a TTL because Route 53 uses the TTL for the
    -- Amazon Web Services resource that an alias record routes traffic to. If
    -- you include the @AWS_ALIAS_DNS_NAME@ attribute when you submit a
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
    -- request, the @TTL@ value is ignored. Always specify a TTL for the
    -- service; you can use a service to register instances that create either
    -- alias or non-alias records.
    ttl :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'dnsRecord_type' - The type of the resource, which indicates the type of value that
-- Route 53 returns in response to DNS queries. You can specify values for
-- @Type@ in the following combinations:
--
-- -   __@A@__
--
-- -   __@AAAA@__
--
-- -   __@A@__ and __@AAAA@__
--
-- -   __@SRV@__
--
-- -   __@CNAME@__
--
-- If you want Cloud Map to create a Route 53 alias record when you
-- register an instance, specify @A@ or @AAAA@ for @Type@.
--
-- You specify other settings, such as the IP address for @A@ and @AAAA@
-- records, when you register an instance. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
--
-- The following values are supported:
--
-- [A]
--     Route 53 returns the IP address of the resource in IPv4 format, such
--     as 192.0.2.44.
--
-- [AAAA]
--     Route 53 returns the IP address of the resource in IPv6 format, such
--     as 2001:0db8:85a3:0000:0000:abcd:0001:2345.
--
-- [CNAME]
--     Route 53 returns the domain name of the resource, such as
--     www.example.com. Note the following:
--
--     -   You specify the domain name that you want to route traffic to
--         when you register an instance. For more information, see
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html#cloudmap-RegisterInstance-request-Attributes Attributes>
--         in the topic
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
--
--     -   You must specify @WEIGHTED@ for the value of @RoutingPolicy@.
--
--     -   You can\'t specify both @CNAME@ for @Type@ and settings for
--         @HealthCheckConfig@. If you do, the request will fail with an
--         @InvalidInput@ error.
--
-- [SRV]
--     Route 53 returns the value for an @SRV@ record. The value for an
--     @SRV@ record uses the following values:
--
--     @priority weight port service-hostname@
--
--     Note the following about the values:
--
--     -   The values of @priority@ and @weight@ are both set to @1@ and
--         can\'t be changed.
--
--     -   The value of @port@ comes from the value that you specify for
--         the @AWS_INSTANCE_PORT@ attribute when you submit a
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
--         request.
--
--     -   The value of @service-hostname@ is a concatenation of the
--         following values:
--
--         -   The value that you specify for @InstanceId@ when you
--             register an instance.
--
--         -   The name of the service.
--
--         -   The name of the namespace.
--
--         For example, if the value of @InstanceId@ is @test@, the name of
--         the service is @backend@, and the name of the namespace is
--         @example.com@, the value of @service-hostname@ is the following:
--
--         @test.backend.example.com@
--
--     If you specify settings for an @SRV@ record, note the following:
--
--     -   If you specify values for @AWS_INSTANCE_IPV4@,
--         @AWS_INSTANCE_IPV6@, or both in the @RegisterInstance@ request,
--         Cloud Map automatically creates @A@ and\/or @AAAA@ records that
--         have the same name as the value of @service-hostname@ in the
--         @SRV@ record. You can ignore these records.
--
--     -   If you\'re using a system that requires a specific @SRV@ format,
--         such as HAProxy, see the
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html#cloudmap-CreateService-request-Name Name>
--         element in the documentation about @CreateService@ for
--         information about how to specify the correct name format.
--
-- 'ttl', 'dnsRecord_ttl' - The amount of time, in seconds, that you want DNS resolvers to cache the
-- settings for this record.
--
-- Alias records don\'t include a TTL because Route 53 uses the TTL for the
-- Amazon Web Services resource that an alias record routes traffic to. If
-- you include the @AWS_ALIAS_DNS_NAME@ attribute when you submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
-- request, the @TTL@ value is ignored. Always specify a TTL for the
-- service; you can use a service to register instances that create either
-- alias or non-alias records.
newDnsRecord ::
  -- | 'type''
  RecordType ->
  -- | 'ttl'
  Prelude.Natural ->
  DnsRecord
newDnsRecord pType_ pTTL_ =
  DnsRecord' {type' = pType_, ttl = pTTL_}

-- | The type of the resource, which indicates the type of value that
-- Route 53 returns in response to DNS queries. You can specify values for
-- @Type@ in the following combinations:
--
-- -   __@A@__
--
-- -   __@AAAA@__
--
-- -   __@A@__ and __@AAAA@__
--
-- -   __@SRV@__
--
-- -   __@CNAME@__
--
-- If you want Cloud Map to create a Route 53 alias record when you
-- register an instance, specify @A@ or @AAAA@ for @Type@.
--
-- You specify other settings, such as the IP address for @A@ and @AAAA@
-- records, when you register an instance. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
--
-- The following values are supported:
--
-- [A]
--     Route 53 returns the IP address of the resource in IPv4 format, such
--     as 192.0.2.44.
--
-- [AAAA]
--     Route 53 returns the IP address of the resource in IPv6 format, such
--     as 2001:0db8:85a3:0000:0000:abcd:0001:2345.
--
-- [CNAME]
--     Route 53 returns the domain name of the resource, such as
--     www.example.com. Note the following:
--
--     -   You specify the domain name that you want to route traffic to
--         when you register an instance. For more information, see
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html#cloudmap-RegisterInstance-request-Attributes Attributes>
--         in the topic
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>.
--
--     -   You must specify @WEIGHTED@ for the value of @RoutingPolicy@.
--
--     -   You can\'t specify both @CNAME@ for @Type@ and settings for
--         @HealthCheckConfig@. If you do, the request will fail with an
--         @InvalidInput@ error.
--
-- [SRV]
--     Route 53 returns the value for an @SRV@ record. The value for an
--     @SRV@ record uses the following values:
--
--     @priority weight port service-hostname@
--
--     Note the following about the values:
--
--     -   The values of @priority@ and @weight@ are both set to @1@ and
--         can\'t be changed.
--
--     -   The value of @port@ comes from the value that you specify for
--         the @AWS_INSTANCE_PORT@ attribute when you submit a
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
--         request.
--
--     -   The value of @service-hostname@ is a concatenation of the
--         following values:
--
--         -   The value that you specify for @InstanceId@ when you
--             register an instance.
--
--         -   The name of the service.
--
--         -   The name of the namespace.
--
--         For example, if the value of @InstanceId@ is @test@, the name of
--         the service is @backend@, and the name of the namespace is
--         @example.com@, the value of @service-hostname@ is the following:
--
--         @test.backend.example.com@
--
--     If you specify settings for an @SRV@ record, note the following:
--
--     -   If you specify values for @AWS_INSTANCE_IPV4@,
--         @AWS_INSTANCE_IPV6@, or both in the @RegisterInstance@ request,
--         Cloud Map automatically creates @A@ and\/or @AAAA@ records that
--         have the same name as the value of @service-hostname@ in the
--         @SRV@ record. You can ignore these records.
--
--     -   If you\'re using a system that requires a specific @SRV@ format,
--         such as HAProxy, see the
--         <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html#cloudmap-CreateService-request-Name Name>
--         element in the documentation about @CreateService@ for
--         information about how to specify the correct name format.
dnsRecord_type :: Lens.Lens' DnsRecord RecordType
dnsRecord_type = Lens.lens (\DnsRecord' {type'} -> type') (\s@DnsRecord' {} a -> s {type' = a} :: DnsRecord)

-- | The amount of time, in seconds, that you want DNS resolvers to cache the
-- settings for this record.
--
-- Alias records don\'t include a TTL because Route 53 uses the TTL for the
-- Amazon Web Services resource that an alias record routes traffic to. If
-- you include the @AWS_ALIAS_DNS_NAME@ attribute when you submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
-- request, the @TTL@ value is ignored. Always specify a TTL for the
-- service; you can use a service to register instances that create either
-- alias or non-alias records.
dnsRecord_ttl :: Lens.Lens' DnsRecord Prelude.Natural
dnsRecord_ttl = Lens.lens (\DnsRecord' {ttl} -> ttl) (\s@DnsRecord' {} a -> s {ttl = a} :: DnsRecord)

instance Data.FromJSON DnsRecord where
  parseJSON =
    Data.withObject
      "DnsRecord"
      ( \x ->
          DnsRecord'
            Prelude.<$> (x Data..: "Type") Prelude.<*> (x Data..: "TTL")
      )

instance Prelude.Hashable DnsRecord where
  hashWithSalt _salt DnsRecord' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ttl

instance Prelude.NFData DnsRecord where
  rnf DnsRecord' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf ttl

instance Data.ToJSON DnsRecord where
  toJSON DnsRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("TTL" Data..= ttl)
          ]
      )
