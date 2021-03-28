{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.DnsRecord
  ( DnsRecord (..)
  -- * Smart constructor
  , mkDnsRecord
  -- * Lenses
  , drType
  , drTTL
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.RecordType as Types

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /See:/ 'mkDnsRecord' smart constructor.
data DnsRecord = DnsRecord'
  { type' :: Types.RecordType
    -- ^ The type of the resource, which indicates the type of value that Route 53 returns in response to DNS queries. You can specify values for @Type@ in the following combinations:
--
--
--     * @A@ 
--
--
--     * @AAAA@ 
--
--
--     * @A@ and @AAAA@ 
--
--
--     * @SRV@ 
--
--
--     * @CNAME@ 
--
--
-- If you want AWS Cloud Map to create a Route 53 alias record when you register an instance, specify @A@ or @AAAA@ for @Type@ .
-- You specify other settings, such as the IP address for @A@ and @AAAA@ records, when you register an instance. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> .
-- The following values are supported:
-- @A@ ____ 
-- Route 53 returns the IP address of the resource in IPv4 format, such as 192.0.2.44.
-- @AAAA@ ____ 
-- Route 53 returns the IP address of the resource in IPv6 format, such as 2001:0db8:85a3:0000:0000:abcd:0001:2345.
-- @CNAME@ ____ 
-- Route 53 returns the domain name of the resource, such as www.example.com. Note the following:
--
--     * You specify the domain name that you want to route traffic to when you register an instance. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html#cloudmap-RegisterInstance-request-Attributes Attributes> in the topic <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> .
--
--
--     * You must specify @WEIGHTED@ for the value of @RoutingPolicy@ .
--
--
--     * You can't specify both @CNAME@ for @Type@ and settings for @HealthCheckConfig@ . If you do, the request will fail with an @InvalidInput@ error.
--
--
-- __SRV__ 
-- Route 53 returns the value for an @SRV@ record. The value for an @SRV@ record uses the following values:
-- @priority weight port service-hostname@ 
-- Note the following about the values:
--
--     * The values of @priority@ and @weight@ are both set to @1@ and can't be changed. 
--
--
--     * The value of @port@ comes from the value that you specify for the @AWS_INSTANCE_PORT@ attribute when you submit a <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request. 
--
--
--     * The value of @service-hostname@ is a concatenation of the following values:
--
--     * The value that you specify for @InstanceId@ when you register an instance.
--
--
--     * The name of the service.
--
--
--     * The name of the namespace. 
--
--
-- For example, if the value of @InstanceId@ is @test@ , the name of the service is @backend@ , and the name of the namespace is @example.com@ , the value of @service-hostname@ is:
-- @test.backend.example.com@ 
--
--
-- If you specify settings for an @SRV@ record, note the following:
--
--     * If you specify values for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both in the @RegisterInstance@ request, AWS Cloud Map automatically creates @A@ and/or @AAAA@ records that have the same name as the value of @service-hostname@ in the @SRV@ record. You can ignore these records.
--
--
--     * If you're using a system that requires a specific @SRV@ format, such as HAProxy, see the <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html#cloudmap-CreateService-request-Name Name> element in the documentation about @CreateService@ for information about how to specify the correct name format.
--
--
  , ttl :: Core.Natural
    -- ^ The amount of time, in seconds, that you want DNS resolvers to cache the settings for this record.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DnsRecord' value with any optional fields omitted.
mkDnsRecord
    :: Types.RecordType -- ^ 'type\''
    -> Core.Natural -- ^ 'ttl'
    -> DnsRecord
mkDnsRecord type' ttl = DnsRecord'{type', ttl}

-- | The type of the resource, which indicates the type of value that Route 53 returns in response to DNS queries. You can specify values for @Type@ in the following combinations:
--
--
--     * @A@ 
--
--
--     * @AAAA@ 
--
--
--     * @A@ and @AAAA@ 
--
--
--     * @SRV@ 
--
--
--     * @CNAME@ 
--
--
-- If you want AWS Cloud Map to create a Route 53 alias record when you register an instance, specify @A@ or @AAAA@ for @Type@ .
-- You specify other settings, such as the IP address for @A@ and @AAAA@ records, when you register an instance. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> .
-- The following values are supported:
-- @A@ ____ 
-- Route 53 returns the IP address of the resource in IPv4 format, such as 192.0.2.44.
-- @AAAA@ ____ 
-- Route 53 returns the IP address of the resource in IPv6 format, such as 2001:0db8:85a3:0000:0000:abcd:0001:2345.
-- @CNAME@ ____ 
-- Route 53 returns the domain name of the resource, such as www.example.com. Note the following:
--
--     * You specify the domain name that you want to route traffic to when you register an instance. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html#cloudmap-RegisterInstance-request-Attributes Attributes> in the topic <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> .
--
--
--     * You must specify @WEIGHTED@ for the value of @RoutingPolicy@ .
--
--
--     * You can't specify both @CNAME@ for @Type@ and settings for @HealthCheckConfig@ . If you do, the request will fail with an @InvalidInput@ error.
--
--
-- __SRV__ 
-- Route 53 returns the value for an @SRV@ record. The value for an @SRV@ record uses the following values:
-- @priority weight port service-hostname@ 
-- Note the following about the values:
--
--     * The values of @priority@ and @weight@ are both set to @1@ and can't be changed. 
--
--
--     * The value of @port@ comes from the value that you specify for the @AWS_INSTANCE_PORT@ attribute when you submit a <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request. 
--
--
--     * The value of @service-hostname@ is a concatenation of the following values:
--
--     * The value that you specify for @InstanceId@ when you register an instance.
--
--
--     * The name of the service.
--
--
--     * The name of the namespace. 
--
--
-- For example, if the value of @InstanceId@ is @test@ , the name of the service is @backend@ , and the name of the namespace is @example.com@ , the value of @service-hostname@ is:
-- @test.backend.example.com@ 
--
--
-- If you specify settings for an @SRV@ record, note the following:
--
--     * If you specify values for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both in the @RegisterInstance@ request, AWS Cloud Map automatically creates @A@ and/or @AAAA@ records that have the same name as the value of @service-hostname@ in the @SRV@ record. You can ignore these records.
--
--
--     * If you're using a system that requires a specific @SRV@ format, such as HAProxy, see the <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html#cloudmap-CreateService-request-Name Name> element in the documentation about @CreateService@ for information about how to specify the correct name format.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drType :: Lens.Lens' DnsRecord Types.RecordType
drType = Lens.field @"type'"
{-# INLINEABLE drType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The amount of time, in seconds, that you want DNS resolvers to cache the settings for this record.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTTL :: Lens.Lens' DnsRecord Core.Natural
drTTL = Lens.field @"ttl"
{-# INLINEABLE drTTL #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

instance Core.FromJSON DnsRecord where
        toJSON DnsRecord{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'), Core.Just ("TTL" Core..= ttl)])

instance Core.FromJSON DnsRecord where
        parseJSON
          = Core.withObject "DnsRecord" Core.$
              \ x ->
                DnsRecord' Core.<$> (x Core..: "Type") Core.<*> x Core..: "TTL"
