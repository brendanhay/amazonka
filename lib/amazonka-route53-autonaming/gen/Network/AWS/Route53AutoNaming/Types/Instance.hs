{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iId
  , iAttributes
  , iCreatorRequestId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.AttrKey as Types
import qualified Network.AWS.Route53AutoNaming.Types.AttrValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about an instance that AWS Cloud Map creates when you submit a @RegisterInstance@ request.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { id :: Types.ResourceId
    -- ^ An identifier that you want to associate with the instance. Note the following:
--
--
--     * If the service that is specified by @ServiceId@ includes settings for an @SRV@ record, the value of @InstanceId@ is automatically included as part of the value for the @SRV@ record. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type> .
--
--
--     * You can use this value to update an existing instance.
--
--
--     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service. 
--
--
--     * If you specify an existing @InstanceId@ and @ServiceId@ , AWS Cloud Map updates the existing DNS records. If there's also an existing health check, AWS Cloud Map deletes the old health check and creates a new one. 
--
--
  , attributes :: Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue)
    -- ^ A string map that contains the following information for the service that you specify in @ServiceId@ :
--
--
--     * The attributes that apply to the records that are defined in the service. 
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
-- __AWS_ALIAS_DNS_NAME__ 
-- ____ 
-- If you want AWS Cloud Map to create a Route 53 alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> .
-- Note the following:
--
--     * The configuration for the service that is specified by @ServiceId@ must include settings for an @A@ record, an @AAAA@ record, or both.
--
--
--     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .
--
--
--     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, AWS Cloud Map will create the health check, but it won't associate the health check with the alias record.
--
--
--     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.
--
--
--     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes.
--
--
-- __AWS_EC2_INSTANCE_ID__ 
-- /HTTP namespaces only./ The Amazon EC2 instance ID for the instance. The @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
-- __AWS_INSTANCE_CNAME__ 
-- If the service configuration includes a @CNAME@ record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @CNAME@ record.
-- __AWS_INSTANCE_IPV4__ 
-- If the service configuration includes an @A@ record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @A@ record. If the service includes settings for an @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both.
-- __AWS_INSTANCE_IPV6__ 
-- If the service configuration includes an @AAAA@ record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @AAAA@ record. If the service includes settings for an @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both.
-- __AWS_INSTANCE_PORT__ 
-- If the service includes an @SRV@ record, the value that you want Route 53 to return for the port.
-- If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to. 
-- This value is required if you specified settings for an @SRV@ record or a Route 53 health check when you created the service.
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Types.ResourceId -- ^ 'id'
    -> Instance
mkInstance id
  = Instance'{id, attributes = Core.Nothing,
              creatorRequestId = Core.Nothing}

-- | An identifier that you want to associate with the instance. Note the following:
--
--
--     * If the service that is specified by @ServiceId@ includes settings for an @SRV@ record, the value of @InstanceId@ is automatically included as part of the value for the @SRV@ record. For more information, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type> .
--
--
--     * You can use this value to update an existing instance.
--
--
--     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service. 
--
--
--     * If you specify an existing @InstanceId@ and @ServiceId@ , AWS Cloud Map updates the existing DNS records. If there's also an existing health check, AWS Cloud Map deletes the old health check and creates a new one. 
--
--
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance Types.ResourceId
iId = Lens.field @"id"
{-# INLINEABLE iId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A string map that contains the following information for the service that you specify in @ServiceId@ :
--
--
--     * The attributes that apply to the records that are defined in the service. 
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
-- __AWS_ALIAS_DNS_NAME__ 
-- ____ 
-- If you want AWS Cloud Map to create a Route 53 alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> .
-- Note the following:
--
--     * The configuration for the service that is specified by @ServiceId@ must include settings for an @A@ record, an @AAAA@ record, or both.
--
--
--     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .
--
--
--     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, AWS Cloud Map will create the health check, but it won't associate the health check with the alias record.
--
--
--     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.
--
--
--     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes.
--
--
-- __AWS_EC2_INSTANCE_ID__ 
-- /HTTP namespaces only./ The Amazon EC2 instance ID for the instance. The @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
-- __AWS_INSTANCE_CNAME__ 
-- If the service configuration includes a @CNAME@ record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @CNAME@ record.
-- __AWS_INSTANCE_IPV4__ 
-- If the service configuration includes an @A@ record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @A@ record. If the service includes settings for an @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both.
-- __AWS_INSTANCE_IPV6__ 
-- If the service configuration includes an @AAAA@ record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
-- This value is required if the service specified by @ServiceId@ includes settings for an @AAAA@ record. If the service includes settings for an @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both.
-- __AWS_INSTANCE_PORT__ 
-- If the service includes an @SRV@ record, the value that you want Route 53 to return for the port.
-- If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to. 
-- This value is required if you specified settings for an @SRV@ record or a Route 53 health check when you created the service.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAttributes :: Lens.Lens' Instance (Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue))
iAttributes = Lens.field @"attributes"
{-# INLINEABLE iAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatorRequestId :: Lens.Lens' Instance (Core.Maybe Types.ResourceId)
iCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE iCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

instance Core.FromJSON Instance where
        parseJSON
          = Core.withObject "Instance" Core.$
              \ x ->
                Instance' Core.<$>
                  (x Core..: "Id") Core.<*> x Core..:? "Attributes" Core.<*>
                    x Core..:? "CreatorRequestId"
