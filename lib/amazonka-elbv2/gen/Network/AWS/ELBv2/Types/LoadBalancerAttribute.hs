{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerAttribute
  ( LoadBalancerAttribute (..),

    -- * Smart constructor
    mkLoadBalancerAttribute,

    -- * Lenses
    lbaValue,
    lbaKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a load balancer attribute.
--
-- /See:/ 'mkLoadBalancerAttribute' smart constructor.
data LoadBalancerAttribute = LoadBalancerAttribute'
  { value ::
      Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerAttribute' with the minimum fields required to make a request.
--
-- * 'key' - The name of the attribute.
--
-- The following attribute is supported by all load balancers:
--
--     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
-- The following attributes are supported by both Application Load Balancers and Network Load Balancers:
--
--     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.
--
--
--     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.
--
--
-- The following attributes are supported by only Application Load Balancers:
--
--     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.
--
--
--     * @routing.http.desync_mitigation_mode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
--     * @routing.http.drop_invalid_header_fields.enabled@ - Indicates whether HTTP headers with invalid header fields are removed by the load balancer (@true@ ) or routed to targets (@false@ ). The default is @false@ .
--
--
--     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . Elastic Load Balancing requires that message header names contain only alphanumeric characters and hyphens.
--
--
--     * @waf.fail_open.enabled@ - Indicates whether to allow a WAF-enabled load balancer to route requests to targets if it is unable to forward the request to AWS WAF. The value is @true@ or @false@ . The default is @false@ .
--
--
-- The following attribute is supported by Network Load Balancers and Gateway Load Balancers:
--
--     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
-- * 'value' - The value of the attribute.
mkLoadBalancerAttribute ::
  LoadBalancerAttribute
mkLoadBalancerAttribute =
  LoadBalancerAttribute' {value = Lude.Nothing, key = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaValue :: Lens.Lens' LoadBalancerAttribute (Lude.Maybe Lude.Text)
lbaValue = Lens.lens (value :: LoadBalancerAttribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: LoadBalancerAttribute)
{-# DEPRECATED lbaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute.
--
-- The following attribute is supported by all load balancers:
--
--     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
-- The following attributes are supported by both Application Load Balancers and Network Load Balancers:
--
--     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.
--
--
--     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.
--
--
-- The following attributes are supported by only Application Load Balancers:
--
--     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.
--
--
--     * @routing.http.desync_mitigation_mode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
--     * @routing.http.drop_invalid_header_fields.enabled@ - Indicates whether HTTP headers with invalid header fields are removed by the load balancer (@true@ ) or routed to targets (@false@ ). The default is @false@ .
--
--
--     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . Elastic Load Balancing requires that message header names contain only alphanumeric characters and hyphens.
--
--
--     * @waf.fail_open.enabled@ - Indicates whether to allow a WAF-enabled load balancer to route requests to targets if it is unable to forward the request to AWS WAF. The value is @true@ or @false@ . The default is @false@ .
--
--
-- The following attribute is supported by Network Load Balancers and Gateway Load Balancers:
--
--     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaKey :: Lens.Lens' LoadBalancerAttribute (Lude.Maybe Lude.Text)
lbaKey = Lens.lens (key :: LoadBalancerAttribute -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: LoadBalancerAttribute)
{-# DEPRECATED lbaKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML LoadBalancerAttribute where
  parseXML x =
    LoadBalancerAttribute'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Key")

instance Lude.ToQuery LoadBalancerAttribute where
  toQuery LoadBalancerAttribute' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Key" Lude.=: key]
