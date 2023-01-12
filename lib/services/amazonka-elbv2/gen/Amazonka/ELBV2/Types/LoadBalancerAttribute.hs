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
-- Module      : Amazonka.ELBV2.Types.LoadBalancerAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.LoadBalancerAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a load balancer attribute.
--
-- /See:/ 'newLoadBalancerAttribute' smart constructor.
data LoadBalancerAttribute = LoadBalancerAttribute'
  { -- | The name of the attribute.
    --
    -- The following attributes are supported by all load balancers:
    --
    -- -   @deletion_protection.enabled@ - Indicates whether deletion
    --     protection is enabled. The value is @true@ or @false@. The default
    --     is @false@.
    --
    -- -   @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone
    --     load balancing is enabled. The possible values are @true@ and
    --     @false@. The default for Network Load Balancers and Gateway Load
    --     Balancers is @false@. The default for Application Load Balancers is
    --     @true@, and cannot be changed.
    --
    -- The following attributes are supported by both Application Load
    -- Balancers and Network Load Balancers:
    --
    -- -   @access_logs.s3.enabled@ - Indicates whether access logs are
    --     enabled. The value is @true@ or @false@. The default is @false@.
    --
    -- -   @access_logs.s3.bucket@ - The name of the S3 bucket for the access
    --     logs. This attribute is required if access logs are enabled. The
    --     bucket must exist in the same region as the load balancer and have a
    --     bucket policy that grants Elastic Load Balancing permissions to
    --     write to the bucket.
    --
    -- -   @access_logs.s3.prefix@ - The prefix for the location in the S3
    --     bucket for the access logs.
    --
    -- -   @ipv6.deny_all_igw_traffic@ - Blocks internet gateway (IGW) access
    --     to the load balancer. It is set to @false@ for internet-facing load
    --     balancers and @true@ for internal load balancers, preventing
    --     unintended access to your internal load balancer through an internet
    --     gateway.
    --
    -- The following attributes are supported by only Application Load
    -- Balancers:
    --
    -- -   @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds.
    --     The valid range is 1-4000 seconds. The default is 60 seconds.
    --
    -- -   @routing.http.desync_mitigation_mode@ - Determines how the load
    --     balancer handles requests that might pose a security risk to your
    --     application. The possible values are @monitor@, @defensive@, and
    --     @strictest@. The default is @defensive@.
    --
    -- -   @routing.http.drop_invalid_header_fields.enabled@ - Indicates
    --     whether HTTP headers with invalid header fields are removed by the
    --     load balancer (@true@) or routed to targets (@false@). The default
    --     is @false@.
    --
    -- -   @routing.http.preserve_host_header.enabled@ - Indicates whether the
    --     Application Load Balancer should preserve the @Host@ header in the
    --     HTTP request and send it to the target without any change. The
    --     possible values are @true@ and @false@. The default is @false@.
    --
    -- -   @routing.http.x_amzn_tls_version_and_cipher_suite.enabled@ -
    --     Indicates whether the two headers (@x-amzn-tls-version@ and
    --     @x-amzn-tls-cipher-suite@), which contain information about the
    --     negotiated TLS version and cipher suite, are added to the client
    --     request before sending it to the target. The @x-amzn-tls-version@
    --     header has information about the TLS protocol version negotiated
    --     with the client, and the @x-amzn-tls-cipher-suite@ header has
    --     information about the cipher suite negotiated with the client. Both
    --     headers are in OpenSSL format. The possible values for the attribute
    --     are @true@ and @false@. The default is @false@.
    --
    -- -   @routing.http.xff_client_port.enabled@ - Indicates whether the
    --     @X-Forwarded-For@ header should preserve the source port that the
    --     client used to connect to the load balancer. The possible values are
    --     @true@ and @false@. The default is @false@.
    --
    -- -   @routing.http.xff_header_processing.mode@ - Enables you to modify,
    --     preserve, or remove the @X-Forwarded-For@ header in the HTTP request
    --     before the Application Load Balancer sends the request to the
    --     target. The possible values are @append@, @preserve@, and @remove@.
    --     The default is @append@.
    --
    --     -   If the value is @append@, the Application Load Balancer adds the
    --         client IP address (of the last hop) to the @X-Forwarded-For@
    --         header in the HTTP request before it sends it to targets.
    --
    --     -   If the value is @preserve@ the Application Load Balancer
    --         preserves the @X-Forwarded-For@ header in the HTTP request, and
    --         sends it to targets without any change.
    --
    --     -   If the value is @remove@, the Application Load Balancer removes
    --         the @X-Forwarded-For@ header in the HTTP request before it sends
    --         it to targets.
    --
    -- -   @routing.http2.enabled@ - Indicates whether HTTP\/2 is enabled. The
    --     possible values are @true@ and @false@. The default is @true@.
    --     Elastic Load Balancing requires that message header names contain
    --     only alphanumeric characters and hyphens.
    --
    -- -   @waf.fail_open.enabled@ - Indicates whether to allow a WAF-enabled
    --     load balancer to route requests to targets if it is unable to
    --     forward the request to Amazon Web Services WAF. The possible values
    --     are @true@ and @false@. The default is @false@.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'loadBalancerAttribute_key' - The name of the attribute.
--
-- The following attributes are supported by all load balancers:
--
-- -   @deletion_protection.enabled@ - Indicates whether deletion
--     protection is enabled. The value is @true@ or @false@. The default
--     is @false@.
--
-- -   @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone
--     load balancing is enabled. The possible values are @true@ and
--     @false@. The default for Network Load Balancers and Gateway Load
--     Balancers is @false@. The default for Application Load Balancers is
--     @true@, and cannot be changed.
--
-- The following attributes are supported by both Application Load
-- Balancers and Network Load Balancers:
--
-- -   @access_logs.s3.enabled@ - Indicates whether access logs are
--     enabled. The value is @true@ or @false@. The default is @false@.
--
-- -   @access_logs.s3.bucket@ - The name of the S3 bucket for the access
--     logs. This attribute is required if access logs are enabled. The
--     bucket must exist in the same region as the load balancer and have a
--     bucket policy that grants Elastic Load Balancing permissions to
--     write to the bucket.
--
-- -   @access_logs.s3.prefix@ - The prefix for the location in the S3
--     bucket for the access logs.
--
-- -   @ipv6.deny_all_igw_traffic@ - Blocks internet gateway (IGW) access
--     to the load balancer. It is set to @false@ for internet-facing load
--     balancers and @true@ for internal load balancers, preventing
--     unintended access to your internal load balancer through an internet
--     gateway.
--
-- The following attributes are supported by only Application Load
-- Balancers:
--
-- -   @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds.
--     The valid range is 1-4000 seconds. The default is 60 seconds.
--
-- -   @routing.http.desync_mitigation_mode@ - Determines how the load
--     balancer handles requests that might pose a security risk to your
--     application. The possible values are @monitor@, @defensive@, and
--     @strictest@. The default is @defensive@.
--
-- -   @routing.http.drop_invalid_header_fields.enabled@ - Indicates
--     whether HTTP headers with invalid header fields are removed by the
--     load balancer (@true@) or routed to targets (@false@). The default
--     is @false@.
--
-- -   @routing.http.preserve_host_header.enabled@ - Indicates whether the
--     Application Load Balancer should preserve the @Host@ header in the
--     HTTP request and send it to the target without any change. The
--     possible values are @true@ and @false@. The default is @false@.
--
-- -   @routing.http.x_amzn_tls_version_and_cipher_suite.enabled@ -
--     Indicates whether the two headers (@x-amzn-tls-version@ and
--     @x-amzn-tls-cipher-suite@), which contain information about the
--     negotiated TLS version and cipher suite, are added to the client
--     request before sending it to the target. The @x-amzn-tls-version@
--     header has information about the TLS protocol version negotiated
--     with the client, and the @x-amzn-tls-cipher-suite@ header has
--     information about the cipher suite negotiated with the client. Both
--     headers are in OpenSSL format. The possible values for the attribute
--     are @true@ and @false@. The default is @false@.
--
-- -   @routing.http.xff_client_port.enabled@ - Indicates whether the
--     @X-Forwarded-For@ header should preserve the source port that the
--     client used to connect to the load balancer. The possible values are
--     @true@ and @false@. The default is @false@.
--
-- -   @routing.http.xff_header_processing.mode@ - Enables you to modify,
--     preserve, or remove the @X-Forwarded-For@ header in the HTTP request
--     before the Application Load Balancer sends the request to the
--     target. The possible values are @append@, @preserve@, and @remove@.
--     The default is @append@.
--
--     -   If the value is @append@, the Application Load Balancer adds the
--         client IP address (of the last hop) to the @X-Forwarded-For@
--         header in the HTTP request before it sends it to targets.
--
--     -   If the value is @preserve@ the Application Load Balancer
--         preserves the @X-Forwarded-For@ header in the HTTP request, and
--         sends it to targets without any change.
--
--     -   If the value is @remove@, the Application Load Balancer removes
--         the @X-Forwarded-For@ header in the HTTP request before it sends
--         it to targets.
--
-- -   @routing.http2.enabled@ - Indicates whether HTTP\/2 is enabled. The
--     possible values are @true@ and @false@. The default is @true@.
--     Elastic Load Balancing requires that message header names contain
--     only alphanumeric characters and hyphens.
--
-- -   @waf.fail_open.enabled@ - Indicates whether to allow a WAF-enabled
--     load balancer to route requests to targets if it is unable to
--     forward the request to Amazon Web Services WAF. The possible values
--     are @true@ and @false@. The default is @false@.
--
-- 'value', 'loadBalancerAttribute_value' - The value of the attribute.
newLoadBalancerAttribute ::
  LoadBalancerAttribute
newLoadBalancerAttribute =
  LoadBalancerAttribute'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the attribute.
--
-- The following attributes are supported by all load balancers:
--
-- -   @deletion_protection.enabled@ - Indicates whether deletion
--     protection is enabled. The value is @true@ or @false@. The default
--     is @false@.
--
-- -   @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone
--     load balancing is enabled. The possible values are @true@ and
--     @false@. The default for Network Load Balancers and Gateway Load
--     Balancers is @false@. The default for Application Load Balancers is
--     @true@, and cannot be changed.
--
-- The following attributes are supported by both Application Load
-- Balancers and Network Load Balancers:
--
-- -   @access_logs.s3.enabled@ - Indicates whether access logs are
--     enabled. The value is @true@ or @false@. The default is @false@.
--
-- -   @access_logs.s3.bucket@ - The name of the S3 bucket for the access
--     logs. This attribute is required if access logs are enabled. The
--     bucket must exist in the same region as the load balancer and have a
--     bucket policy that grants Elastic Load Balancing permissions to
--     write to the bucket.
--
-- -   @access_logs.s3.prefix@ - The prefix for the location in the S3
--     bucket for the access logs.
--
-- -   @ipv6.deny_all_igw_traffic@ - Blocks internet gateway (IGW) access
--     to the load balancer. It is set to @false@ for internet-facing load
--     balancers and @true@ for internal load balancers, preventing
--     unintended access to your internal load balancer through an internet
--     gateway.
--
-- The following attributes are supported by only Application Load
-- Balancers:
--
-- -   @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds.
--     The valid range is 1-4000 seconds. The default is 60 seconds.
--
-- -   @routing.http.desync_mitigation_mode@ - Determines how the load
--     balancer handles requests that might pose a security risk to your
--     application. The possible values are @monitor@, @defensive@, and
--     @strictest@. The default is @defensive@.
--
-- -   @routing.http.drop_invalid_header_fields.enabled@ - Indicates
--     whether HTTP headers with invalid header fields are removed by the
--     load balancer (@true@) or routed to targets (@false@). The default
--     is @false@.
--
-- -   @routing.http.preserve_host_header.enabled@ - Indicates whether the
--     Application Load Balancer should preserve the @Host@ header in the
--     HTTP request and send it to the target without any change. The
--     possible values are @true@ and @false@. The default is @false@.
--
-- -   @routing.http.x_amzn_tls_version_and_cipher_suite.enabled@ -
--     Indicates whether the two headers (@x-amzn-tls-version@ and
--     @x-amzn-tls-cipher-suite@), which contain information about the
--     negotiated TLS version and cipher suite, are added to the client
--     request before sending it to the target. The @x-amzn-tls-version@
--     header has information about the TLS protocol version negotiated
--     with the client, and the @x-amzn-tls-cipher-suite@ header has
--     information about the cipher suite negotiated with the client. Both
--     headers are in OpenSSL format. The possible values for the attribute
--     are @true@ and @false@. The default is @false@.
--
-- -   @routing.http.xff_client_port.enabled@ - Indicates whether the
--     @X-Forwarded-For@ header should preserve the source port that the
--     client used to connect to the load balancer. The possible values are
--     @true@ and @false@. The default is @false@.
--
-- -   @routing.http.xff_header_processing.mode@ - Enables you to modify,
--     preserve, or remove the @X-Forwarded-For@ header in the HTTP request
--     before the Application Load Balancer sends the request to the
--     target. The possible values are @append@, @preserve@, and @remove@.
--     The default is @append@.
--
--     -   If the value is @append@, the Application Load Balancer adds the
--         client IP address (of the last hop) to the @X-Forwarded-For@
--         header in the HTTP request before it sends it to targets.
--
--     -   If the value is @preserve@ the Application Load Balancer
--         preserves the @X-Forwarded-For@ header in the HTTP request, and
--         sends it to targets without any change.
--
--     -   If the value is @remove@, the Application Load Balancer removes
--         the @X-Forwarded-For@ header in the HTTP request before it sends
--         it to targets.
--
-- -   @routing.http2.enabled@ - Indicates whether HTTP\/2 is enabled. The
--     possible values are @true@ and @false@. The default is @true@.
--     Elastic Load Balancing requires that message header names contain
--     only alphanumeric characters and hyphens.
--
-- -   @waf.fail_open.enabled@ - Indicates whether to allow a WAF-enabled
--     load balancer to route requests to targets if it is unable to
--     forward the request to Amazon Web Services WAF. The possible values
--     are @true@ and @false@. The default is @false@.
loadBalancerAttribute_key :: Lens.Lens' LoadBalancerAttribute (Prelude.Maybe Prelude.Text)
loadBalancerAttribute_key = Lens.lens (\LoadBalancerAttribute' {key} -> key) (\s@LoadBalancerAttribute' {} a -> s {key = a} :: LoadBalancerAttribute)

-- | The value of the attribute.
loadBalancerAttribute_value :: Lens.Lens' LoadBalancerAttribute (Prelude.Maybe Prelude.Text)
loadBalancerAttribute_value = Lens.lens (\LoadBalancerAttribute' {value} -> value) (\s@LoadBalancerAttribute' {} a -> s {value = a} :: LoadBalancerAttribute)

instance Data.FromXML LoadBalancerAttribute where
  parseXML x =
    LoadBalancerAttribute'
      Prelude.<$> (x Data..@? "Key") Prelude.<*> (x Data..@? "Value")

instance Prelude.Hashable LoadBalancerAttribute where
  hashWithSalt _salt LoadBalancerAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData LoadBalancerAttribute where
  rnf LoadBalancerAttribute' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToQuery LoadBalancerAttribute where
  toQuery LoadBalancerAttribute' {..} =
    Prelude.mconcat
      ["Key" Data.=: key, "Value" Data.=: value]
