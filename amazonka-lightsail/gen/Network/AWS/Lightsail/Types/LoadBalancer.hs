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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancer where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.IpAddressType
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes the Lightsail load balancer.
--
-- /See:/ 'newLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The IP address type of the load balancer.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | An array of LoadBalancerTlsCertificateSummary objects that provide
    -- additional information about the SSL\/TLS certificates. For example, if
    -- @true@, the certificate is attached to the load balancer.
    tlsCertificateSummaries :: Core.Maybe [LoadBalancerTlsCertificateSummary],
    -- | An array of InstanceHealthSummary objects describing the health of the
    -- load balancer.
    instanceHealthSummary :: Core.Maybe [InstanceHealthSummary],
    -- | An array of public port settings for your load balancer. For HTTP, use
    -- port 80. For HTTPS, use port 443.
    publicPorts :: Core.Maybe [Core.Int],
    -- | A string to string map of the configuration options for your load
    -- balancer. Valid values are listed below.
    configurationOptions :: Core.Maybe (Core.HashMap LoadBalancerAttributeName Core.Text),
    -- | The port where the load balancer will direct traffic to your Lightsail
    -- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
    -- port 443.
    instancePort :: Core.Maybe Core.Int,
    -- | The date when your load balancer was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    arn :: Core.Maybe Core.Text,
    -- | The resource type (e.g., @LoadBalancer@.
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The status of your load balancer. Valid values are below.
    state :: Core.Maybe LoadBalancerState,
    -- | The name of the load balancer (e.g., @my-load-balancer@).
    name :: Core.Maybe Core.Text,
    -- | The path you specified to perform your health checks. If no path is
    -- specified, the load balancer tries to make a request to the default
    -- (root) page.
    healthCheckPath :: Core.Maybe Core.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The DNS name of your Lightsail load balancer.
    dnsName :: Core.Maybe Core.Text,
    -- | The protocol you have enabled for your load balancer. Valid values are
    -- below.
    --
    -- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
    protocol :: Core.Maybe LoadBalancerProtocol,
    -- | The AWS Region where your load balancer was created (e.g.,
    -- @us-east-2a@). Lightsail automatically creates your load balancer across
    -- Availability Zones.
    location :: Core.Maybe ResourceLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'loadBalancer_ipAddressType' - The IP address type of the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'tlsCertificateSummaries', 'loadBalancer_tlsCertificateSummaries' - An array of LoadBalancerTlsCertificateSummary objects that provide
-- additional information about the SSL\/TLS certificates. For example, if
-- @true@, the certificate is attached to the load balancer.
--
-- 'instanceHealthSummary', 'loadBalancer_instanceHealthSummary' - An array of InstanceHealthSummary objects describing the health of the
-- load balancer.
--
-- 'publicPorts', 'loadBalancer_publicPorts' - An array of public port settings for your load balancer. For HTTP, use
-- port 80. For HTTPS, use port 443.
--
-- 'configurationOptions', 'loadBalancer_configurationOptions' - A string to string map of the configuration options for your load
-- balancer. Valid values are listed below.
--
-- 'instancePort', 'loadBalancer_instancePort' - The port where the load balancer will direct traffic to your Lightsail
-- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
-- port 443.
--
-- 'createdAt', 'loadBalancer_createdAt' - The date when your load balancer was created.
--
-- 'arn', 'loadBalancer_arn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'resourceType', 'loadBalancer_resourceType' - The resource type (e.g., @LoadBalancer@.
--
-- 'supportCode', 'loadBalancer_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer. This code enables our
-- support team to look up your Lightsail information more easily.
--
-- 'state', 'loadBalancer_state' - The status of your load balancer. Valid values are below.
--
-- 'name', 'loadBalancer_name' - The name of the load balancer (e.g., @my-load-balancer@).
--
-- 'healthCheckPath', 'loadBalancer_healthCheckPath' - The path you specified to perform your health checks. If no path is
-- specified, the load balancer tries to make a request to the default
-- (root) page.
--
-- 'tags', 'loadBalancer_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'dnsName', 'loadBalancer_dnsName' - The DNS name of your Lightsail load balancer.
--
-- 'protocol', 'loadBalancer_protocol' - The protocol you have enabled for your load balancer. Valid values are
-- below.
--
-- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
--
-- 'location', 'loadBalancer_location' - The AWS Region where your load balancer was created (e.g.,
-- @us-east-2a@). Lightsail automatically creates your load balancer across
-- Availability Zones.
newLoadBalancer ::
  LoadBalancer
newLoadBalancer =
  LoadBalancer'
    { ipAddressType = Core.Nothing,
      tlsCertificateSummaries = Core.Nothing,
      instanceHealthSummary = Core.Nothing,
      publicPorts = Core.Nothing,
      configurationOptions = Core.Nothing,
      instancePort = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      healthCheckPath = Core.Nothing,
      tags = Core.Nothing,
      dnsName = Core.Nothing,
      protocol = Core.Nothing,
      location = Core.Nothing
    }

-- | The IP address type of the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
loadBalancer_ipAddressType :: Lens.Lens' LoadBalancer (Core.Maybe IpAddressType)
loadBalancer_ipAddressType = Lens.lens (\LoadBalancer' {ipAddressType} -> ipAddressType) (\s@LoadBalancer' {} a -> s {ipAddressType = a} :: LoadBalancer)

-- | An array of LoadBalancerTlsCertificateSummary objects that provide
-- additional information about the SSL\/TLS certificates. For example, if
-- @true@, the certificate is attached to the load balancer.
loadBalancer_tlsCertificateSummaries :: Lens.Lens' LoadBalancer (Core.Maybe [LoadBalancerTlsCertificateSummary])
loadBalancer_tlsCertificateSummaries = Lens.lens (\LoadBalancer' {tlsCertificateSummaries} -> tlsCertificateSummaries) (\s@LoadBalancer' {} a -> s {tlsCertificateSummaries = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | An array of InstanceHealthSummary objects describing the health of the
-- load balancer.
loadBalancer_instanceHealthSummary :: Lens.Lens' LoadBalancer (Core.Maybe [InstanceHealthSummary])
loadBalancer_instanceHealthSummary = Lens.lens (\LoadBalancer' {instanceHealthSummary} -> instanceHealthSummary) (\s@LoadBalancer' {} a -> s {instanceHealthSummary = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | An array of public port settings for your load balancer. For HTTP, use
-- port 80. For HTTPS, use port 443.
loadBalancer_publicPorts :: Lens.Lens' LoadBalancer (Core.Maybe [Core.Int])
loadBalancer_publicPorts = Lens.lens (\LoadBalancer' {publicPorts} -> publicPorts) (\s@LoadBalancer' {} a -> s {publicPorts = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | A string to string map of the configuration options for your load
-- balancer. Valid values are listed below.
loadBalancer_configurationOptions :: Lens.Lens' LoadBalancer (Core.Maybe (Core.HashMap LoadBalancerAttributeName Core.Text))
loadBalancer_configurationOptions = Lens.lens (\LoadBalancer' {configurationOptions} -> configurationOptions) (\s@LoadBalancer' {} a -> s {configurationOptions = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The port where the load balancer will direct traffic to your Lightsail
-- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
-- port 443.
loadBalancer_instancePort :: Lens.Lens' LoadBalancer (Core.Maybe Core.Int)
loadBalancer_instancePort = Lens.lens (\LoadBalancer' {instancePort} -> instancePort) (\s@LoadBalancer' {} a -> s {instancePort = a} :: LoadBalancer)

-- | The date when your load balancer was created.
loadBalancer_createdAt :: Lens.Lens' LoadBalancer (Core.Maybe Core.UTCTime)
loadBalancer_createdAt = Lens.lens (\LoadBalancer' {createdAt} -> createdAt) (\s@LoadBalancer' {} a -> s {createdAt = a} :: LoadBalancer) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the load balancer.
loadBalancer_arn :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_arn = Lens.lens (\LoadBalancer' {arn} -> arn) (\s@LoadBalancer' {} a -> s {arn = a} :: LoadBalancer)

-- | The resource type (e.g., @LoadBalancer@.
loadBalancer_resourceType :: Lens.Lens' LoadBalancer (Core.Maybe ResourceType)
loadBalancer_resourceType = Lens.lens (\LoadBalancer' {resourceType} -> resourceType) (\s@LoadBalancer' {} a -> s {resourceType = a} :: LoadBalancer)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer. This code enables our
-- support team to look up your Lightsail information more easily.
loadBalancer_supportCode :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_supportCode = Lens.lens (\LoadBalancer' {supportCode} -> supportCode) (\s@LoadBalancer' {} a -> s {supportCode = a} :: LoadBalancer)

-- | The status of your load balancer. Valid values are below.
loadBalancer_state :: Lens.Lens' LoadBalancer (Core.Maybe LoadBalancerState)
loadBalancer_state = Lens.lens (\LoadBalancer' {state} -> state) (\s@LoadBalancer' {} a -> s {state = a} :: LoadBalancer)

-- | The name of the load balancer (e.g., @my-load-balancer@).
loadBalancer_name :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_name = Lens.lens (\LoadBalancer' {name} -> name) (\s@LoadBalancer' {} a -> s {name = a} :: LoadBalancer)

-- | The path you specified to perform your health checks. If no path is
-- specified, the load balancer tries to make a request to the default
-- (root) page.
loadBalancer_healthCheckPath :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_healthCheckPath = Lens.lens (\LoadBalancer' {healthCheckPath} -> healthCheckPath) (\s@LoadBalancer' {} a -> s {healthCheckPath = a} :: LoadBalancer)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
loadBalancer_tags :: Lens.Lens' LoadBalancer (Core.Maybe [Tag])
loadBalancer_tags = Lens.lens (\LoadBalancer' {tags} -> tags) (\s@LoadBalancer' {} a -> s {tags = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The DNS name of your Lightsail load balancer.
loadBalancer_dnsName :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_dnsName = Lens.lens (\LoadBalancer' {dnsName} -> dnsName) (\s@LoadBalancer' {} a -> s {dnsName = a} :: LoadBalancer)

-- | The protocol you have enabled for your load balancer. Valid values are
-- below.
--
-- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
loadBalancer_protocol :: Lens.Lens' LoadBalancer (Core.Maybe LoadBalancerProtocol)
loadBalancer_protocol = Lens.lens (\LoadBalancer' {protocol} -> protocol) (\s@LoadBalancer' {} a -> s {protocol = a} :: LoadBalancer)

-- | The AWS Region where your load balancer was created (e.g.,
-- @us-east-2a@). Lightsail automatically creates your load balancer across
-- Availability Zones.
loadBalancer_location :: Lens.Lens' LoadBalancer (Core.Maybe ResourceLocation)
loadBalancer_location = Lens.lens (\LoadBalancer' {location} -> location) (\s@LoadBalancer' {} a -> s {location = a} :: LoadBalancer)

instance Core.FromJSON LoadBalancer where
  parseJSON =
    Core.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Core.<$> (x Core..:? "ipAddressType")
            Core.<*> ( x Core..:? "tlsCertificateSummaries"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "instanceHealthSummary"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "publicPorts" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "configurationOptions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "instancePort")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "healthCheckPath")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "dnsName")
            Core.<*> (x Core..:? "protocol")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable LoadBalancer

instance Core.NFData LoadBalancer
