{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Lightsail load balancer.
--
-- /See:/ 'newLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The IP address type of the load balancer.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | An array of LoadBalancerTlsCertificateSummary objects that provide
    -- additional information about the SSL\/TLS certificates. For example, if
    -- @true@, the certificate is attached to the load balancer.
    tlsCertificateSummaries :: Prelude.Maybe [LoadBalancerTlsCertificateSummary],
    -- | An array of InstanceHealthSummary objects describing the health of the
    -- load balancer.
    instanceHealthSummary :: Prelude.Maybe [InstanceHealthSummary],
    -- | An array of public port settings for your load balancer. For HTTP, use
    -- port 80. For HTTPS, use port 443.
    publicPorts :: Prelude.Maybe [Prelude.Int],
    -- | A string to string map of the configuration options for your load
    -- balancer. Valid values are listed below.
    configurationOptions :: Prelude.Maybe (Prelude.HashMap LoadBalancerAttributeName Prelude.Text),
    -- | The port where the load balancer will direct traffic to your Lightsail
    -- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
    -- port 443.
    instancePort :: Prelude.Maybe Prelude.Int,
    -- | The date when your load balancer was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The resource type (e.g., @LoadBalancer@.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The status of your load balancer. Valid values are below.
    state :: Prelude.Maybe LoadBalancerState,
    -- | The name of the load balancer (e.g., @my-load-balancer@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The path you specified to perform your health checks. If no path is
    -- specified, the load balancer tries to make a request to the default
    -- (root) page.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The DNS name of your Lightsail load balancer.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The protocol you have enabled for your load balancer. Valid values are
    -- below.
    --
    -- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
    protocol :: Prelude.Maybe LoadBalancerProtocol,
    -- | The AWS Region where your load balancer was created (e.g.,
    -- @us-east-2a@). Lightsail automatically creates your load balancer across
    -- Availability Zones.
    location :: Prelude.Maybe ResourceLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ipAddressType = Prelude.Nothing,
      tlsCertificateSummaries = Prelude.Nothing,
      instanceHealthSummary = Prelude.Nothing,
      publicPorts = Prelude.Nothing,
      configurationOptions = Prelude.Nothing,
      instancePort = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      tags = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      protocol = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The IP address type of the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
loadBalancer_ipAddressType :: Lens.Lens' LoadBalancer (Prelude.Maybe IpAddressType)
loadBalancer_ipAddressType = Lens.lens (\LoadBalancer' {ipAddressType} -> ipAddressType) (\s@LoadBalancer' {} a -> s {ipAddressType = a} :: LoadBalancer)

-- | An array of LoadBalancerTlsCertificateSummary objects that provide
-- additional information about the SSL\/TLS certificates. For example, if
-- @true@, the certificate is attached to the load balancer.
loadBalancer_tlsCertificateSummaries :: Lens.Lens' LoadBalancer (Prelude.Maybe [LoadBalancerTlsCertificateSummary])
loadBalancer_tlsCertificateSummaries = Lens.lens (\LoadBalancer' {tlsCertificateSummaries} -> tlsCertificateSummaries) (\s@LoadBalancer' {} a -> s {tlsCertificateSummaries = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of InstanceHealthSummary objects describing the health of the
-- load balancer.
loadBalancer_instanceHealthSummary :: Lens.Lens' LoadBalancer (Prelude.Maybe [InstanceHealthSummary])
loadBalancer_instanceHealthSummary = Lens.lens (\LoadBalancer' {instanceHealthSummary} -> instanceHealthSummary) (\s@LoadBalancer' {} a -> s {instanceHealthSummary = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of public port settings for your load balancer. For HTTP, use
-- port 80. For HTTPS, use port 443.
loadBalancer_publicPorts :: Lens.Lens' LoadBalancer (Prelude.Maybe [Prelude.Int])
loadBalancer_publicPorts = Lens.lens (\LoadBalancer' {publicPorts} -> publicPorts) (\s@LoadBalancer' {} a -> s {publicPorts = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | A string to string map of the configuration options for your load
-- balancer. Valid values are listed below.
loadBalancer_configurationOptions :: Lens.Lens' LoadBalancer (Prelude.Maybe (Prelude.HashMap LoadBalancerAttributeName Prelude.Text))
loadBalancer_configurationOptions = Lens.lens (\LoadBalancer' {configurationOptions} -> configurationOptions) (\s@LoadBalancer' {} a -> s {configurationOptions = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | The port where the load balancer will direct traffic to your Lightsail
-- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
-- port 443.
loadBalancer_instancePort :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Int)
loadBalancer_instancePort = Lens.lens (\LoadBalancer' {instancePort} -> instancePort) (\s@LoadBalancer' {} a -> s {instancePort = a} :: LoadBalancer)

-- | The date when your load balancer was created.
loadBalancer_createdAt :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.UTCTime)
loadBalancer_createdAt = Lens.lens (\LoadBalancer' {createdAt} -> createdAt) (\s@LoadBalancer' {} a -> s {createdAt = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the load balancer.
loadBalancer_arn :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_arn = Lens.lens (\LoadBalancer' {arn} -> arn) (\s@LoadBalancer' {} a -> s {arn = a} :: LoadBalancer)

-- | The resource type (e.g., @LoadBalancer@.
loadBalancer_resourceType :: Lens.Lens' LoadBalancer (Prelude.Maybe ResourceType)
loadBalancer_resourceType = Lens.lens (\LoadBalancer' {resourceType} -> resourceType) (\s@LoadBalancer' {} a -> s {resourceType = a} :: LoadBalancer)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer. This code enables our
-- support team to look up your Lightsail information more easily.
loadBalancer_supportCode :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_supportCode = Lens.lens (\LoadBalancer' {supportCode} -> supportCode) (\s@LoadBalancer' {} a -> s {supportCode = a} :: LoadBalancer)

-- | The status of your load balancer. Valid values are below.
loadBalancer_state :: Lens.Lens' LoadBalancer (Prelude.Maybe LoadBalancerState)
loadBalancer_state = Lens.lens (\LoadBalancer' {state} -> state) (\s@LoadBalancer' {} a -> s {state = a} :: LoadBalancer)

-- | The name of the load balancer (e.g., @my-load-balancer@).
loadBalancer_name :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_name = Lens.lens (\LoadBalancer' {name} -> name) (\s@LoadBalancer' {} a -> s {name = a} :: LoadBalancer)

-- | The path you specified to perform your health checks. If no path is
-- specified, the load balancer tries to make a request to the default
-- (root) page.
loadBalancer_healthCheckPath :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_healthCheckPath = Lens.lens (\LoadBalancer' {healthCheckPath} -> healthCheckPath) (\s@LoadBalancer' {} a -> s {healthCheckPath = a} :: LoadBalancer)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
loadBalancer_tags :: Lens.Lens' LoadBalancer (Prelude.Maybe [Tag])
loadBalancer_tags = Lens.lens (\LoadBalancer' {tags} -> tags) (\s@LoadBalancer' {} a -> s {tags = a} :: LoadBalancer) Prelude.. Lens.mapping Prelude._Coerce

-- | The DNS name of your Lightsail load balancer.
loadBalancer_dnsName :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_dnsName = Lens.lens (\LoadBalancer' {dnsName} -> dnsName) (\s@LoadBalancer' {} a -> s {dnsName = a} :: LoadBalancer)

-- | The protocol you have enabled for your load balancer. Valid values are
-- below.
--
-- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
loadBalancer_protocol :: Lens.Lens' LoadBalancer (Prelude.Maybe LoadBalancerProtocol)
loadBalancer_protocol = Lens.lens (\LoadBalancer' {protocol} -> protocol) (\s@LoadBalancer' {} a -> s {protocol = a} :: LoadBalancer)

-- | The AWS Region where your load balancer was created (e.g.,
-- @us-east-2a@). Lightsail automatically creates your load balancer across
-- Availability Zones.
loadBalancer_location :: Lens.Lens' LoadBalancer (Prelude.Maybe ResourceLocation)
loadBalancer_location = Lens.lens (\LoadBalancer' {location} -> location) (\s@LoadBalancer' {} a -> s {location = a} :: LoadBalancer)

instance Prelude.FromJSON LoadBalancer where
  parseJSON =
    Prelude.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Prelude.<$> (x Prelude..:? "ipAddressType")
            Prelude.<*> ( x Prelude..:? "tlsCertificateSummaries"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "instanceHealthSummary"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "publicPorts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "configurationOptions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "instancePort")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "healthCheckPath")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "dnsName")
            Prelude.<*> (x Prelude..:? "protocol")
            Prelude.<*> (x Prelude..:? "location")
      )

instance Prelude.Hashable LoadBalancer

instance Prelude.NFData LoadBalancer
