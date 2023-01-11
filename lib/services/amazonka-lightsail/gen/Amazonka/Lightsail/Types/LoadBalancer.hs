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
-- Module      : Amazonka.Lightsail.Types.LoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.InstanceHealthSummary
import Amazonka.Lightsail.Types.IpAddressType
import Amazonka.Lightsail.Types.LoadBalancerAttributeName
import Amazonka.Lightsail.Types.LoadBalancerProtocol
import Amazonka.Lightsail.Types.LoadBalancerState
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer.
--
-- /See:/ 'newLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A string to string map of the configuration options for your load
    -- balancer. Valid values are listed below.
    configurationOptions :: Prelude.Maybe (Prelude.HashMap LoadBalancerAttributeName Prelude.Text),
    -- | The date when your load balancer was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The DNS name of your Lightsail load balancer.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The path you specified to perform your health checks. If no path is
    -- specified, the load balancer tries to make a request to the default
    -- (root) page.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether HTTPS redirection is enabled for
    -- the load balancer.
    httpsRedirectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An array of InstanceHealthSummary objects describing the health of the
    -- load balancer.
    instanceHealthSummary :: Prelude.Maybe [InstanceHealthSummary],
    -- | The port where the load balancer will direct traffic to your Lightsail
    -- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
    -- port 443.
    instancePort :: Prelude.Maybe Prelude.Int,
    -- | The IP address type of the load balancer.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The AWS Region where your load balancer was created (e.g.,
    -- @us-east-2a@). Lightsail automatically creates your load balancer across
    -- Availability Zones.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the load balancer (e.g., @my-load-balancer@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocol you have enabled for your load balancer. Valid values are
    -- below.
    --
    -- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
    protocol :: Prelude.Maybe LoadBalancerProtocol,
    -- | An array of public port settings for your load balancer. For HTTP, use
    -- port 80. For HTTPS, use port 443.
    publicPorts :: Prelude.Maybe [Prelude.Int],
    -- | The resource type (e.g., @LoadBalancer@.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The status of your load balancer. Valid values are below.
    state :: Prelude.Maybe LoadBalancerState,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | An array of LoadBalancerTlsCertificateSummary objects that provide
    -- additional information about the SSL\/TLS certificates. For example, if
    -- @true@, the certificate is attached to the load balancer.
    tlsCertificateSummaries :: Prelude.Maybe [LoadBalancerTlsCertificateSummary],
    -- | The name of the TLS security policy for the load balancer.
    tlsPolicyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'loadBalancer_arn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'configurationOptions', 'loadBalancer_configurationOptions' - A string to string map of the configuration options for your load
-- balancer. Valid values are listed below.
--
-- 'createdAt', 'loadBalancer_createdAt' - The date when your load balancer was created.
--
-- 'dnsName', 'loadBalancer_dnsName' - The DNS name of your Lightsail load balancer.
--
-- 'healthCheckPath', 'loadBalancer_healthCheckPath' - The path you specified to perform your health checks. If no path is
-- specified, the load balancer tries to make a request to the default
-- (root) page.
--
-- 'httpsRedirectionEnabled', 'loadBalancer_httpsRedirectionEnabled' - A Boolean value that indicates whether HTTPS redirection is enabled for
-- the load balancer.
--
-- 'instanceHealthSummary', 'loadBalancer_instanceHealthSummary' - An array of InstanceHealthSummary objects describing the health of the
-- load balancer.
--
-- 'instancePort', 'loadBalancer_instancePort' - The port where the load balancer will direct traffic to your Lightsail
-- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
-- port 443.
--
-- 'ipAddressType', 'loadBalancer_ipAddressType' - The IP address type of the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'location', 'loadBalancer_location' - The AWS Region where your load balancer was created (e.g.,
-- @us-east-2a@). Lightsail automatically creates your load balancer across
-- Availability Zones.
--
-- 'name', 'loadBalancer_name' - The name of the load balancer (e.g., @my-load-balancer@).
--
-- 'protocol', 'loadBalancer_protocol' - The protocol you have enabled for your load balancer. Valid values are
-- below.
--
-- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
--
-- 'publicPorts', 'loadBalancer_publicPorts' - An array of public port settings for your load balancer. For HTTP, use
-- port 80. For HTTPS, use port 443.
--
-- 'resourceType', 'loadBalancer_resourceType' - The resource type (e.g., @LoadBalancer@.
--
-- 'state', 'loadBalancer_state' - The status of your load balancer. Valid values are below.
--
-- 'supportCode', 'loadBalancer_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer. This code enables our
-- support team to look up your Lightsail information more easily.
--
-- 'tags', 'loadBalancer_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'tlsCertificateSummaries', 'loadBalancer_tlsCertificateSummaries' - An array of LoadBalancerTlsCertificateSummary objects that provide
-- additional information about the SSL\/TLS certificates. For example, if
-- @true@, the certificate is attached to the load balancer.
--
-- 'tlsPolicyName', 'loadBalancer_tlsPolicyName' - The name of the TLS security policy for the load balancer.
newLoadBalancer ::
  LoadBalancer
newLoadBalancer =
  LoadBalancer'
    { arn = Prelude.Nothing,
      configurationOptions = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      httpsRedirectionEnabled = Prelude.Nothing,
      instanceHealthSummary = Prelude.Nothing,
      instancePort = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      protocol = Prelude.Nothing,
      publicPorts = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      tlsCertificateSummaries = Prelude.Nothing,
      tlsPolicyName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
loadBalancer_arn :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_arn = Lens.lens (\LoadBalancer' {arn} -> arn) (\s@LoadBalancer' {} a -> s {arn = a} :: LoadBalancer)

-- | A string to string map of the configuration options for your load
-- balancer. Valid values are listed below.
loadBalancer_configurationOptions :: Lens.Lens' LoadBalancer (Prelude.Maybe (Prelude.HashMap LoadBalancerAttributeName Prelude.Text))
loadBalancer_configurationOptions = Lens.lens (\LoadBalancer' {configurationOptions} -> configurationOptions) (\s@LoadBalancer' {} a -> s {configurationOptions = a} :: LoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The date when your load balancer was created.
loadBalancer_createdAt :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.UTCTime)
loadBalancer_createdAt = Lens.lens (\LoadBalancer' {createdAt} -> createdAt) (\s@LoadBalancer' {} a -> s {createdAt = a} :: LoadBalancer) Prelude.. Lens.mapping Data._Time

-- | The DNS name of your Lightsail load balancer.
loadBalancer_dnsName :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_dnsName = Lens.lens (\LoadBalancer' {dnsName} -> dnsName) (\s@LoadBalancer' {} a -> s {dnsName = a} :: LoadBalancer)

-- | The path you specified to perform your health checks. If no path is
-- specified, the load balancer tries to make a request to the default
-- (root) page.
loadBalancer_healthCheckPath :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_healthCheckPath = Lens.lens (\LoadBalancer' {healthCheckPath} -> healthCheckPath) (\s@LoadBalancer' {} a -> s {healthCheckPath = a} :: LoadBalancer)

-- | A Boolean value that indicates whether HTTPS redirection is enabled for
-- the load balancer.
loadBalancer_httpsRedirectionEnabled :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Bool)
loadBalancer_httpsRedirectionEnabled = Lens.lens (\LoadBalancer' {httpsRedirectionEnabled} -> httpsRedirectionEnabled) (\s@LoadBalancer' {} a -> s {httpsRedirectionEnabled = a} :: LoadBalancer)

-- | An array of InstanceHealthSummary objects describing the health of the
-- load balancer.
loadBalancer_instanceHealthSummary :: Lens.Lens' LoadBalancer (Prelude.Maybe [InstanceHealthSummary])
loadBalancer_instanceHealthSummary = Lens.lens (\LoadBalancer' {instanceHealthSummary} -> instanceHealthSummary) (\s@LoadBalancer' {} a -> s {instanceHealthSummary = a} :: LoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The port where the load balancer will direct traffic to your Lightsail
-- instances. For HTTP traffic, it\'s port 80. For HTTPS traffic, it\'s
-- port 443.
loadBalancer_instancePort :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Int)
loadBalancer_instancePort = Lens.lens (\LoadBalancer' {instancePort} -> instancePort) (\s@LoadBalancer' {} a -> s {instancePort = a} :: LoadBalancer)

-- | The IP address type of the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
loadBalancer_ipAddressType :: Lens.Lens' LoadBalancer (Prelude.Maybe IpAddressType)
loadBalancer_ipAddressType = Lens.lens (\LoadBalancer' {ipAddressType} -> ipAddressType) (\s@LoadBalancer' {} a -> s {ipAddressType = a} :: LoadBalancer)

-- | The AWS Region where your load balancer was created (e.g.,
-- @us-east-2a@). Lightsail automatically creates your load balancer across
-- Availability Zones.
loadBalancer_location :: Lens.Lens' LoadBalancer (Prelude.Maybe ResourceLocation)
loadBalancer_location = Lens.lens (\LoadBalancer' {location} -> location) (\s@LoadBalancer' {} a -> s {location = a} :: LoadBalancer)

-- | The name of the load balancer (e.g., @my-load-balancer@).
loadBalancer_name :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_name = Lens.lens (\LoadBalancer' {name} -> name) (\s@LoadBalancer' {} a -> s {name = a} :: LoadBalancer)

-- | The protocol you have enabled for your load balancer. Valid values are
-- below.
--
-- You can\'t just have @HTTP_HTTPS@, but you can have just @HTTP@.
loadBalancer_protocol :: Lens.Lens' LoadBalancer (Prelude.Maybe LoadBalancerProtocol)
loadBalancer_protocol = Lens.lens (\LoadBalancer' {protocol} -> protocol) (\s@LoadBalancer' {} a -> s {protocol = a} :: LoadBalancer)

-- | An array of public port settings for your load balancer. For HTTP, use
-- port 80. For HTTPS, use port 443.
loadBalancer_publicPorts :: Lens.Lens' LoadBalancer (Prelude.Maybe [Prelude.Int])
loadBalancer_publicPorts = Lens.lens (\LoadBalancer' {publicPorts} -> publicPorts) (\s@LoadBalancer' {} a -> s {publicPorts = a} :: LoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The resource type (e.g., @LoadBalancer@.
loadBalancer_resourceType :: Lens.Lens' LoadBalancer (Prelude.Maybe ResourceType)
loadBalancer_resourceType = Lens.lens (\LoadBalancer' {resourceType} -> resourceType) (\s@LoadBalancer' {} a -> s {resourceType = a} :: LoadBalancer)

-- | The status of your load balancer. Valid values are below.
loadBalancer_state :: Lens.Lens' LoadBalancer (Prelude.Maybe LoadBalancerState)
loadBalancer_state = Lens.lens (\LoadBalancer' {state} -> state) (\s@LoadBalancer' {} a -> s {state = a} :: LoadBalancer)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer. This code enables our
-- support team to look up your Lightsail information more easily.
loadBalancer_supportCode :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_supportCode = Lens.lens (\LoadBalancer' {supportCode} -> supportCode) (\s@LoadBalancer' {} a -> s {supportCode = a} :: LoadBalancer)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
loadBalancer_tags :: Lens.Lens' LoadBalancer (Prelude.Maybe [Tag])
loadBalancer_tags = Lens.lens (\LoadBalancer' {tags} -> tags) (\s@LoadBalancer' {} a -> s {tags = a} :: LoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | An array of LoadBalancerTlsCertificateSummary objects that provide
-- additional information about the SSL\/TLS certificates. For example, if
-- @true@, the certificate is attached to the load balancer.
loadBalancer_tlsCertificateSummaries :: Lens.Lens' LoadBalancer (Prelude.Maybe [LoadBalancerTlsCertificateSummary])
loadBalancer_tlsCertificateSummaries = Lens.lens (\LoadBalancer' {tlsCertificateSummaries} -> tlsCertificateSummaries) (\s@LoadBalancer' {} a -> s {tlsCertificateSummaries = a} :: LoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The name of the TLS security policy for the load balancer.
loadBalancer_tlsPolicyName :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_tlsPolicyName = Lens.lens (\LoadBalancer' {tlsPolicyName} -> tlsPolicyName) (\s@LoadBalancer' {} a -> s {tlsPolicyName = a} :: LoadBalancer)

instance Data.FromJSON LoadBalancer where
  parseJSON =
    Data.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> ( x Data..:? "configurationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "dnsName")
            Prelude.<*> (x Data..:? "healthCheckPath")
            Prelude.<*> (x Data..:? "httpsRedirectionEnabled")
            Prelude.<*> ( x Data..:? "instanceHealthSummary"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "instancePort")
            Prelude.<*> (x Data..:? "ipAddressType")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "publicPorts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "tlsCertificateSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "tlsPolicyName")
      )

instance Prelude.Hashable LoadBalancer where
  hashWithSalt _salt LoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` configurationOptions
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` httpsRedirectionEnabled
      `Prelude.hashWithSalt` instanceHealthSummary
      `Prelude.hashWithSalt` instancePort
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` publicPorts
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tlsCertificateSummaries
      `Prelude.hashWithSalt` tlsPolicyName

instance Prelude.NFData LoadBalancer where
  rnf LoadBalancer' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf configurationOptions
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf httpsRedirectionEnabled
      `Prelude.seq` Prelude.rnf instanceHealthSummary
      `Prelude.seq` Prelude.rnf instancePort
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf publicPorts
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        tlsCertificateSummaries
      `Prelude.seq` Prelude.rnf tlsPolicyName
