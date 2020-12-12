{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancer
  ( LoadBalancer (..),

    -- * Smart constructor
    mkLoadBalancer,

    -- * Lenses
    lbHealthCheckPath,
    lbState,
    lbResourceType,
    lbArn,
    lbCreatedAt,
    lbLocation,
    lbInstancePort,
    lbConfigurationOptions,
    lbProtocol,
    lbTlsCertificateSummaries,
    lbName,
    lbSupportCode,
    lbPublicPorts,
    lbDnsName,
    lbInstanceHealthSummary,
    lbTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes the Lightsail load balancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { healthCheckPath ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe LoadBalancerState,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    instancePort :: Lude.Maybe Lude.Int,
    configurationOptions ::
      Lude.Maybe (Lude.HashMap LoadBalancerAttributeName (Lude.Text)),
    protocol :: Lude.Maybe LoadBalancerProtocol,
    tlsCertificateSummaries ::
      Lude.Maybe [LoadBalancerTLSCertificateSummary],
    name :: Lude.Maybe Lude.Text,
    supportCode :: Lude.Maybe Lude.Text,
    publicPorts :: Lude.Maybe [Lude.Int],
    dnsName :: Lude.Maybe Lude.Text,
    instanceHealthSummary :: Lude.Maybe [InstanceHealthSummary],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'configurationOptions' - A string to string map of the configuration options for your load balancer. Valid values are listed below.
-- * 'createdAt' - The date when your load balancer was created.
-- * 'dnsName' - The DNS name of your Lightsail load balancer.
-- * 'healthCheckPath' - The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
-- * 'instanceHealthSummary' - An array of InstanceHealthSummary objects describing the health of the load balancer.
-- * 'instancePort' - The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
-- * 'location' - The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
-- * 'name' - The name of the load balancer (e.g., @my-load-balancer@ ).
-- * 'protocol' - The protocol you have enabled for your load balancer. Valid values are below.
--
-- You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
-- * 'publicPorts' - An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
-- * 'resourceType' - The resource type (e.g., @LoadBalancer@ .
-- * 'state' - The status of your load balancer. Valid values are below.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
-- * 'tlsCertificateSummaries' - An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
mkLoadBalancer ::
  LoadBalancer
mkLoadBalancer =
  LoadBalancer'
    { healthCheckPath = Lude.Nothing,
      state = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      instancePort = Lude.Nothing,
      configurationOptions = Lude.Nothing,
      protocol = Lude.Nothing,
      tlsCertificateSummaries = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      publicPorts = Lude.Nothing,
      dnsName = Lude.Nothing,
      instanceHealthSummary = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbHealthCheckPath :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbHealthCheckPath = Lens.lens (healthCheckPath :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPath = a} :: LoadBalancer)
{-# DEPRECATED lbHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | The status of your load balancer. Valid values are below.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbState :: Lens.Lens' LoadBalancer (Lude.Maybe LoadBalancerState)
lbState = Lens.lens (state :: LoadBalancer -> Lude.Maybe LoadBalancerState) (\s a -> s {state = a} :: LoadBalancer)
{-# DEPRECATED lbState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The resource type (e.g., @LoadBalancer@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbResourceType :: Lens.Lens' LoadBalancer (Lude.Maybe ResourceType)
lbResourceType = Lens.lens (resourceType :: LoadBalancer -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: LoadBalancer)
{-# DEPRECATED lbResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbArn :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbArn = Lens.lens (arn :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LoadBalancer)
{-# DEPRECATED lbArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when your load balancer was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCreatedAt :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Timestamp)
lbCreatedAt = Lens.lens (createdAt :: LoadBalancer -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: LoadBalancer)
{-# DEPRECATED lbCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLocation :: Lens.Lens' LoadBalancer (Lude.Maybe ResourceLocation)
lbLocation = Lens.lens (location :: LoadBalancer -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: LoadBalancer)
{-# DEPRECATED lbLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbInstancePort :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Int)
lbInstancePort = Lens.lens (instancePort :: LoadBalancer -> Lude.Maybe Lude.Int) (\s a -> s {instancePort = a} :: LoadBalancer)
{-# DEPRECATED lbInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

-- | A string to string map of the configuration options for your load balancer. Valid values are listed below.
--
-- /Note:/ Consider using 'configurationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbConfigurationOptions :: Lens.Lens' LoadBalancer (Lude.Maybe (Lude.HashMap LoadBalancerAttributeName (Lude.Text)))
lbConfigurationOptions = Lens.lens (configurationOptions :: LoadBalancer -> Lude.Maybe (Lude.HashMap LoadBalancerAttributeName (Lude.Text))) (\s a -> s {configurationOptions = a} :: LoadBalancer)
{-# DEPRECATED lbConfigurationOptions "Use generic-lens or generic-optics with 'configurationOptions' instead." #-}

-- | The protocol you have enabled for your load balancer. Valid values are below.
--
-- You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbProtocol :: Lens.Lens' LoadBalancer (Lude.Maybe LoadBalancerProtocol)
lbProtocol = Lens.lens (protocol :: LoadBalancer -> Lude.Maybe LoadBalancerProtocol) (\s a -> s {protocol = a} :: LoadBalancer)
{-# DEPRECATED lbProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
--
-- /Note:/ Consider using 'tlsCertificateSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTlsCertificateSummaries :: Lens.Lens' LoadBalancer (Lude.Maybe [LoadBalancerTLSCertificateSummary])
lbTlsCertificateSummaries = Lens.lens (tlsCertificateSummaries :: LoadBalancer -> Lude.Maybe [LoadBalancerTLSCertificateSummary]) (\s a -> s {tlsCertificateSummaries = a} :: LoadBalancer)
{-# DEPRECATED lbTlsCertificateSummaries "Use generic-lens or generic-optics with 'tlsCertificateSummaries' instead." #-}

-- | The name of the load balancer (e.g., @my-load-balancer@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbName = Lens.lens (name :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LoadBalancer)
{-# DEPRECATED lbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbSupportCode :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbSupportCode = Lens.lens (supportCode :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: LoadBalancer)
{-# DEPRECATED lbSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
--
-- /Note:/ Consider using 'publicPorts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbPublicPorts :: Lens.Lens' LoadBalancer (Lude.Maybe [Lude.Int])
lbPublicPorts = Lens.lens (publicPorts :: LoadBalancer -> Lude.Maybe [Lude.Int]) (\s a -> s {publicPorts = a} :: LoadBalancer)
{-# DEPRECATED lbPublicPorts "Use generic-lens or generic-optics with 'publicPorts' instead." #-}

-- | The DNS name of your Lightsail load balancer.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbDnsName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbDnsName = Lens.lens (dnsName :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: LoadBalancer)
{-# DEPRECATED lbDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | An array of InstanceHealthSummary objects describing the health of the load balancer.
--
-- /Note:/ Consider using 'instanceHealthSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbInstanceHealthSummary :: Lens.Lens' LoadBalancer (Lude.Maybe [InstanceHealthSummary])
lbInstanceHealthSummary = Lens.lens (instanceHealthSummary :: LoadBalancer -> Lude.Maybe [InstanceHealthSummary]) (\s a -> s {instanceHealthSummary = a} :: LoadBalancer)
{-# DEPRECATED lbInstanceHealthSummary "Use generic-lens or generic-optics with 'instanceHealthSummary' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTags :: Lens.Lens' LoadBalancer (Lude.Maybe [Tag])
lbTags = Lens.lens (tags :: LoadBalancer -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LoadBalancer)
{-# DEPRECATED lbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON LoadBalancer where
  parseJSON =
    Lude.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Lude.<$> (x Lude..:? "healthCheckPath")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "instancePort")
            Lude.<*> (x Lude..:? "configurationOptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "tlsCertificateSummaries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "publicPorts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "dnsName")
            Lude.<*> (x Lude..:? "instanceHealthSummary" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
