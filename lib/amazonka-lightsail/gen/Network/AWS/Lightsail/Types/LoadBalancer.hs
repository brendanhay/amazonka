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
    lbArn,
    lbConfigurationOptions,
    lbCreatedAt,
    lbDnsName,
    lbHealthCheckPath,
    lbInstanceHealthSummary,
    lbInstancePort,
    lbLocation,
    lbName,
    lbProtocol,
    lbPublicPorts,
    lbResourceType,
    lbState,
    lbSupportCode,
    lbTags,
    lbTlsCertificateSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.InstanceHealthSummary as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerAttributeName as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerProtocol as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerState as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the Lightsail load balancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | A string to string map of the configuration options for your load balancer. Valid values are listed below.
    configurationOptions :: Core.Maybe (Core.HashMap Types.LoadBalancerAttributeName Types.String),
    -- | The date when your load balancer was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The DNS name of your Lightsail load balancer.
    dnsName :: Core.Maybe Types.NonEmptyString,
    -- | The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
    healthCheckPath :: Core.Maybe Types.NonEmptyString,
    -- | An array of InstanceHealthSummary objects describing the health of the load balancer.
    instanceHealthSummary :: Core.Maybe [Types.InstanceHealthSummary],
    -- | The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
    instancePort :: Core.Maybe Core.Int,
    -- | The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The name of the load balancer (e.g., @my-load-balancer@ ).
    name :: Core.Maybe Types.ResourceName,
    -- | The protocol you have enabled for your load balancer. Valid values are below.
    --
    -- You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
    protocol :: Core.Maybe Types.LoadBalancerProtocol,
    -- | An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
    publicPorts :: Core.Maybe [Core.Int],
    -- | The resource type (e.g., @LoadBalancer@ .
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The status of your load balancer. Valid values are below.
    state :: Core.Maybe Types.LoadBalancerState,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag],
    -- | An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
    tlsCertificateSummaries :: Core.Maybe [Types.LoadBalancerTlsCertificateSummary]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LoadBalancer' value with any optional fields omitted.
mkLoadBalancer ::
  LoadBalancer
mkLoadBalancer =
  LoadBalancer'
    { arn = Core.Nothing,
      configurationOptions = Core.Nothing,
      createdAt = Core.Nothing,
      dnsName = Core.Nothing,
      healthCheckPath = Core.Nothing,
      instanceHealthSummary = Core.Nothing,
      instancePort = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      protocol = Core.Nothing,
      publicPorts = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      supportCode = Core.Nothing,
      tags = Core.Nothing,
      tlsCertificateSummaries = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbArn :: Lens.Lens' LoadBalancer (Core.Maybe Types.NonEmptyString)
lbArn = Lens.field @"arn"
{-# DEPRECATED lbArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A string to string map of the configuration options for your load balancer. Valid values are listed below.
--
-- /Note:/ Consider using 'configurationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbConfigurationOptions :: Lens.Lens' LoadBalancer (Core.Maybe (Core.HashMap Types.LoadBalancerAttributeName Types.String))
lbConfigurationOptions = Lens.field @"configurationOptions"
{-# DEPRECATED lbConfigurationOptions "Use generic-lens or generic-optics with 'configurationOptions' instead." #-}

-- | The date when your load balancer was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCreatedAt :: Lens.Lens' LoadBalancer (Core.Maybe Core.NominalDiffTime)
lbCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED lbCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The DNS name of your Lightsail load balancer.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbDnsName :: Lens.Lens' LoadBalancer (Core.Maybe Types.NonEmptyString)
lbDnsName = Lens.field @"dnsName"
{-# DEPRECATED lbDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbHealthCheckPath :: Lens.Lens' LoadBalancer (Core.Maybe Types.NonEmptyString)
lbHealthCheckPath = Lens.field @"healthCheckPath"
{-# DEPRECATED lbHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | An array of InstanceHealthSummary objects describing the health of the load balancer.
--
-- /Note:/ Consider using 'instanceHealthSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbInstanceHealthSummary :: Lens.Lens' LoadBalancer (Core.Maybe [Types.InstanceHealthSummary])
lbInstanceHealthSummary = Lens.field @"instanceHealthSummary"
{-# DEPRECATED lbInstanceHealthSummary "Use generic-lens or generic-optics with 'instanceHealthSummary' instead." #-}

-- | The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbInstancePort :: Lens.Lens' LoadBalancer (Core.Maybe Core.Int)
lbInstancePort = Lens.field @"instancePort"
{-# DEPRECATED lbInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

-- | The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLocation :: Lens.Lens' LoadBalancer (Core.Maybe Types.ResourceLocation)
lbLocation = Lens.field @"location"
{-# DEPRECATED lbLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the load balancer (e.g., @my-load-balancer@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LoadBalancer (Core.Maybe Types.ResourceName)
lbName = Lens.field @"name"
{-# DEPRECATED lbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol you have enabled for your load balancer. Valid values are below.
--
-- You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbProtocol :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerProtocol)
lbProtocol = Lens.field @"protocol"
{-# DEPRECATED lbProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
--
-- /Note:/ Consider using 'publicPorts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbPublicPorts :: Lens.Lens' LoadBalancer (Core.Maybe [Core.Int])
lbPublicPorts = Lens.field @"publicPorts"
{-# DEPRECATED lbPublicPorts "Use generic-lens or generic-optics with 'publicPorts' instead." #-}

-- | The resource type (e.g., @LoadBalancer@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbResourceType :: Lens.Lens' LoadBalancer (Core.Maybe Types.ResourceType)
lbResourceType = Lens.field @"resourceType"
{-# DEPRECATED lbResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The status of your load balancer. Valid values are below.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbState :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerState)
lbState = Lens.field @"state"
{-# DEPRECATED lbState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbSupportCode :: Lens.Lens' LoadBalancer (Core.Maybe Types.String)
lbSupportCode = Lens.field @"supportCode"
{-# DEPRECATED lbSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTags :: Lens.Lens' LoadBalancer (Core.Maybe [Types.Tag])
lbTags = Lens.field @"tags"
{-# DEPRECATED lbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
--
-- /Note:/ Consider using 'tlsCertificateSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTlsCertificateSummaries :: Lens.Lens' LoadBalancer (Core.Maybe [Types.LoadBalancerTlsCertificateSummary])
lbTlsCertificateSummaries = Lens.field @"tlsCertificateSummaries"
{-# DEPRECATED lbTlsCertificateSummaries "Use generic-lens or generic-optics with 'tlsCertificateSummaries' instead." #-}

instance Core.FromJSON LoadBalancer where
  parseJSON =
    Core.withObject "LoadBalancer" Core.$
      \x ->
        LoadBalancer'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "configurationOptions")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "dnsName")
          Core.<*> (x Core..:? "healthCheckPath")
          Core.<*> (x Core..:? "instanceHealthSummary")
          Core.<*> (x Core..:? "instancePort")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "protocol")
          Core.<*> (x Core..:? "publicPorts")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "tlsCertificateSummaries")
