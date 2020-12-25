{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LightsailDistribution
  ( LightsailDistribution (..),

    -- * Smart constructor
    mkLightsailDistribution,

    -- * Lenses
    ldAbleToUpdateBundle,
    ldAlternativeDomainNames,
    ldArn,
    ldBundleId,
    ldCacheBehaviorSettings,
    ldCacheBehaviors,
    ldCertificateName,
    ldCreatedAt,
    ldDefaultCacheBehavior,
    ldDomainName,
    ldIsEnabled,
    ldLocation,
    ldName,
    ldOrigin,
    ldOriginPublicDNS,
    ldResourceType,
    ldStatus,
    ldSupportCode,
    ldTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.CacheBehavior as Types
import qualified Network.AWS.Lightsail.Types.CacheBehaviorPerPath as Types
import qualified Network.AWS.Lightsail.Types.CacheSettings as Types
import qualified Network.AWS.Lightsail.Types.Origin as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Lightsail content delivery network (CDN) distribution.
--
-- /See:/ 'mkLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { -- | Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
    --
    -- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
    ableToUpdateBundle :: Core.Maybe Core.Bool,
    -- | The alternate domain names of the distribution.
    alternativeDomainNames :: Core.Maybe [Types.String],
    -- | The Amazon Resource Name (ARN) of the distribution.
    arn :: Core.Maybe Types.Arn,
    -- | The ID of the bundle currently applied to the distribution.
    bundleId :: Core.Maybe Types.String,
    -- | An object that describes the cache behavior settings of the distribution.
    cacheBehaviorSettings :: Core.Maybe Types.CacheSettings,
    -- | An array of objects that describe the per-path cache behavior of the distribution.
    cacheBehaviors :: Core.Maybe [Types.CacheBehaviorPerPath],
    -- | The name of the SSL/TLS certificate attached to the distribution, if any.
    certificateName :: Core.Maybe Types.ResourceName,
    -- | The timestamp when the distribution was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | An object that describes the default cache behavior of the distribution.
    defaultCacheBehavior :: Core.Maybe Types.CacheBehavior,
    -- | The domain name of the distribution.
    domainName :: Core.Maybe Types.String,
    -- | Indicates whether the distribution is enabled.
    isEnabled :: Core.Maybe Core.Bool,
    -- | An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The name of the distribution.
    name :: Core.Maybe Types.ResourceName,
    -- | An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Core.Maybe Types.Origin,
    -- | The public DNS of the origin.
    originPublicDNS :: Core.Maybe Types.String,
    -- | The Lightsail resource type (e.g., @Distribution@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The status of the distribution.
    status :: Core.Maybe Types.String,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LightsailDistribution' value with any optional fields omitted.
mkLightsailDistribution ::
  LightsailDistribution
mkLightsailDistribution =
  LightsailDistribution'
    { ableToUpdateBundle = Core.Nothing,
      alternativeDomainNames = Core.Nothing,
      arn = Core.Nothing,
      bundleId = Core.Nothing,
      cacheBehaviorSettings = Core.Nothing,
      cacheBehaviors = Core.Nothing,
      certificateName = Core.Nothing,
      createdAt = Core.Nothing,
      defaultCacheBehavior = Core.Nothing,
      domainName = Core.Nothing,
      isEnabled = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      origin = Core.Nothing,
      originPublicDNS = Core.Nothing,
      resourceType = Core.Nothing,
      status = Core.Nothing,
      supportCode = Core.Nothing,
      tags = Core.Nothing
    }

-- | Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
--
-- /Note:/ Consider using 'ableToUpdateBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAbleToUpdateBundle :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
ldAbleToUpdateBundle = Lens.field @"ableToUpdateBundle"
{-# DEPRECATED ldAbleToUpdateBundle "Use generic-lens or generic-optics with 'ableToUpdateBundle' instead." #-}

-- | The alternate domain names of the distribution.
--
-- /Note:/ Consider using 'alternativeDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAlternativeDomainNames :: Lens.Lens' LightsailDistribution (Core.Maybe [Types.String])
ldAlternativeDomainNames = Lens.field @"alternativeDomainNames"
{-# DEPRECATED ldAlternativeDomainNames "Use generic-lens or generic-optics with 'alternativeDomainNames' instead." #-}

-- | The Amazon Resource Name (ARN) of the distribution.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldArn :: Lens.Lens' LightsailDistribution (Core.Maybe Types.Arn)
ldArn = Lens.field @"arn"
{-# DEPRECATED ldArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the bundle currently applied to the distribution.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldBundleId :: Lens.Lens' LightsailDistribution (Core.Maybe Types.String)
ldBundleId = Lens.field @"bundleId"
{-# DEPRECATED ldBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | An object that describes the cache behavior settings of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Core.Maybe Types.CacheSettings)
ldCacheBehaviorSettings = Lens.field @"cacheBehaviorSettings"
{-# DEPRECATED ldCacheBehaviorSettings "Use generic-lens or generic-optics with 'cacheBehaviorSettings' instead." #-}

-- | An array of objects that describe the per-path cache behavior of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviors :: Lens.Lens' LightsailDistribution (Core.Maybe [Types.CacheBehaviorPerPath])
ldCacheBehaviors = Lens.field @"cacheBehaviors"
{-# DEPRECATED ldCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

-- | The name of the SSL/TLS certificate attached to the distribution, if any.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCertificateName :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceName)
ldCertificateName = Lens.field @"certificateName"
{-# DEPRECATED ldCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The timestamp when the distribution was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCreatedAt :: Lens.Lens' LightsailDistribution (Core.Maybe Core.NominalDiffTime)
ldCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED ldCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes the default cache behavior of the distribution.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDefaultCacheBehavior :: Lens.Lens' LightsailDistribution (Core.Maybe Types.CacheBehavior)
ldDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# DEPRECATED ldDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | The domain name of the distribution.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDomainName :: Lens.Lens' LightsailDistribution (Core.Maybe Types.String)
ldDomainName = Lens.field @"domainName"
{-# DEPRECATED ldDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Indicates whether the distribution is enabled.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIsEnabled :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
ldIsEnabled = Lens.field @"isEnabled"
{-# DEPRECATED ldIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLocation :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceLocation)
ldLocation = Lens.field @"location"
{-# DEPRECATED ldLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the distribution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldName :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceName)
ldName = Lens.field @"name"
{-# DEPRECATED ldName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOrigin :: Lens.Lens' LightsailDistribution (Core.Maybe Types.Origin)
ldOrigin = Lens.field @"origin"
{-# DEPRECATED ldOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The public DNS of the origin.
--
-- /Note:/ Consider using 'originPublicDNS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOriginPublicDNS :: Lens.Lens' LightsailDistribution (Core.Maybe Types.String)
ldOriginPublicDNS = Lens.field @"originPublicDNS"
{-# DEPRECATED ldOriginPublicDNS "Use generic-lens or generic-optics with 'originPublicDNS' instead." #-}

-- | The Lightsail resource type (e.g., @Distribution@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldResourceType :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceType)
ldResourceType = Lens.field @"resourceType"
{-# DEPRECATED ldResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The status of the distribution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStatus :: Lens.Lens' LightsailDistribution (Core.Maybe Types.String)
ldStatus = Lens.field @"status"
{-# DEPRECATED ldStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldSupportCode :: Lens.Lens' LightsailDistribution (Core.Maybe Types.String)
ldSupportCode = Lens.field @"supportCode"
{-# DEPRECATED ldSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldTags :: Lens.Lens' LightsailDistribution (Core.Maybe [Types.Tag])
ldTags = Lens.field @"tags"
{-# DEPRECATED ldTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON LightsailDistribution where
  parseJSON =
    Core.withObject "LightsailDistribution" Core.$
      \x ->
        LightsailDistribution'
          Core.<$> (x Core..:? "ableToUpdateBundle")
          Core.<*> (x Core..:? "alternativeDomainNames")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "bundleId")
          Core.<*> (x Core..:? "cacheBehaviorSettings")
          Core.<*> (x Core..:? "cacheBehaviors")
          Core.<*> (x Core..:? "certificateName")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "defaultCacheBehavior")
          Core.<*> (x Core..:? "domainName")
          Core.<*> (x Core..:? "isEnabled")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "origin")
          Core.<*> (x Core..:? "originPublicDNS")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "tags")
