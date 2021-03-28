{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LightsailDistribution
  ( LightsailDistribution (..)
  -- * Smart constructor
  , mkLightsailDistribution
  -- * Lenses
  , ldAbleToUpdateBundle
  , ldAlternativeDomainNames
  , ldArn
  , ldBundleId
  , ldCacheBehaviorSettings
  , ldCacheBehaviors
  , ldCertificateName
  , ldCreatedAt
  , ldDefaultCacheBehavior
  , ldDomainName
  , ldIsEnabled
  , ldLocation
  , ldName
  , ldOrigin
  , ldOriginPublicDNS
  , ldResourceType
  , ldStatus
  , ldSupportCode
  , ldTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.CacheBehavior as Types
import qualified Network.AWS.Lightsail.Types.CacheBehaviorPerPath as Types
import qualified Network.AWS.Lightsail.Types.CacheSettings as Types
import qualified Network.AWS.Lightsail.Types.Origin as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Lightsail content delivery network (CDN) distribution.
--
-- /See:/ 'mkLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { ableToUpdateBundle :: Core.Maybe Core.Bool
    -- ^ Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
  , alternativeDomainNames :: Core.Maybe [Core.Text]
    -- ^ The alternate domain names of the distribution.
  , arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the distribution.
  , bundleId :: Core.Maybe Core.Text
    -- ^ The ID of the bundle currently applied to the distribution.
  , cacheBehaviorSettings :: Core.Maybe Types.CacheSettings
    -- ^ An object that describes the cache behavior settings of the distribution.
  , cacheBehaviors :: Core.Maybe [Types.CacheBehaviorPerPath]
    -- ^ An array of objects that describe the per-path cache behavior of the distribution.
  , certificateName :: Core.Maybe Types.ResourceName
    -- ^ The name of the SSL/TLS certificate attached to the distribution, if any.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the distribution was created.
  , defaultCacheBehavior :: Core.Maybe Types.CacheBehavior
    -- ^ An object that describes the default cache behavior of the distribution.
  , domainName :: Core.Maybe Core.Text
    -- ^ The domain name of the distribution.
  , isEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the distribution is enabled.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the distribution.
  , origin :: Core.Maybe Types.Origin
    -- ^ An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
  , originPublicDNS :: Core.Maybe Core.Text
    -- ^ The public DNS of the origin.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The Lightsail resource type (e.g., @Distribution@ ).
  , status :: Core.Maybe Core.Text
    -- ^ The status of the distribution.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LightsailDistribution' value with any optional fields omitted.
mkLightsailDistribution
    :: LightsailDistribution
mkLightsailDistribution
  = LightsailDistribution'{ableToUpdateBundle = Core.Nothing,
                           alternativeDomainNames = Core.Nothing, arn = Core.Nothing,
                           bundleId = Core.Nothing, cacheBehaviorSettings = Core.Nothing,
                           cacheBehaviors = Core.Nothing, certificateName = Core.Nothing,
                           createdAt = Core.Nothing, defaultCacheBehavior = Core.Nothing,
                           domainName = Core.Nothing, isEnabled = Core.Nothing,
                           location = Core.Nothing, name = Core.Nothing,
                           origin = Core.Nothing, originPublicDNS = Core.Nothing,
                           resourceType = Core.Nothing, status = Core.Nothing,
                           supportCode = Core.Nothing, tags = Core.Nothing}

-- | Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
--
-- /Note:/ Consider using 'ableToUpdateBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAbleToUpdateBundle :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
ldAbleToUpdateBundle = Lens.field @"ableToUpdateBundle"
{-# INLINEABLE ldAbleToUpdateBundle #-}
{-# DEPRECATED ableToUpdateBundle "Use generic-lens or generic-optics with 'ableToUpdateBundle' instead"  #-}

-- | The alternate domain names of the distribution.
--
-- /Note:/ Consider using 'alternativeDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAlternativeDomainNames :: Lens.Lens' LightsailDistribution (Core.Maybe [Core.Text])
ldAlternativeDomainNames = Lens.field @"alternativeDomainNames"
{-# INLINEABLE ldAlternativeDomainNames #-}
{-# DEPRECATED alternativeDomainNames "Use generic-lens or generic-optics with 'alternativeDomainNames' instead"  #-}

-- | The Amazon Resource Name (ARN) of the distribution.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldArn :: Lens.Lens' LightsailDistribution (Core.Maybe Types.Arn)
ldArn = Lens.field @"arn"
{-# INLINEABLE ldArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ID of the bundle currently applied to the distribution.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldBundleId :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
ldBundleId = Lens.field @"bundleId"
{-# INLINEABLE ldBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | An object that describes the cache behavior settings of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Core.Maybe Types.CacheSettings)
ldCacheBehaviorSettings = Lens.field @"cacheBehaviorSettings"
{-# INLINEABLE ldCacheBehaviorSettings #-}
{-# DEPRECATED cacheBehaviorSettings "Use generic-lens or generic-optics with 'cacheBehaviorSettings' instead"  #-}

-- | An array of objects that describe the per-path cache behavior of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviors :: Lens.Lens' LightsailDistribution (Core.Maybe [Types.CacheBehaviorPerPath])
ldCacheBehaviors = Lens.field @"cacheBehaviors"
{-# INLINEABLE ldCacheBehaviors #-}
{-# DEPRECATED cacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead"  #-}

-- | The name of the SSL/TLS certificate attached to the distribution, if any.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCertificateName :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceName)
ldCertificateName = Lens.field @"certificateName"
{-# INLINEABLE ldCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The timestamp when the distribution was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCreatedAt :: Lens.Lens' LightsailDistribution (Core.Maybe Core.NominalDiffTime)
ldCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE ldCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An object that describes the default cache behavior of the distribution.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDefaultCacheBehavior :: Lens.Lens' LightsailDistribution (Core.Maybe Types.CacheBehavior)
ldDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# INLINEABLE ldDefaultCacheBehavior #-}
{-# DEPRECATED defaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead"  #-}

-- | The domain name of the distribution.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDomainName :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
ldDomainName = Lens.field @"domainName"
{-# INLINEABLE ldDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Indicates whether the distribution is enabled.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIsEnabled :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
ldIsEnabled = Lens.field @"isEnabled"
{-# INLINEABLE ldIsEnabled #-}
{-# DEPRECATED isEnabled "Use generic-lens or generic-optics with 'isEnabled' instead"  #-}

-- | An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLocation :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceLocation)
ldLocation = Lens.field @"location"
{-# INLINEABLE ldLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the distribution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldName :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceName)
ldName = Lens.field @"name"
{-# INLINEABLE ldName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOrigin :: Lens.Lens' LightsailDistribution (Core.Maybe Types.Origin)
ldOrigin = Lens.field @"origin"
{-# INLINEABLE ldOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

-- | The public DNS of the origin.
--
-- /Note:/ Consider using 'originPublicDNS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOriginPublicDNS :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
ldOriginPublicDNS = Lens.field @"originPublicDNS"
{-# INLINEABLE ldOriginPublicDNS #-}
{-# DEPRECATED originPublicDNS "Use generic-lens or generic-optics with 'originPublicDNS' instead"  #-}

-- | The Lightsail resource type (e.g., @Distribution@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldResourceType :: Lens.Lens' LightsailDistribution (Core.Maybe Types.ResourceType)
ldResourceType = Lens.field @"resourceType"
{-# INLINEABLE ldResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The status of the distribution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStatus :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
ldStatus = Lens.field @"status"
{-# INLINEABLE ldStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldSupportCode :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
ldSupportCode = Lens.field @"supportCode"
{-# INLINEABLE ldSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldTags :: Lens.Lens' LightsailDistribution (Core.Maybe [Types.Tag])
ldTags = Lens.field @"tags"
{-# INLINEABLE ldTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON LightsailDistribution where
        parseJSON
          = Core.withObject "LightsailDistribution" Core.$
              \ x ->
                LightsailDistribution' Core.<$>
                  (x Core..:? "ableToUpdateBundle") Core.<*>
                    x Core..:? "alternativeDomainNames"
                    Core.<*> x Core..:? "arn"
                    Core.<*> x Core..:? "bundleId"
                    Core.<*> x Core..:? "cacheBehaviorSettings"
                    Core.<*> x Core..:? "cacheBehaviors"
                    Core.<*> x Core..:? "certificateName"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "defaultCacheBehavior"
                    Core.<*> x Core..:? "domainName"
                    Core.<*> x Core..:? "isEnabled"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "origin"
                    Core.<*> x Core..:? "originPublicDNS"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
