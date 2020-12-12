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
    ldStatus,
    ldOrigin,
    ldCertificateName,
    ldResourceType,
    ldArn,
    ldCreatedAt,
    ldLocation,
    ldCacheBehaviorSettings,
    ldAlternativeDomainNames,
    ldBundleId,
    ldAbleToUpdateBundle,
    ldOriginPublicDNS,
    ldDomainName,
    ldName,
    ldIsEnabled,
    ldSupportCode,
    ldDefaultCacheBehavior,
    ldCacheBehaviors,
    ldTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon Lightsail content delivery network (CDN) distribution.
--
-- /See:/ 'mkLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { status ::
      Lude.Maybe Lude.Text,
    origin :: Lude.Maybe Origin,
    certificateName :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    cacheBehaviorSettings ::
      Lude.Maybe CacheSettings,
    alternativeDomainNames ::
      Lude.Maybe [Lude.Text],
    bundleId :: Lude.Maybe Lude.Text,
    ableToUpdateBundle :: Lude.Maybe Lude.Bool,
    originPublicDNS :: Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    isEnabled :: Lude.Maybe Lude.Bool,
    supportCode :: Lude.Maybe Lude.Text,
    defaultCacheBehavior ::
      Lude.Maybe CacheBehavior,
    cacheBehaviors ::
      Lude.Maybe [CacheBehaviorPerPath],
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

-- | Creates a value of 'LightsailDistribution' with the minimum fields required to make a request.
--
-- * 'ableToUpdateBundle' - Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
-- * 'alternativeDomainNames' - The alternate domain names of the distribution.
-- * 'arn' - The Amazon Resource Name (ARN) of the distribution.
-- * 'bundleId' - The ID of the bundle currently applied to the distribution.
-- * 'cacheBehaviorSettings' - An object that describes the cache behavior settings of the distribution.
-- * 'cacheBehaviors' - An array of objects that describe the per-path cache behavior of the distribution.
-- * 'certificateName' - The name of the SSL/TLS certificate attached to the distribution, if any.
-- * 'createdAt' - The timestamp when the distribution was created.
-- * 'defaultCacheBehavior' - An object that describes the default cache behavior of the distribution.
-- * 'domainName' - The domain name of the distribution.
-- * 'isEnabled' - Indicates whether the distribution is enabled.
-- * 'location' - An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
-- * 'name' - The name of the distribution.
-- * 'origin' - An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
-- * 'originPublicDNS' - The public DNS of the origin.
-- * 'resourceType' - The Lightsail resource type (e.g., @Distribution@ ).
-- * 'status' - The status of the distribution.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkLightsailDistribution ::
  LightsailDistribution
mkLightsailDistribution =
  LightsailDistribution'
    { status = Lude.Nothing,
      origin = Lude.Nothing,
      certificateName = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      cacheBehaviorSettings = Lude.Nothing,
      alternativeDomainNames = Lude.Nothing,
      bundleId = Lude.Nothing,
      ableToUpdateBundle = Lude.Nothing,
      originPublicDNS = Lude.Nothing,
      domainName = Lude.Nothing,
      name = Lude.Nothing,
      isEnabled = Lude.Nothing,
      supportCode = Lude.Nothing,
      defaultCacheBehavior = Lude.Nothing,
      cacheBehaviors = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status of the distribution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStatus :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldStatus = Lens.lens (status :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: LightsailDistribution)
{-# DEPRECATED ldStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOrigin :: Lens.Lens' LightsailDistribution (Lude.Maybe Origin)
ldOrigin = Lens.lens (origin :: LightsailDistribution -> Lude.Maybe Origin) (\s a -> s {origin = a} :: LightsailDistribution)
{-# DEPRECATED ldOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The name of the SSL/TLS certificate attached to the distribution, if any.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCertificateName :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldCertificateName = Lens.lens (certificateName :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: LightsailDistribution)
{-# DEPRECATED ldCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The Lightsail resource type (e.g., @Distribution@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldResourceType :: Lens.Lens' LightsailDistribution (Lude.Maybe ResourceType)
ldResourceType = Lens.lens (resourceType :: LightsailDistribution -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: LightsailDistribution)
{-# DEPRECATED ldResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the distribution.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldArn :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldArn = Lens.lens (arn :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LightsailDistribution)
{-# DEPRECATED ldArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the distribution was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCreatedAt :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Timestamp)
ldCreatedAt = Lens.lens (createdAt :: LightsailDistribution -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: LightsailDistribution)
{-# DEPRECATED ldCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLocation :: Lens.Lens' LightsailDistribution (Lude.Maybe ResourceLocation)
ldLocation = Lens.lens (location :: LightsailDistribution -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: LightsailDistribution)
{-# DEPRECATED ldLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | An object that describes the cache behavior settings of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Lude.Maybe CacheSettings)
ldCacheBehaviorSettings = Lens.lens (cacheBehaviorSettings :: LightsailDistribution -> Lude.Maybe CacheSettings) (\s a -> s {cacheBehaviorSettings = a} :: LightsailDistribution)
{-# DEPRECATED ldCacheBehaviorSettings "Use generic-lens or generic-optics with 'cacheBehaviorSettings' instead." #-}

-- | The alternate domain names of the distribution.
--
-- /Note:/ Consider using 'alternativeDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAlternativeDomainNames :: Lens.Lens' LightsailDistribution (Lude.Maybe [Lude.Text])
ldAlternativeDomainNames = Lens.lens (alternativeDomainNames :: LightsailDistribution -> Lude.Maybe [Lude.Text]) (\s a -> s {alternativeDomainNames = a} :: LightsailDistribution)
{-# DEPRECATED ldAlternativeDomainNames "Use generic-lens or generic-optics with 'alternativeDomainNames' instead." #-}

-- | The ID of the bundle currently applied to the distribution.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldBundleId :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldBundleId = Lens.lens (bundleId :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: LightsailDistribution)
{-# DEPRECATED ldBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
--
-- /Note:/ Consider using 'ableToUpdateBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAbleToUpdateBundle :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Bool)
ldAbleToUpdateBundle = Lens.lens (ableToUpdateBundle :: LightsailDistribution -> Lude.Maybe Lude.Bool) (\s a -> s {ableToUpdateBundle = a} :: LightsailDistribution)
{-# DEPRECATED ldAbleToUpdateBundle "Use generic-lens or generic-optics with 'ableToUpdateBundle' instead." #-}

-- | The public DNS of the origin.
--
-- /Note:/ Consider using 'originPublicDNS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldOriginPublicDNS :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldOriginPublicDNS = Lens.lens (originPublicDNS :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {originPublicDNS = a} :: LightsailDistribution)
{-# DEPRECATED ldOriginPublicDNS "Use generic-lens or generic-optics with 'originPublicDNS' instead." #-}

-- | The domain name of the distribution.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDomainName :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldDomainName = Lens.lens (domainName :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: LightsailDistribution)
{-# DEPRECATED ldDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the distribution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldName :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldName = Lens.lens (name :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LightsailDistribution)
{-# DEPRECATED ldName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates whether the distribution is enabled.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIsEnabled :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Bool)
ldIsEnabled = Lens.lens (isEnabled :: LightsailDistribution -> Lude.Maybe Lude.Bool) (\s a -> s {isEnabled = a} :: LightsailDistribution)
{-# DEPRECATED ldIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldSupportCode :: Lens.Lens' LightsailDistribution (Lude.Maybe Lude.Text)
ldSupportCode = Lens.lens (supportCode :: LightsailDistribution -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: LightsailDistribution)
{-# DEPRECATED ldSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | An object that describes the default cache behavior of the distribution.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDefaultCacheBehavior :: Lens.Lens' LightsailDistribution (Lude.Maybe CacheBehavior)
ldDefaultCacheBehavior = Lens.lens (defaultCacheBehavior :: LightsailDistribution -> Lude.Maybe CacheBehavior) (\s a -> s {defaultCacheBehavior = a} :: LightsailDistribution)
{-# DEPRECATED ldDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | An array of objects that describe the per-path cache behavior of the distribution.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCacheBehaviors :: Lens.Lens' LightsailDistribution (Lude.Maybe [CacheBehaviorPerPath])
ldCacheBehaviors = Lens.lens (cacheBehaviors :: LightsailDistribution -> Lude.Maybe [CacheBehaviorPerPath]) (\s a -> s {cacheBehaviors = a} :: LightsailDistribution)
{-# DEPRECATED ldCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldTags :: Lens.Lens' LightsailDistribution (Lude.Maybe [Tag])
ldTags = Lens.lens (tags :: LightsailDistribution -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LightsailDistribution)
{-# DEPRECATED ldTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON LightsailDistribution where
  parseJSON =
    Lude.withObject
      "LightsailDistribution"
      ( \x ->
          LightsailDistribution'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "origin")
            Lude.<*> (x Lude..:? "certificateName")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "cacheBehaviorSettings")
            Lude.<*> (x Lude..:? "alternativeDomainNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "ableToUpdateBundle")
            Lude.<*> (x Lude..:? "originPublicDNS")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "isEnabled")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "defaultCacheBehavior")
            Lude.<*> (x Lude..:? "cacheBehaviors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
