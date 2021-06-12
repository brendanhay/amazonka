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
-- Module      : Network.AWS.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LightsailDistribution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.IpAddressType
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes an Amazon Lightsail content delivery network (CDN)
-- distribution.
--
-- /See:/ 'newLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { -- | Indicates whether the distribution is enabled.
    isEnabled :: Core.Maybe Core.Bool,
    -- | The IP address type of the distribution.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | An object that describes the origin resource of the distribution, such
    -- as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Core.Maybe Origin,
    -- | The status of the distribution.
    status :: Core.Maybe Core.Text,
    -- | The public DNS of the origin.
    originPublicDNS :: Core.Maybe Core.Text,
    -- | The ID of the bundle currently applied to the distribution.
    bundleId :: Core.Maybe Core.Text,
    -- | The alternate domain names of the distribution.
    alternativeDomainNames :: Core.Maybe [Core.Text],
    -- | The timestamp when the distribution was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | An object that describes the cache behavior settings of the
    -- distribution.
    cacheBehaviorSettings :: Core.Maybe CacheSettings,
    -- | The Amazon Resource Name (ARN) of the distribution.
    arn :: Core.Maybe Core.Text,
    -- | The Lightsail resource type (e.g., @Distribution@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail distribution. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The name of the distribution.
    name :: Core.Maybe Core.Text,
    -- | The domain name of the distribution.
    domainName :: Core.Maybe Core.Text,
    -- | Indicates whether the bundle that is currently applied to your
    -- distribution, specified using the @distributionName@ parameter, can be
    -- changed to another bundle.
    --
    -- Use the @UpdateDistributionBundle@ action to change your distribution\'s
    -- bundle.
    ableToUpdateBundle :: Core.Maybe Core.Bool,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | An array of objects that describe the per-path cache behavior of the
    -- distribution.
    cacheBehaviors :: Core.Maybe [CacheBehaviorPerPath],
    -- | An object that describes the default cache behavior of the distribution.
    defaultCacheBehavior :: Core.Maybe CacheBehavior,
    -- | An object that describes the location of the distribution, such as the
    -- AWS Region and Availability Zone.
    --
    -- Lightsail distributions are global resources that can reference an
    -- origin in any AWS Region, and distribute its content globally. However,
    -- all distributions are located in the @us-east-1@ Region.
    location :: Core.Maybe ResourceLocation,
    -- | The name of the SSL\/TLS certificate attached to the distribution, if
    -- any.
    certificateName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LightsailDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isEnabled', 'lightsailDistribution_isEnabled' - Indicates whether the distribution is enabled.
--
-- 'ipAddressType', 'lightsailDistribution_ipAddressType' - The IP address type of the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'origin', 'lightsailDistribution_origin' - An object that describes the origin resource of the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- 'status', 'lightsailDistribution_status' - The status of the distribution.
--
-- 'originPublicDNS', 'lightsailDistribution_originPublicDNS' - The public DNS of the origin.
--
-- 'bundleId', 'lightsailDistribution_bundleId' - The ID of the bundle currently applied to the distribution.
--
-- 'alternativeDomainNames', 'lightsailDistribution_alternativeDomainNames' - The alternate domain names of the distribution.
--
-- 'createdAt', 'lightsailDistribution_createdAt' - The timestamp when the distribution was created.
--
-- 'cacheBehaviorSettings', 'lightsailDistribution_cacheBehaviorSettings' - An object that describes the cache behavior settings of the
-- distribution.
--
-- 'arn', 'lightsailDistribution_arn' - The Amazon Resource Name (ARN) of the distribution.
--
-- 'resourceType', 'lightsailDistribution_resourceType' - The Lightsail resource type (e.g., @Distribution@).
--
-- 'supportCode', 'lightsailDistribution_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail distribution. This code enables our
-- support team to look up your Lightsail information more easily.
--
-- 'name', 'lightsailDistribution_name' - The name of the distribution.
--
-- 'domainName', 'lightsailDistribution_domainName' - The domain name of the distribution.
--
-- 'ableToUpdateBundle', 'lightsailDistribution_ableToUpdateBundle' - Indicates whether the bundle that is currently applied to your
-- distribution, specified using the @distributionName@ parameter, can be
-- changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution\'s
-- bundle.
--
-- 'tags', 'lightsailDistribution_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'cacheBehaviors', 'lightsailDistribution_cacheBehaviors' - An array of objects that describe the per-path cache behavior of the
-- distribution.
--
-- 'defaultCacheBehavior', 'lightsailDistribution_defaultCacheBehavior' - An object that describes the default cache behavior of the distribution.
--
-- 'location', 'lightsailDistribution_location' - An object that describes the location of the distribution, such as the
-- AWS Region and Availability Zone.
--
-- Lightsail distributions are global resources that can reference an
-- origin in any AWS Region, and distribute its content globally. However,
-- all distributions are located in the @us-east-1@ Region.
--
-- 'certificateName', 'lightsailDistribution_certificateName' - The name of the SSL\/TLS certificate attached to the distribution, if
-- any.
newLightsailDistribution ::
  LightsailDistribution
newLightsailDistribution =
  LightsailDistribution'
    { isEnabled = Core.Nothing,
      ipAddressType = Core.Nothing,
      origin = Core.Nothing,
      status = Core.Nothing,
      originPublicDNS = Core.Nothing,
      bundleId = Core.Nothing,
      alternativeDomainNames = Core.Nothing,
      createdAt = Core.Nothing,
      cacheBehaviorSettings = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      name = Core.Nothing,
      domainName = Core.Nothing,
      ableToUpdateBundle = Core.Nothing,
      tags = Core.Nothing,
      cacheBehaviors = Core.Nothing,
      defaultCacheBehavior = Core.Nothing,
      location = Core.Nothing,
      certificateName = Core.Nothing
    }

-- | Indicates whether the distribution is enabled.
lightsailDistribution_isEnabled :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
lightsailDistribution_isEnabled = Lens.lens (\LightsailDistribution' {isEnabled} -> isEnabled) (\s@LightsailDistribution' {} a -> s {isEnabled = a} :: LightsailDistribution)

-- | The IP address type of the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
lightsailDistribution_ipAddressType :: Lens.Lens' LightsailDistribution (Core.Maybe IpAddressType)
lightsailDistribution_ipAddressType = Lens.lens (\LightsailDistribution' {ipAddressType} -> ipAddressType) (\s@LightsailDistribution' {} a -> s {ipAddressType = a} :: LightsailDistribution)

-- | An object that describes the origin resource of the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
lightsailDistribution_origin :: Lens.Lens' LightsailDistribution (Core.Maybe Origin)
lightsailDistribution_origin = Lens.lens (\LightsailDistribution' {origin} -> origin) (\s@LightsailDistribution' {} a -> s {origin = a} :: LightsailDistribution)

-- | The status of the distribution.
lightsailDistribution_status :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_status = Lens.lens (\LightsailDistribution' {status} -> status) (\s@LightsailDistribution' {} a -> s {status = a} :: LightsailDistribution)

-- | The public DNS of the origin.
lightsailDistribution_originPublicDNS :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_originPublicDNS = Lens.lens (\LightsailDistribution' {originPublicDNS} -> originPublicDNS) (\s@LightsailDistribution' {} a -> s {originPublicDNS = a} :: LightsailDistribution)

-- | The ID of the bundle currently applied to the distribution.
lightsailDistribution_bundleId :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_bundleId = Lens.lens (\LightsailDistribution' {bundleId} -> bundleId) (\s@LightsailDistribution' {} a -> s {bundleId = a} :: LightsailDistribution)

-- | The alternate domain names of the distribution.
lightsailDistribution_alternativeDomainNames :: Lens.Lens' LightsailDistribution (Core.Maybe [Core.Text])
lightsailDistribution_alternativeDomainNames = Lens.lens (\LightsailDistribution' {alternativeDomainNames} -> alternativeDomainNames) (\s@LightsailDistribution' {} a -> s {alternativeDomainNames = a} :: LightsailDistribution) Core.. Lens.mapping Lens._Coerce

-- | The timestamp when the distribution was created.
lightsailDistribution_createdAt :: Lens.Lens' LightsailDistribution (Core.Maybe Core.UTCTime)
lightsailDistribution_createdAt = Lens.lens (\LightsailDistribution' {createdAt} -> createdAt) (\s@LightsailDistribution' {} a -> s {createdAt = a} :: LightsailDistribution) Core.. Lens.mapping Core._Time

-- | An object that describes the cache behavior settings of the
-- distribution.
lightsailDistribution_cacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Core.Maybe CacheSettings)
lightsailDistribution_cacheBehaviorSettings = Lens.lens (\LightsailDistribution' {cacheBehaviorSettings} -> cacheBehaviorSettings) (\s@LightsailDistribution' {} a -> s {cacheBehaviorSettings = a} :: LightsailDistribution)

-- | The Amazon Resource Name (ARN) of the distribution.
lightsailDistribution_arn :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_arn = Lens.lens (\LightsailDistribution' {arn} -> arn) (\s@LightsailDistribution' {} a -> s {arn = a} :: LightsailDistribution)

-- | The Lightsail resource type (e.g., @Distribution@).
lightsailDistribution_resourceType :: Lens.Lens' LightsailDistribution (Core.Maybe ResourceType)
lightsailDistribution_resourceType = Lens.lens (\LightsailDistribution' {resourceType} -> resourceType) (\s@LightsailDistribution' {} a -> s {resourceType = a} :: LightsailDistribution)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail distribution. This code enables our
-- support team to look up your Lightsail information more easily.
lightsailDistribution_supportCode :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_supportCode = Lens.lens (\LightsailDistribution' {supportCode} -> supportCode) (\s@LightsailDistribution' {} a -> s {supportCode = a} :: LightsailDistribution)

-- | The name of the distribution.
lightsailDistribution_name :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_name = Lens.lens (\LightsailDistribution' {name} -> name) (\s@LightsailDistribution' {} a -> s {name = a} :: LightsailDistribution)

-- | The domain name of the distribution.
lightsailDistribution_domainName :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_domainName = Lens.lens (\LightsailDistribution' {domainName} -> domainName) (\s@LightsailDistribution' {} a -> s {domainName = a} :: LightsailDistribution)

-- | Indicates whether the bundle that is currently applied to your
-- distribution, specified using the @distributionName@ parameter, can be
-- changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution\'s
-- bundle.
lightsailDistribution_ableToUpdateBundle :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Bool)
lightsailDistribution_ableToUpdateBundle = Lens.lens (\LightsailDistribution' {ableToUpdateBundle} -> ableToUpdateBundle) (\s@LightsailDistribution' {} a -> s {ableToUpdateBundle = a} :: LightsailDistribution)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
lightsailDistribution_tags :: Lens.Lens' LightsailDistribution (Core.Maybe [Tag])
lightsailDistribution_tags = Lens.lens (\LightsailDistribution' {tags} -> tags) (\s@LightsailDistribution' {} a -> s {tags = a} :: LightsailDistribution) Core.. Lens.mapping Lens._Coerce

-- | An array of objects that describe the per-path cache behavior of the
-- distribution.
lightsailDistribution_cacheBehaviors :: Lens.Lens' LightsailDistribution (Core.Maybe [CacheBehaviorPerPath])
lightsailDistribution_cacheBehaviors = Lens.lens (\LightsailDistribution' {cacheBehaviors} -> cacheBehaviors) (\s@LightsailDistribution' {} a -> s {cacheBehaviors = a} :: LightsailDistribution) Core.. Lens.mapping Lens._Coerce

-- | An object that describes the default cache behavior of the distribution.
lightsailDistribution_defaultCacheBehavior :: Lens.Lens' LightsailDistribution (Core.Maybe CacheBehavior)
lightsailDistribution_defaultCacheBehavior = Lens.lens (\LightsailDistribution' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@LightsailDistribution' {} a -> s {defaultCacheBehavior = a} :: LightsailDistribution)

-- | An object that describes the location of the distribution, such as the
-- AWS Region and Availability Zone.
--
-- Lightsail distributions are global resources that can reference an
-- origin in any AWS Region, and distribute its content globally. However,
-- all distributions are located in the @us-east-1@ Region.
lightsailDistribution_location :: Lens.Lens' LightsailDistribution (Core.Maybe ResourceLocation)
lightsailDistribution_location = Lens.lens (\LightsailDistribution' {location} -> location) (\s@LightsailDistribution' {} a -> s {location = a} :: LightsailDistribution)

-- | The name of the SSL\/TLS certificate attached to the distribution, if
-- any.
lightsailDistribution_certificateName :: Lens.Lens' LightsailDistribution (Core.Maybe Core.Text)
lightsailDistribution_certificateName = Lens.lens (\LightsailDistribution' {certificateName} -> certificateName) (\s@LightsailDistribution' {} a -> s {certificateName = a} :: LightsailDistribution)

instance Core.FromJSON LightsailDistribution where
  parseJSON =
    Core.withObject
      "LightsailDistribution"
      ( \x ->
          LightsailDistribution'
            Core.<$> (x Core..:? "isEnabled")
            Core.<*> (x Core..:? "ipAddressType")
            Core.<*> (x Core..:? "origin")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "originPublicDNS")
            Core.<*> (x Core..:? "bundleId")
            Core.<*> ( x Core..:? "alternativeDomainNames"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "cacheBehaviorSettings")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "domainName")
            Core.<*> (x Core..:? "ableToUpdateBundle")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "cacheBehaviors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "defaultCacheBehavior")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "certificateName")
      )

instance Core.Hashable LightsailDistribution

instance Core.NFData LightsailDistribution
