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
-- Module      : Network.AWS.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LightsailDistribution where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.IpAddressType
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Lightsail content delivery network (CDN)
-- distribution.
--
-- /See:/ 'newLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { -- | Indicates whether the distribution is enabled.
    isEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The IP address type of the distribution.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | An object that describes the origin resource of the distribution, such
    -- as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Prelude.Maybe Origin,
    -- | The status of the distribution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The public DNS of the origin.
    originPublicDNS :: Prelude.Maybe Prelude.Text,
    -- | The ID of the bundle currently applied to the distribution.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The alternate domain names of the distribution.
    alternativeDomainNames :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp when the distribution was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | An object that describes the cache behavior settings of the
    -- distribution.
    cacheBehaviorSettings :: Prelude.Maybe CacheSettings,
    -- | The Amazon Resource Name (ARN) of the distribution.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @Distribution@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail distribution. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the distribution.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the bundle that is currently applied to your
    -- distribution, specified using the @distributionName@ parameter, can be
    -- changed to another bundle.
    --
    -- Use the @UpdateDistributionBundle@ action to change your distribution\'s
    -- bundle.
    ableToUpdateBundle :: Prelude.Maybe Prelude.Bool,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | An array of objects that describe the per-path cache behavior of the
    -- distribution.
    cacheBehaviors :: Prelude.Maybe [CacheBehaviorPerPath],
    -- | An object that describes the default cache behavior of the distribution.
    defaultCacheBehavior :: Prelude.Maybe CacheBehavior,
    -- | An object that describes the location of the distribution, such as the
    -- AWS Region and Availability Zone.
    --
    -- Lightsail distributions are global resources that can reference an
    -- origin in any AWS Region, and distribute its content globally. However,
    -- all distributions are located in the @us-east-1@ Region.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the SSL\/TLS certificate attached to the distribution, if
    -- any.
    certificateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { isEnabled = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      origin = Prelude.Nothing,
      status = Prelude.Nothing,
      originPublicDNS = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      alternativeDomainNames = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      cacheBehaviorSettings = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      name = Prelude.Nothing,
      domainName = Prelude.Nothing,
      ableToUpdateBundle = Prelude.Nothing,
      tags = Prelude.Nothing,
      cacheBehaviors = Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing,
      location = Prelude.Nothing,
      certificateName = Prelude.Nothing
    }

-- | Indicates whether the distribution is enabled.
lightsailDistribution_isEnabled :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Bool)
lightsailDistribution_isEnabled = Lens.lens (\LightsailDistribution' {isEnabled} -> isEnabled) (\s@LightsailDistribution' {} a -> s {isEnabled = a} :: LightsailDistribution)

-- | The IP address type of the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
lightsailDistribution_ipAddressType :: Lens.Lens' LightsailDistribution (Prelude.Maybe IpAddressType)
lightsailDistribution_ipAddressType = Lens.lens (\LightsailDistribution' {ipAddressType} -> ipAddressType) (\s@LightsailDistribution' {} a -> s {ipAddressType = a} :: LightsailDistribution)

-- | An object that describes the origin resource of the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
lightsailDistribution_origin :: Lens.Lens' LightsailDistribution (Prelude.Maybe Origin)
lightsailDistribution_origin = Lens.lens (\LightsailDistribution' {origin} -> origin) (\s@LightsailDistribution' {} a -> s {origin = a} :: LightsailDistribution)

-- | The status of the distribution.
lightsailDistribution_status :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_status = Lens.lens (\LightsailDistribution' {status} -> status) (\s@LightsailDistribution' {} a -> s {status = a} :: LightsailDistribution)

-- | The public DNS of the origin.
lightsailDistribution_originPublicDNS :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_originPublicDNS = Lens.lens (\LightsailDistribution' {originPublicDNS} -> originPublicDNS) (\s@LightsailDistribution' {} a -> s {originPublicDNS = a} :: LightsailDistribution)

-- | The ID of the bundle currently applied to the distribution.
lightsailDistribution_bundleId :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_bundleId = Lens.lens (\LightsailDistribution' {bundleId} -> bundleId) (\s@LightsailDistribution' {} a -> s {bundleId = a} :: LightsailDistribution)

-- | The alternate domain names of the distribution.
lightsailDistribution_alternativeDomainNames :: Lens.Lens' LightsailDistribution (Prelude.Maybe [Prelude.Text])
lightsailDistribution_alternativeDomainNames = Lens.lens (\LightsailDistribution' {alternativeDomainNames} -> alternativeDomainNames) (\s@LightsailDistribution' {} a -> s {alternativeDomainNames = a} :: LightsailDistribution) Prelude.. Lens.mapping Prelude._Coerce

-- | The timestamp when the distribution was created.
lightsailDistribution_createdAt :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.UTCTime)
lightsailDistribution_createdAt = Lens.lens (\LightsailDistribution' {createdAt} -> createdAt) (\s@LightsailDistribution' {} a -> s {createdAt = a} :: LightsailDistribution) Prelude.. Lens.mapping Prelude._Time

-- | An object that describes the cache behavior settings of the
-- distribution.
lightsailDistribution_cacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Prelude.Maybe CacheSettings)
lightsailDistribution_cacheBehaviorSettings = Lens.lens (\LightsailDistribution' {cacheBehaviorSettings} -> cacheBehaviorSettings) (\s@LightsailDistribution' {} a -> s {cacheBehaviorSettings = a} :: LightsailDistribution)

-- | The Amazon Resource Name (ARN) of the distribution.
lightsailDistribution_arn :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_arn = Lens.lens (\LightsailDistribution' {arn} -> arn) (\s@LightsailDistribution' {} a -> s {arn = a} :: LightsailDistribution)

-- | The Lightsail resource type (e.g., @Distribution@).
lightsailDistribution_resourceType :: Lens.Lens' LightsailDistribution (Prelude.Maybe ResourceType)
lightsailDistribution_resourceType = Lens.lens (\LightsailDistribution' {resourceType} -> resourceType) (\s@LightsailDistribution' {} a -> s {resourceType = a} :: LightsailDistribution)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail distribution. This code enables our
-- support team to look up your Lightsail information more easily.
lightsailDistribution_supportCode :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_supportCode = Lens.lens (\LightsailDistribution' {supportCode} -> supportCode) (\s@LightsailDistribution' {} a -> s {supportCode = a} :: LightsailDistribution)

-- | The name of the distribution.
lightsailDistribution_name :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_name = Lens.lens (\LightsailDistribution' {name} -> name) (\s@LightsailDistribution' {} a -> s {name = a} :: LightsailDistribution)

-- | The domain name of the distribution.
lightsailDistribution_domainName :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_domainName = Lens.lens (\LightsailDistribution' {domainName} -> domainName) (\s@LightsailDistribution' {} a -> s {domainName = a} :: LightsailDistribution)

-- | Indicates whether the bundle that is currently applied to your
-- distribution, specified using the @distributionName@ parameter, can be
-- changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution\'s
-- bundle.
lightsailDistribution_ableToUpdateBundle :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Bool)
lightsailDistribution_ableToUpdateBundle = Lens.lens (\LightsailDistribution' {ableToUpdateBundle} -> ableToUpdateBundle) (\s@LightsailDistribution' {} a -> s {ableToUpdateBundle = a} :: LightsailDistribution)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
lightsailDistribution_tags :: Lens.Lens' LightsailDistribution (Prelude.Maybe [Tag])
lightsailDistribution_tags = Lens.lens (\LightsailDistribution' {tags} -> tags) (\s@LightsailDistribution' {} a -> s {tags = a} :: LightsailDistribution) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of objects that describe the per-path cache behavior of the
-- distribution.
lightsailDistribution_cacheBehaviors :: Lens.Lens' LightsailDistribution (Prelude.Maybe [CacheBehaviorPerPath])
lightsailDistribution_cacheBehaviors = Lens.lens (\LightsailDistribution' {cacheBehaviors} -> cacheBehaviors) (\s@LightsailDistribution' {} a -> s {cacheBehaviors = a} :: LightsailDistribution) Prelude.. Lens.mapping Prelude._Coerce

-- | An object that describes the default cache behavior of the distribution.
lightsailDistribution_defaultCacheBehavior :: Lens.Lens' LightsailDistribution (Prelude.Maybe CacheBehavior)
lightsailDistribution_defaultCacheBehavior = Lens.lens (\LightsailDistribution' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@LightsailDistribution' {} a -> s {defaultCacheBehavior = a} :: LightsailDistribution)

-- | An object that describes the location of the distribution, such as the
-- AWS Region and Availability Zone.
--
-- Lightsail distributions are global resources that can reference an
-- origin in any AWS Region, and distribute its content globally. However,
-- all distributions are located in the @us-east-1@ Region.
lightsailDistribution_location :: Lens.Lens' LightsailDistribution (Prelude.Maybe ResourceLocation)
lightsailDistribution_location = Lens.lens (\LightsailDistribution' {location} -> location) (\s@LightsailDistribution' {} a -> s {location = a} :: LightsailDistribution)

-- | The name of the SSL\/TLS certificate attached to the distribution, if
-- any.
lightsailDistribution_certificateName :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_certificateName = Lens.lens (\LightsailDistribution' {certificateName} -> certificateName) (\s@LightsailDistribution' {} a -> s {certificateName = a} :: LightsailDistribution)

instance Prelude.FromJSON LightsailDistribution where
  parseJSON =
    Prelude.withObject
      "LightsailDistribution"
      ( \x ->
          LightsailDistribution'
            Prelude.<$> (x Prelude..:? "isEnabled")
            Prelude.<*> (x Prelude..:? "ipAddressType")
            Prelude.<*> (x Prelude..:? "origin")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "originPublicDNS")
            Prelude.<*> (x Prelude..:? "bundleId")
            Prelude.<*> ( x Prelude..:? "alternativeDomainNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "cacheBehaviorSettings")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "domainName")
            Prelude.<*> (x Prelude..:? "ableToUpdateBundle")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "cacheBehaviors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "defaultCacheBehavior")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "certificateName")
      )

instance Prelude.Hashable LightsailDistribution

instance Prelude.NFData LightsailDistribution
