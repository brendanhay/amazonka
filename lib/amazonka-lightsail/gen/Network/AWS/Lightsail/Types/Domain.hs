{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Domain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Domain
  ( Domain (..)
  -- * Smart constructor
  , mkDomain
  -- * Lenses
  , dfArn
  , dfCreatedAt
  , dfDomainEntries
  , dfLocation
  , dfName
  , dfResourceType
  , dfSupportCode
  , dfTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.DomainEntry as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a domain where you are storing recordsets in Lightsail.
--
-- /See:/ 'mkDomain' smart constructor.
data Domain = Domain'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the domain recordset was created.
  , domainEntries :: Core.Maybe [Types.DomainEntry]
    -- ^ An array of key-value pairs containing information about the domain entries.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The AWS Region and Availability Zones where the domain recordset was created.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the domain.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type. 
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Domain' value with any optional fields omitted.
mkDomain
    :: Domain
mkDomain
  = Domain'{arn = Core.Nothing, createdAt = Core.Nothing,
            domainEntries = Core.Nothing, location = Core.Nothing,
            name = Core.Nothing, resourceType = Core.Nothing,
            supportCode = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfArn :: Lens.Lens' Domain (Core.Maybe Types.Arn)
dfArn = Lens.field @"arn"
{-# INLINEABLE dfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date when the domain recordset was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCreatedAt :: Lens.Lens' Domain (Core.Maybe Core.NominalDiffTime)
dfCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE dfCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An array of key-value pairs containing information about the domain entries.
--
-- /Note:/ Consider using 'domainEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDomainEntries :: Lens.Lens' Domain (Core.Maybe [Types.DomainEntry])
dfDomainEntries = Lens.field @"domainEntries"
{-# INLINEABLE dfDomainEntries #-}
{-# DEPRECATED domainEntries "Use generic-lens or generic-optics with 'domainEntries' instead"  #-}

-- | The AWS Region and Availability Zones where the domain recordset was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLocation :: Lens.Lens' Domain (Core.Maybe Types.ResourceLocation)
dfLocation = Lens.field @"location"
{-# INLINEABLE dfLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' Domain (Core.Maybe Types.ResourceName)
dfName = Lens.field @"name"
{-# INLINEABLE dfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The resource type. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfResourceType :: Lens.Lens' Domain (Core.Maybe Types.ResourceType)
dfResourceType = Lens.field @"resourceType"
{-# INLINEABLE dfResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfSupportCode :: Lens.Lens' Domain (Core.Maybe Core.Text)
dfSupportCode = Lens.field @"supportCode"
{-# INLINEABLE dfSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfTags :: Lens.Lens' Domain (Core.Maybe [Types.Tag])
dfTags = Lens.field @"tags"
{-# INLINEABLE dfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON Domain where
        parseJSON
          = Core.withObject "Domain" Core.$
              \ x ->
                Domain' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "domainEntries"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
