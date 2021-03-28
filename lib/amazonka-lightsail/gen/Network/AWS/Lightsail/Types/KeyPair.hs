{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.KeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.KeyPair
  ( KeyPair (..)
  -- * Smart constructor
  , mkKeyPair
  -- * Lenses
  , kpArn
  , kpCreatedAt
  , kpFingerprint
  , kpLocation
  , kpName
  , kpResourceType
  , kpSupportCode
  , kpTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.Fingerprint as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the SSH key pair.
--
-- /See:/ 'mkKeyPair' smart constructor.
data KeyPair = KeyPair'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the key pair was created (e.g., @1479816991.349@ ).
  , fingerprint :: Core.Maybe Types.Fingerprint
    -- ^ The RSA fingerprint of the key pair.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The region name and Availability Zone where the key pair was created.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The friendly name of the SSH key pair.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type (usually @KeyPair@ ).
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'KeyPair' value with any optional fields omitted.
mkKeyPair
    :: KeyPair
mkKeyPair
  = KeyPair'{arn = Core.Nothing, createdAt = Core.Nothing,
             fingerprint = Core.Nothing, location = Core.Nothing,
             name = Core.Nothing, resourceType = Core.Nothing,
             supportCode = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpArn :: Lens.Lens' KeyPair (Core.Maybe Types.Arn)
kpArn = Lens.field @"arn"
{-# INLINEABLE kpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The timestamp when the key pair was created (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpCreatedAt :: Lens.Lens' KeyPair (Core.Maybe Core.NominalDiffTime)
kpCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE kpCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The RSA fingerprint of the key pair.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpFingerprint :: Lens.Lens' KeyPair (Core.Maybe Types.Fingerprint)
kpFingerprint = Lens.field @"fingerprint"
{-# INLINEABLE kpFingerprint #-}
{-# DEPRECATED fingerprint "Use generic-lens or generic-optics with 'fingerprint' instead"  #-}

-- | The region name and Availability Zone where the key pair was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpLocation :: Lens.Lens' KeyPair (Core.Maybe Types.ResourceLocation)
kpLocation = Lens.field @"location"
{-# INLINEABLE kpLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The friendly name of the SSH key pair.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpName :: Lens.Lens' KeyPair (Core.Maybe Types.ResourceName)
kpName = Lens.field @"name"
{-# INLINEABLE kpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The resource type (usually @KeyPair@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpResourceType :: Lens.Lens' KeyPair (Core.Maybe Types.ResourceType)
kpResourceType = Lens.field @"resourceType"
{-# INLINEABLE kpResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpSupportCode :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
kpSupportCode = Lens.field @"supportCode"
{-# INLINEABLE kpSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpTags :: Lens.Lens' KeyPair (Core.Maybe [Types.Tag])
kpTags = Lens.field @"tags"
{-# INLINEABLE kpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON KeyPair where
        parseJSON
          = Core.withObject "KeyPair" Core.$
              \ x ->
                KeyPair' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "fingerprint"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
