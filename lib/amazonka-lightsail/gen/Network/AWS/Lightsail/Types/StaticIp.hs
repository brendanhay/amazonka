{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.StaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.StaticIp
  ( StaticIp (..)
  -- * Smart constructor
  , mkStaticIp
  -- * Lenses
  , siArn
  , siAttachedTo
  , siCreatedAt
  , siIpAddress
  , siIsAttached
  , siLocation
  , siName
  , siResourceType
  , siSupportCode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.IpAddress as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the static IP.
--
-- /See:/ 'mkStaticIp' smart constructor.
data StaticIp = StaticIp'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
  , attachedTo :: Core.Maybe Types.ResourceName
    -- ^ The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the static IP was created (e.g., @1479735304.222@ ).
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The static IP address.
  , isAttached :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the static IP is attached.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The region and Availability Zone where the static IP was created.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type (usually @StaticIp@ ).
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StaticIp' value with any optional fields omitted.
mkStaticIp
    :: StaticIp
mkStaticIp
  = StaticIp'{arn = Core.Nothing, attachedTo = Core.Nothing,
              createdAt = Core.Nothing, ipAddress = Core.Nothing,
              isAttached = Core.Nothing, location = Core.Nothing,
              name = Core.Nothing, resourceType = Core.Nothing,
              supportCode = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siArn :: Lens.Lens' StaticIp (Core.Maybe Types.Arn)
siArn = Lens.field @"arn"
{-# INLINEABLE siArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'attachedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAttachedTo :: Lens.Lens' StaticIp (Core.Maybe Types.ResourceName)
siAttachedTo = Lens.field @"attachedTo"
{-# INLINEABLE siAttachedTo #-}
{-# DEPRECATED attachedTo "Use generic-lens or generic-optics with 'attachedTo' instead"  #-}

-- | The timestamp when the static IP was created (e.g., @1479735304.222@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatedAt :: Lens.Lens' StaticIp (Core.Maybe Core.NominalDiffTime)
siCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE siCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The static IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIpAddress :: Lens.Lens' StaticIp (Core.Maybe Types.IpAddress)
siIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE siIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | A Boolean value indicating whether the static IP is attached.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIsAttached :: Lens.Lens' StaticIp (Core.Maybe Core.Bool)
siIsAttached = Lens.field @"isAttached"
{-# INLINEABLE siIsAttached #-}
{-# DEPRECATED isAttached "Use generic-lens or generic-optics with 'isAttached' instead"  #-}

-- | The region and Availability Zone where the static IP was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLocation :: Lens.Lens' StaticIp (Core.Maybe Types.ResourceLocation)
siLocation = Lens.field @"location"
{-# INLINEABLE siLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' StaticIp (Core.Maybe Types.ResourceName)
siName = Lens.field @"name"
{-# INLINEABLE siName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The resource type (usually @StaticIp@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siResourceType :: Lens.Lens' StaticIp (Core.Maybe Types.ResourceType)
siResourceType = Lens.field @"resourceType"
{-# INLINEABLE siResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSupportCode :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
siSupportCode = Lens.field @"supportCode"
{-# INLINEABLE siSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

instance Core.FromJSON StaticIp where
        parseJSON
          = Core.withObject "StaticIp" Core.$
              \ x ->
                StaticIp' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "attachedTo" Core.<*>
                    x Core..:? "createdAt"
                    Core.<*> x Core..:? "ipAddress"
                    Core.<*> x Core..:? "isAttached"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "supportCode"
