{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Core
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Core
  ( Core (..)
  -- * Smart constructor
  , mkCore
  -- * Lenses
  , cThingArn
  , cId
  , cCertificateArn
  , cSyncShadow
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a core.
--
-- /See:/ 'mkCore' smart constructor.
data Core = Core'
  { thingArn :: Core.Text
    -- ^ The ARN of the thing which is the core.
  , id :: Core.Text
    -- ^ A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
  , certificateArn :: Core.Text
    -- ^ The ARN of the certificate associated with the core.
  , syncShadow :: Core.Maybe Core.Bool
    -- ^ If true, the core's local shadow is automatically synced with the cloud.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Core' value with any optional fields omitted.
mkCore
    :: Core.Text -- ^ 'thingArn'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'certificateArn'
    -> Core
mkCore thingArn id certificateArn
  = Core'{thingArn, id, certificateArn, syncShadow = Core.Nothing}

-- | The ARN of the thing which is the core.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThingArn :: Lens.Lens' Core Core.Text
cThingArn = Lens.field @"thingArn"
{-# INLINEABLE cThingArn #-}
{-# DEPRECATED thingArn "Use generic-lens or generic-optics with 'thingArn' instead"  #-}

-- | A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Core Core.Text
cId = Lens.field @"id"
{-# INLINEABLE cId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ARN of the certificate associated with the core.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Core Core.Text
cCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | If true, the core's local shadow is automatically synced with the cloud.
--
-- /Note:/ Consider using 'syncShadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSyncShadow :: Lens.Lens' Core (Core.Maybe Core.Bool)
cSyncShadow = Lens.field @"syncShadow"
{-# INLINEABLE cSyncShadow #-}
{-# DEPRECATED syncShadow "Use generic-lens or generic-optics with 'syncShadow' instead"  #-}

instance Core.FromJSON Core where
        toJSON Core{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ThingArn" Core..= thingArn),
                  Core.Just ("Id" Core..= id),
                  Core.Just ("CertificateArn" Core..= certificateArn),
                  ("SyncShadow" Core..=) Core.<$> syncShadow])

instance Core.FromJSON Core where
        parseJSON
          = Core.withObject "Core" Core.$
              \ x ->
                Core' Core.<$>
                  (x Core..: "ThingArn") Core.<*> x Core..: "Id" Core.<*>
                    x Core..: "CertificateArn"
                    Core.<*> x Core..:? "SyncShadow"
