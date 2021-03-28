{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.VolumeFrom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.VolumeFrom
  ( VolumeFrom (..)
  -- * Smart constructor
  , mkVolumeFrom
  -- * Lenses
  , vfReadOnly
  , vfSourceContainer
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on a data volume from another container in the same task definition.
--
-- /See:/ 'mkVolumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
  { readOnly :: Core.Maybe Core.Bool
    -- ^ If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
  , sourceContainer :: Core.Maybe Core.Text
    -- ^ The name of another container within the same task definition from which to mount volumes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeFrom' value with any optional fields omitted.
mkVolumeFrom
    :: VolumeFrom
mkVolumeFrom
  = VolumeFrom'{readOnly = Core.Nothing,
                sourceContainer = Core.Nothing}

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfReadOnly :: Lens.Lens' VolumeFrom (Core.Maybe Core.Bool)
vfReadOnly = Lens.field @"readOnly"
{-# INLINEABLE vfReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | The name of another container within the same task definition from which to mount volumes.
--
-- /Note:/ Consider using 'sourceContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfSourceContainer :: Lens.Lens' VolumeFrom (Core.Maybe Core.Text)
vfSourceContainer = Lens.field @"sourceContainer"
{-# INLINEABLE vfSourceContainer #-}
{-# DEPRECATED sourceContainer "Use generic-lens or generic-optics with 'sourceContainer' instead"  #-}

instance Core.FromJSON VolumeFrom where
        toJSON VolumeFrom{..}
          = Core.object
              (Core.catMaybes
                 [("readOnly" Core..=) Core.<$> readOnly,
                  ("sourceContainer" Core..=) Core.<$> sourceContainer])

instance Core.FromJSON VolumeFrom where
        parseJSON
          = Core.withObject "VolumeFrom" Core.$
              \ x ->
                VolumeFrom' Core.<$>
                  (x Core..:? "readOnly") Core.<*> x Core..:? "sourceContainer"
