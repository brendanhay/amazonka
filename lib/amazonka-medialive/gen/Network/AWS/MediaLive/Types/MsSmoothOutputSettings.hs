{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MsSmoothOutputSettings
  ( MsSmoothOutputSettings (..)
  -- * Smart constructor
  , mkMsSmoothOutputSettings
  -- * Lenses
  , msosH265PackagingType
  , msosNameModifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MsSmoothH265PackagingType as Types
import qualified Network.AWS.Prelude as Core

-- | Ms Smooth Output Settings
--
-- /See:/ 'mkMsSmoothOutputSettings' smart constructor.
data MsSmoothOutputSettings = MsSmoothOutputSettings'
  { h265PackagingType :: Core.Maybe Types.MsSmoothH265PackagingType
    -- ^ Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
  , nameModifier :: Core.Maybe Core.Text
    -- ^ String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MsSmoothOutputSettings' value with any optional fields omitted.
mkMsSmoothOutputSettings
    :: MsSmoothOutputSettings
mkMsSmoothOutputSettings
  = MsSmoothOutputSettings'{h265PackagingType = Core.Nothing,
                            nameModifier = Core.Nothing}

-- | Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- /Note:/ Consider using 'h265PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msosH265PackagingType :: Lens.Lens' MsSmoothOutputSettings (Core.Maybe Types.MsSmoothH265PackagingType)
msosH265PackagingType = Lens.field @"h265PackagingType"
{-# INLINEABLE msosH265PackagingType #-}
{-# DEPRECATED h265PackagingType "Use generic-lens or generic-optics with 'h265PackagingType' instead"  #-}

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msosNameModifier :: Lens.Lens' MsSmoothOutputSettings (Core.Maybe Core.Text)
msosNameModifier = Lens.field @"nameModifier"
{-# INLINEABLE msosNameModifier #-}
{-# DEPRECATED nameModifier "Use generic-lens or generic-optics with 'nameModifier' instead"  #-}

instance Core.FromJSON MsSmoothOutputSettings where
        toJSON MsSmoothOutputSettings{..}
          = Core.object
              (Core.catMaybes
                 [("h265PackagingType" Core..=) Core.<$> h265PackagingType,
                  ("nameModifier" Core..=) Core.<$> nameModifier])

instance Core.FromJSON MsSmoothOutputSettings where
        parseJSON
          = Core.withObject "MsSmoothOutputSettings" Core.$
              \ x ->
                MsSmoothOutputSettings' Core.<$>
                  (x Core..:? "h265PackagingType") Core.<*> x Core..:? "nameModifier"
