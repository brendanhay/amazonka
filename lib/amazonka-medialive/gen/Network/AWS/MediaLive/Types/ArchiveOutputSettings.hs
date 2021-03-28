{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ArchiveOutputSettings
  ( ArchiveOutputSettings (..)
  -- * Smart constructor
  , mkArchiveOutputSettings
  -- * Lenses
  , aosContainerSettings
  , aosExtension
  , aosNameModifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ArchiveContainerSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Archive Output Settings
--
-- /See:/ 'mkArchiveOutputSettings' smart constructor.
data ArchiveOutputSettings = ArchiveOutputSettings'
  { containerSettings :: Types.ArchiveContainerSettings
    -- ^ Settings specific to the container type of the file.
  , extension :: Core.Maybe Core.Text
    -- ^ Output file extension. If excluded, this will be auto-selected from the container type.
  , nameModifier :: Core.Maybe Core.Text
    -- ^ String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveOutputSettings' value with any optional fields omitted.
mkArchiveOutputSettings
    :: Types.ArchiveContainerSettings -- ^ 'containerSettings'
    -> ArchiveOutputSettings
mkArchiveOutputSettings containerSettings
  = ArchiveOutputSettings'{containerSettings,
                           extension = Core.Nothing, nameModifier = Core.Nothing}

-- | Settings specific to the container type of the file.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosContainerSettings :: Lens.Lens' ArchiveOutputSettings Types.ArchiveContainerSettings
aosContainerSettings = Lens.field @"containerSettings"
{-# INLINEABLE aosContainerSettings #-}
{-# DEPRECATED containerSettings "Use generic-lens or generic-optics with 'containerSettings' instead"  #-}

-- | Output file extension. If excluded, this will be auto-selected from the container type.
--
-- /Note:/ Consider using 'extension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosExtension :: Lens.Lens' ArchiveOutputSettings (Core.Maybe Core.Text)
aosExtension = Lens.field @"extension"
{-# INLINEABLE aosExtension #-}
{-# DEPRECATED extension "Use generic-lens or generic-optics with 'extension' instead"  #-}

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosNameModifier :: Lens.Lens' ArchiveOutputSettings (Core.Maybe Core.Text)
aosNameModifier = Lens.field @"nameModifier"
{-# INLINEABLE aosNameModifier #-}
{-# DEPRECATED nameModifier "Use generic-lens or generic-optics with 'nameModifier' instead"  #-}

instance Core.FromJSON ArchiveOutputSettings where
        toJSON ArchiveOutputSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerSettings" Core..= containerSettings),
                  ("extension" Core..=) Core.<$> extension,
                  ("nameModifier" Core..=) Core.<$> nameModifier])

instance Core.FromJSON ArchiveOutputSettings where
        parseJSON
          = Core.withObject "ArchiveOutputSettings" Core.$
              \ x ->
                ArchiveOutputSettings' Core.<$>
                  (x Core..: "containerSettings") Core.<*> x Core..:? "extension"
                    Core.<*> x Core..:? "nameModifier"
