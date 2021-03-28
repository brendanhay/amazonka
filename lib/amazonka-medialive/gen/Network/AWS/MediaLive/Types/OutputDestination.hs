{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OutputDestination
  ( OutputDestination (..)
  -- * Smart constructor
  , mkOutputDestination
  -- * Lenses
  , odId
  , odMediaPackageSettings
  , odMultiplexSettings
  , odSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.OutputDestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for OutputDestination
--
-- /See:/ 'mkOutputDestination' smart constructor.
data OutputDestination = OutputDestination'
  { id :: Core.Maybe Core.Text
    -- ^ User-specified id. This is used in an output group or an output.
  , mediaPackageSettings :: Core.Maybe [Types.MediaPackageOutputDestinationSettings]
    -- ^ Destination settings for a MediaPackage output; one destination for both encoders.
  , multiplexSettings :: Core.Maybe Types.MultiplexProgramChannelDestinationSettings
    -- ^ Destination settings for a Multiplex output; one destination for both encoders.
  , settings :: Core.Maybe [Types.OutputDestinationSettings]
    -- ^ Destination settings for a standard output; one destination for each redundant encoder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDestination' value with any optional fields omitted.
mkOutputDestination
    :: OutputDestination
mkOutputDestination
  = OutputDestination'{id = Core.Nothing,
                       mediaPackageSettings = Core.Nothing,
                       multiplexSettings = Core.Nothing, settings = Core.Nothing}

-- | User-specified id. This is used in an output group or an output.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odId :: Lens.Lens' OutputDestination (Core.Maybe Core.Text)
odId = Lens.field @"id"
{-# INLINEABLE odId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Destination settings for a MediaPackage output; one destination for both encoders.
--
-- /Note:/ Consider using 'mediaPackageSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odMediaPackageSettings :: Lens.Lens' OutputDestination (Core.Maybe [Types.MediaPackageOutputDestinationSettings])
odMediaPackageSettings = Lens.field @"mediaPackageSettings"
{-# INLINEABLE odMediaPackageSettings #-}
{-# DEPRECATED mediaPackageSettings "Use generic-lens or generic-optics with 'mediaPackageSettings' instead"  #-}

-- | Destination settings for a Multiplex output; one destination for both encoders.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odMultiplexSettings :: Lens.Lens' OutputDestination (Core.Maybe Types.MultiplexProgramChannelDestinationSettings)
odMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE odMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | Destination settings for a standard output; one destination for each redundant encoder.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odSettings :: Lens.Lens' OutputDestination (Core.Maybe [Types.OutputDestinationSettings])
odSettings = Lens.field @"settings"
{-# INLINEABLE odSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

instance Core.FromJSON OutputDestination where
        toJSON OutputDestination{..}
          = Core.object
              (Core.catMaybes
                 [("id" Core..=) Core.<$> id,
                  ("mediaPackageSettings" Core..=) Core.<$> mediaPackageSettings,
                  ("multiplexSettings" Core..=) Core.<$> multiplexSettings,
                  ("settings" Core..=) Core.<$> settings])

instance Core.FromJSON OutputDestination where
        parseJSON
          = Core.withObject "OutputDestination" Core.$
              \ x ->
                OutputDestination' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "mediaPackageSettings"
                    Core.<*> x Core..:? "multiplexSettings"
                    Core.<*> x Core..:? "settings"
