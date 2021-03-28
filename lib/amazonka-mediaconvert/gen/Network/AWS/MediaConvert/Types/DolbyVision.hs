{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DolbyVision
  ( DolbyVision (..)
  -- * Smart constructor
  , mkDolbyVision
  -- * Lenses
  , dvL6Metadata
  , dvL6Mode
  , dvProfile
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata as Types
import qualified Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode as Types
import qualified Network.AWS.MediaConvert.Types.DolbyVisionProfile as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for Dolby Vision
--
-- /See:/ 'mkDolbyVision' smart constructor.
data DolbyVision = DolbyVision'
  { l6Metadata :: Core.Maybe Types.DolbyVisionLevel6Metadata
    -- ^ Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
  , l6Mode :: Core.Maybe Types.DolbyVisionLevel6Mode
    -- ^ Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
  , profile :: Core.Maybe Types.DolbyVisionProfile
    -- ^ In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DolbyVision' value with any optional fields omitted.
mkDolbyVision
    :: DolbyVision
mkDolbyVision
  = DolbyVision'{l6Metadata = Core.Nothing, l6Mode = Core.Nothing,
                 profile = Core.Nothing}

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
--
-- /Note:/ Consider using 'l6Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvL6Metadata :: Lens.Lens' DolbyVision (Core.Maybe Types.DolbyVisionLevel6Metadata)
dvL6Metadata = Lens.field @"l6Metadata"
{-# INLINEABLE dvL6Metadata #-}
{-# DEPRECATED l6Metadata "Use generic-lens or generic-optics with 'l6Metadata' instead"  #-}

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
--
-- /Note:/ Consider using 'l6Mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvL6Mode :: Lens.Lens' DolbyVision (Core.Maybe Types.DolbyVisionLevel6Mode)
dvL6Mode = Lens.field @"l6Mode"
{-# INLINEABLE dvL6Mode #-}
{-# DEPRECATED l6Mode "Use generic-lens or generic-optics with 'l6Mode' instead"  #-}

-- | In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvProfile :: Lens.Lens' DolbyVision (Core.Maybe Types.DolbyVisionProfile)
dvProfile = Lens.field @"profile"
{-# INLINEABLE dvProfile #-}
{-# DEPRECATED profile "Use generic-lens or generic-optics with 'profile' instead"  #-}

instance Core.FromJSON DolbyVision where
        toJSON DolbyVision{..}
          = Core.object
              (Core.catMaybes
                 [("l6Metadata" Core..=) Core.<$> l6Metadata,
                  ("l6Mode" Core..=) Core.<$> l6Mode,
                  ("profile" Core..=) Core.<$> profile])

instance Core.FromJSON DolbyVision where
        parseJSON
          = Core.withObject "DolbyVision" Core.$
              \ x ->
                DolbyVision' Core.<$>
                  (x Core..:? "l6Metadata") Core.<*> x Core..:? "l6Mode" Core.<*>
                    x Core..:? "profile"
