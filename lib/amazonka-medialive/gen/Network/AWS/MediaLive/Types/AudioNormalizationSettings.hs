{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioNormalizationSettings
  ( AudioNormalizationSettings (..)
  -- * Smart constructor
  , mkAudioNormalizationSettings
  -- * Lenses
  , ansAlgorithm
  , ansAlgorithmControl
  , ansTargetLkfs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm as Types
import qualified Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Normalization Settings
--
-- /See:/ 'mkAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { algorithm :: Core.Maybe Types.AudioNormalizationAlgorithm
    -- ^ Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
  , algorithmControl :: Core.Maybe Types.AudioNormalizationAlgorithmControl
    -- ^ When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
  , targetLkfs :: Core.Maybe Core.Double
    -- ^ Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioNormalizationSettings' value with any optional fields omitted.
mkAudioNormalizationSettings
    :: AudioNormalizationSettings
mkAudioNormalizationSettings
  = AudioNormalizationSettings'{algorithm = Core.Nothing,
                                algorithmControl = Core.Nothing, targetLkfs = Core.Nothing}

-- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithm :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationAlgorithm)
ansAlgorithm = Lens.field @"algorithm"
{-# INLINEABLE ansAlgorithm #-}
{-# DEPRECATED algorithm "Use generic-lens or generic-optics with 'algorithm' instead"  #-}

-- | When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
--
-- /Note:/ Consider using 'algorithmControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithmControl :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationAlgorithmControl)
ansAlgorithmControl = Lens.field @"algorithmControl"
{-# INLINEABLE ansAlgorithmControl #-}
{-# DEPRECATED algorithmControl "Use generic-lens or generic-optics with 'algorithmControl' instead"  #-}

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
--
-- /Note:/ Consider using 'targetLkfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansTargetLkfs :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Core.Double)
ansTargetLkfs = Lens.field @"targetLkfs"
{-# INLINEABLE ansTargetLkfs #-}
{-# DEPRECATED targetLkfs "Use generic-lens or generic-optics with 'targetLkfs' instead"  #-}

instance Core.FromJSON AudioNormalizationSettings where
        toJSON AudioNormalizationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("algorithm" Core..=) Core.<$> algorithm,
                  ("algorithmControl" Core..=) Core.<$> algorithmControl,
                  ("targetLkfs" Core..=) Core.<$> targetLkfs])

instance Core.FromJSON AudioNormalizationSettings where
        parseJSON
          = Core.withObject "AudioNormalizationSettings" Core.$
              \ x ->
                AudioNormalizationSettings' Core.<$>
                  (x Core..:? "algorithm") Core.<*> x Core..:? "algorithmControl"
                    Core.<*> x Core..:? "targetLkfs"
