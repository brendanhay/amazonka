{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioTrackSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioTrackSelection
  ( AudioTrackSelection (..)
  -- * Smart constructor
  , mkAudioTrackSelection
  -- * Lenses
  , atsTracks
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioTrack as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Track Selection
--
-- /See:/ 'mkAudioTrackSelection' smart constructor.
newtype AudioTrackSelection = AudioTrackSelection'
  { tracks :: [Types.AudioTrack]
    -- ^ Selects one or more unique audio tracks from within a source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AudioTrackSelection' value with any optional fields omitted.
mkAudioTrackSelection
    :: AudioTrackSelection
mkAudioTrackSelection = AudioTrackSelection'{tracks = Core.mempty}

-- | Selects one or more unique audio tracks from within a source.
--
-- /Note:/ Consider using 'tracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsTracks :: Lens.Lens' AudioTrackSelection [Types.AudioTrack]
atsTracks = Lens.field @"tracks"
{-# INLINEABLE atsTracks #-}
{-# DEPRECATED tracks "Use generic-lens or generic-optics with 'tracks' instead"  #-}

instance Core.FromJSON AudioTrackSelection where
        toJSON AudioTrackSelection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("tracks" Core..= tracks)])

instance Core.FromJSON AudioTrackSelection where
        parseJSON
          = Core.withObject "AudioTrackSelection" Core.$
              \ x ->
                AudioTrackSelection' Core.<$>
                  (x Core..:? "tracks" Core..!= Core.mempty)
