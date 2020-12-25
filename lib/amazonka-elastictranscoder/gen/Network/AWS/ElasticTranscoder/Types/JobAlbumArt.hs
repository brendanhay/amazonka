{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobAlbumArt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobAlbumArt
  ( JobAlbumArt (..),

    -- * Smart constructor
    mkJobAlbumArt,

    -- * Lenses
    jaaArtwork,
    jaaMergePolicy,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Artwork as Types
import qualified Network.AWS.ElasticTranscoder.Types.MergePolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The .jpg or .png file associated with an audio file.
--
-- /See:/ 'mkJobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { -- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
    artwork :: Core.Maybe [Types.Artwork],
    -- | A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.
    --
    --
    --     * @Replace:@ The specified album art replaces any existing album art.
    --
    --
    --     * @Prepend:@ The specified album art is placed in front of any existing album art.
    --
    --
    --     * @Append:@ The specified album art is placed after any existing album art.
    --
    --
    --     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
    mergePolicy :: Core.Maybe Types.MergePolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobAlbumArt' value with any optional fields omitted.
mkJobAlbumArt ::
  JobAlbumArt
mkJobAlbumArt =
  JobAlbumArt' {artwork = Core.Nothing, mergePolicy = Core.Nothing}

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
--
-- /Note:/ Consider using 'artwork' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jaaArtwork :: Lens.Lens' JobAlbumArt (Core.Maybe [Types.Artwork])
jaaArtwork = Lens.field @"artwork"
{-# DEPRECATED jaaArtwork "Use generic-lens or generic-optics with 'artwork' instead." #-}

-- | A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.
--
--
--     * @Replace:@ The specified album art replaces any existing album art.
--
--
--     * @Prepend:@ The specified album art is placed in front of any existing album art.
--
--
--     * @Append:@ The specified album art is placed after any existing album art.
--
--
--     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
--
--
--
-- /Note:/ Consider using 'mergePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jaaMergePolicy :: Lens.Lens' JobAlbumArt (Core.Maybe Types.MergePolicy)
jaaMergePolicy = Lens.field @"mergePolicy"
{-# DEPRECATED jaaMergePolicy "Use generic-lens or generic-optics with 'mergePolicy' instead." #-}

instance Core.FromJSON JobAlbumArt where
  toJSON JobAlbumArt {..} =
    Core.object
      ( Core.catMaybes
          [ ("Artwork" Core..=) Core.<$> artwork,
            ("MergePolicy" Core..=) Core.<$> mergePolicy
          ]
      )

instance Core.FromJSON JobAlbumArt where
  parseJSON =
    Core.withObject "JobAlbumArt" Core.$
      \x ->
        JobAlbumArt'
          Core.<$> (x Core..:? "Artwork") Core.<*> (x Core..:? "MergePolicy")
