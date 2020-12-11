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
    jaaMergePolicy,
    jaaArtwork,
  )
where

import Network.AWS.ElasticTranscoder.Types.Artwork
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The .jpg or .png file associated with an audio file.
--
-- /See:/ 'mkJobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { mergePolicy ::
      Lude.Maybe Lude.Text,
    artwork :: Lude.Maybe [Artwork]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobAlbumArt' with the minimum fields required to make a request.
--
-- * 'artwork' - The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
-- * 'mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.
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
mkJobAlbumArt ::
  JobAlbumArt
mkJobAlbumArt =
  JobAlbumArt' {mergePolicy = Lude.Nothing, artwork = Lude.Nothing}

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
jaaMergePolicy :: Lens.Lens' JobAlbumArt (Lude.Maybe Lude.Text)
jaaMergePolicy = Lens.lens (mergePolicy :: JobAlbumArt -> Lude.Maybe Lude.Text) (\s a -> s {mergePolicy = a} :: JobAlbumArt)
{-# DEPRECATED jaaMergePolicy "Use generic-lens or generic-optics with 'mergePolicy' instead." #-}

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
--
-- /Note:/ Consider using 'artwork' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jaaArtwork :: Lens.Lens' JobAlbumArt (Lude.Maybe [Artwork])
jaaArtwork = Lens.lens (artwork :: JobAlbumArt -> Lude.Maybe [Artwork]) (\s a -> s {artwork = a} :: JobAlbumArt)
{-# DEPRECATED jaaArtwork "Use generic-lens or generic-optics with 'artwork' instead." #-}

instance Lude.FromJSON JobAlbumArt where
  parseJSON =
    Lude.withObject
      "JobAlbumArt"
      ( \x ->
          JobAlbumArt'
            Lude.<$> (x Lude..:? "MergePolicy")
            Lude.<*> (x Lude..:? "Artwork" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON JobAlbumArt where
  toJSON JobAlbumArt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MergePolicy" Lude..=) Lude.<$> mergePolicy,
            ("Artwork" Lude..=) Lude.<$> artwork
          ]
      )
