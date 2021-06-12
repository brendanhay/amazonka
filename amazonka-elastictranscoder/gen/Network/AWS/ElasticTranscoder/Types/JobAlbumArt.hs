{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobAlbumArt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobAlbumArt where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.Artwork
import qualified Network.AWS.Lens as Lens

-- | The .jpg or .png file associated with an audio file.
--
-- /See:/ 'newJobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { -- | The file to be used as album art. There can be multiple artworks
    -- associated with an audio file, to a maximum of 20. Valid formats are
    -- @.jpg@ and @.png@
    artwork :: Core.Maybe [Artwork],
    -- | A policy that determines how Elastic Transcoder handles the existence of
    -- multiple album artwork files.
    --
    -- -   @Replace:@ The specified album art replaces any existing album art.
    --
    -- -   @Prepend:@ The specified album art is placed in front of any
    --     existing album art.
    --
    -- -   @Append:@ The specified album art is placed after any existing album
    --     art.
    --
    -- -   @Fallback:@ If the original input file contains artwork, Elastic
    --     Transcoder uses that artwork for the output. If the original input
    --     does not contain artwork, Elastic Transcoder uses the specified
    --     album art file.
    mergePolicy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobAlbumArt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artwork', 'jobAlbumArt_artwork' - The file to be used as album art. There can be multiple artworks
-- associated with an audio file, to a maximum of 20. Valid formats are
-- @.jpg@ and @.png@
--
-- 'mergePolicy', 'jobAlbumArt_mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of
-- multiple album artwork files.
--
-- -   @Replace:@ The specified album art replaces any existing album art.
--
-- -   @Prepend:@ The specified album art is placed in front of any
--     existing album art.
--
-- -   @Append:@ The specified album art is placed after any existing album
--     art.
--
-- -   @Fallback:@ If the original input file contains artwork, Elastic
--     Transcoder uses that artwork for the output. If the original input
--     does not contain artwork, Elastic Transcoder uses the specified
--     album art file.
newJobAlbumArt ::
  JobAlbumArt
newJobAlbumArt =
  JobAlbumArt'
    { artwork = Core.Nothing,
      mergePolicy = Core.Nothing
    }

-- | The file to be used as album art. There can be multiple artworks
-- associated with an audio file, to a maximum of 20. Valid formats are
-- @.jpg@ and @.png@
jobAlbumArt_artwork :: Lens.Lens' JobAlbumArt (Core.Maybe [Artwork])
jobAlbumArt_artwork = Lens.lens (\JobAlbumArt' {artwork} -> artwork) (\s@JobAlbumArt' {} a -> s {artwork = a} :: JobAlbumArt) Core.. Lens.mapping Lens._Coerce

-- | A policy that determines how Elastic Transcoder handles the existence of
-- multiple album artwork files.
--
-- -   @Replace:@ The specified album art replaces any existing album art.
--
-- -   @Prepend:@ The specified album art is placed in front of any
--     existing album art.
--
-- -   @Append:@ The specified album art is placed after any existing album
--     art.
--
-- -   @Fallback:@ If the original input file contains artwork, Elastic
--     Transcoder uses that artwork for the output. If the original input
--     does not contain artwork, Elastic Transcoder uses the specified
--     album art file.
jobAlbumArt_mergePolicy :: Lens.Lens' JobAlbumArt (Core.Maybe Core.Text)
jobAlbumArt_mergePolicy = Lens.lens (\JobAlbumArt' {mergePolicy} -> mergePolicy) (\s@JobAlbumArt' {} a -> s {mergePolicy = a} :: JobAlbumArt)

instance Core.FromJSON JobAlbumArt where
  parseJSON =
    Core.withObject
      "JobAlbumArt"
      ( \x ->
          JobAlbumArt'
            Core.<$> (x Core..:? "Artwork" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MergePolicy")
      )

instance Core.Hashable JobAlbumArt

instance Core.NFData JobAlbumArt

instance Core.ToJSON JobAlbumArt where
  toJSON JobAlbumArt' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Artwork" Core..=) Core.<$> artwork,
            ("MergePolicy" Core..=) Core.<$> mergePolicy
          ]
      )
