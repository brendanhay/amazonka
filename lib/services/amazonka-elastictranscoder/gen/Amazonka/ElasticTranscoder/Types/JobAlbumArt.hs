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
-- Module      : Amazonka.ElasticTranscoder.Types.JobAlbumArt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.JobAlbumArt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.Artwork
import qualified Amazonka.Prelude as Prelude

-- | The .jpg or .png file associated with an audio file.
--
-- /See:/ 'newJobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { -- | The file to be used as album art. There can be multiple artworks
    -- associated with an audio file, to a maximum of 20. Valid formats are
    -- @.jpg@ and @.png@
    artwork :: Prelude.Maybe [Artwork],
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
    mergePolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { artwork = Prelude.Nothing,
      mergePolicy = Prelude.Nothing
    }

-- | The file to be used as album art. There can be multiple artworks
-- associated with an audio file, to a maximum of 20. Valid formats are
-- @.jpg@ and @.png@
jobAlbumArt_artwork :: Lens.Lens' JobAlbumArt (Prelude.Maybe [Artwork])
jobAlbumArt_artwork = Lens.lens (\JobAlbumArt' {artwork} -> artwork) (\s@JobAlbumArt' {} a -> s {artwork = a} :: JobAlbumArt) Prelude.. Lens.mapping Lens.coerced

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
jobAlbumArt_mergePolicy :: Lens.Lens' JobAlbumArt (Prelude.Maybe Prelude.Text)
jobAlbumArt_mergePolicy = Lens.lens (\JobAlbumArt' {mergePolicy} -> mergePolicy) (\s@JobAlbumArt' {} a -> s {mergePolicy = a} :: JobAlbumArt)

instance Data.FromJSON JobAlbumArt where
  parseJSON =
    Data.withObject
      "JobAlbumArt"
      ( \x ->
          JobAlbumArt'
            Prelude.<$> (x Data..:? "Artwork" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MergePolicy")
      )

instance Prelude.Hashable JobAlbumArt where
  hashWithSalt _salt JobAlbumArt' {..} =
    _salt
      `Prelude.hashWithSalt` artwork
      `Prelude.hashWithSalt` mergePolicy

instance Prelude.NFData JobAlbumArt where
  rnf JobAlbumArt' {..} =
    Prelude.rnf artwork
      `Prelude.seq` Prelude.rnf mergePolicy

instance Data.ToJSON JobAlbumArt where
  toJSON JobAlbumArt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Artwork" Data..=) Prelude.<$> artwork,
            ("MergePolicy" Data..=) Prelude.<$> mergePolicy
          ]
      )
