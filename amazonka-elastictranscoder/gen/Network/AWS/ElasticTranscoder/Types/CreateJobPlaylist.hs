{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the master playlist.
--
-- /See:/ 'newCreateJobPlaylist' smart constructor.
data CreateJobPlaylist = CreateJobPlaylist'
  { -- | The DRM settings, if any, that you want Elastic Transcoder to apply to
    -- the output files associated with this playlist.
    playReadyDrm :: Prelude.Maybe PlayReadyDrm,
    -- | For each output in this job that you want to include in a master
    -- playlist, the value of the @Outputs:Key@ object.
    --
    -- -   If your output is not @HLS@ or does not have a segment duration set,
    --     the name of the output file is a concatenation of @OutputKeyPrefix@
    --     and @Outputs:Key@:
    --
    --     OutputKeyPrefix@Outputs:Key@
    --
    -- -   If your output is @HLSv3@ and has a segment duration set, or is not
    --     included in a playlist, Elastic Transcoder creates an output
    --     playlist file with a file extension of @.m3u8@, and a series of
    --     @.ts@ files that include a five-digit sequential counter beginning
    --     with 00000:
    --
    --     OutputKeyPrefix@Outputs:Key@.m3u8
    --
    --     OutputKeyPrefix@Outputs:Key@00000.ts
    --
    -- -   If your output is @HLSv4@, has a segment duration set, and is
    --     included in an @HLSv4@ playlist, Elastic Transcoder creates an
    --     output playlist file with a file extension of @_v4.m3u8@. If the
    --     output is video, Elastic Transcoder also creates an output file with
    --     an extension of @_iframe.m3u8@:
    --
    --     OutputKeyPrefix@Outputs:Key@_v4.m3u8
    --
    --     OutputKeyPrefix@Outputs:Key@_iframe.m3u8
    --
    --     OutputKeyPrefix@Outputs:Key@.ts
    --
    -- Elastic Transcoder automatically appends the relevant file extension to
    -- the file name. If you include a file extension in Output Key, the file
    -- name will have two extensions.
    --
    -- If you include more than one output in a playlist, any segment duration
    -- settings, clip settings, or caption settings must be the same for all
    -- outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@,
    -- @Video:Profile@, and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio
    -- must be the same for all outputs.
    outputKeys :: Prelude.Maybe [Prelude.Text],
    -- | The format of the output playlist. Valid formats include @HLSv3@,
    -- @HLSv4@, and @Smooth@.
    format :: Prelude.Maybe Prelude.Text,
    -- | The HLS content protection settings, if any, that you want Elastic
    -- Transcoder to apply to the output files associated with this playlist.
    hlsContentProtection :: Prelude.Maybe HlsContentProtection,
    -- | The name that you want Elastic Transcoder to assign to the master
    -- playlist, for example, nyc-vacation.m3u8. If the name includes a @\/@
    -- character, the section of the name before the last @\/@ must be
    -- identical for all @Name@ objects. If you create more than one master
    -- playlist, the values of all @Name@ objects must be unique.
    --
    -- Elastic Transcoder automatically appends the relevant file extension to
    -- the file name (@.m3u8@ for @HLSv3@ and @HLSv4@ playlists, and @.ism@ and
    -- @.ismc@ for @Smooth@ playlists). If you include a file extension in
    -- @Name@, the file name will have two extensions.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateJobPlaylist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playReadyDrm', 'createJobPlaylist_playReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
--
-- 'outputKeys', 'createJobPlaylist_outputKeys' - For each output in this job that you want to include in a master
-- playlist, the value of the @Outputs:Key@ object.
--
-- -   If your output is not @HLS@ or does not have a segment duration set,
--     the name of the output file is a concatenation of @OutputKeyPrefix@
--     and @Outputs:Key@:
--
--     OutputKeyPrefix@Outputs:Key@
--
-- -   If your output is @HLSv3@ and has a segment duration set, or is not
--     included in a playlist, Elastic Transcoder creates an output
--     playlist file with a file extension of @.m3u8@, and a series of
--     @.ts@ files that include a five-digit sequential counter beginning
--     with 00000:
--
--     OutputKeyPrefix@Outputs:Key@.m3u8
--
--     OutputKeyPrefix@Outputs:Key@00000.ts
--
-- -   If your output is @HLSv4@, has a segment duration set, and is
--     included in an @HLSv4@ playlist, Elastic Transcoder creates an
--     output playlist file with a file extension of @_v4.m3u8@. If the
--     output is video, Elastic Transcoder also creates an output file with
--     an extension of @_iframe.m3u8@:
--
--     OutputKeyPrefix@Outputs:Key@_v4.m3u8
--
--     OutputKeyPrefix@Outputs:Key@_iframe.m3u8
--
--     OutputKeyPrefix@Outputs:Key@.ts
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name. If you include a file extension in Output Key, the file
-- name will have two extensions.
--
-- If you include more than one output in a playlist, any segment duration
-- settings, clip settings, or caption settings must be the same for all
-- outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@,
-- @Video:Profile@, and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio
-- must be the same for all outputs.
--
-- 'format', 'createJobPlaylist_format' - The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
--
-- 'hlsContentProtection', 'createJobPlaylist_hlsContentProtection' - The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
--
-- 'name', 'createJobPlaylist_name' - The name that you want Elastic Transcoder to assign to the master
-- playlist, for example, nyc-vacation.m3u8. If the name includes a @\/@
-- character, the section of the name before the last @\/@ must be
-- identical for all @Name@ objects. If you create more than one master
-- playlist, the values of all @Name@ objects must be unique.
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name (@.m3u8@ for @HLSv3@ and @HLSv4@ playlists, and @.ism@ and
-- @.ismc@ for @Smooth@ playlists). If you include a file extension in
-- @Name@, the file name will have two extensions.
newCreateJobPlaylist ::
  CreateJobPlaylist
newCreateJobPlaylist =
  CreateJobPlaylist'
    { playReadyDrm = Prelude.Nothing,
      outputKeys = Prelude.Nothing,
      format = Prelude.Nothing,
      hlsContentProtection = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
createJobPlaylist_playReadyDrm :: Lens.Lens' CreateJobPlaylist (Prelude.Maybe PlayReadyDrm)
createJobPlaylist_playReadyDrm = Lens.lens (\CreateJobPlaylist' {playReadyDrm} -> playReadyDrm) (\s@CreateJobPlaylist' {} a -> s {playReadyDrm = a} :: CreateJobPlaylist)

-- | For each output in this job that you want to include in a master
-- playlist, the value of the @Outputs:Key@ object.
--
-- -   If your output is not @HLS@ or does not have a segment duration set,
--     the name of the output file is a concatenation of @OutputKeyPrefix@
--     and @Outputs:Key@:
--
--     OutputKeyPrefix@Outputs:Key@
--
-- -   If your output is @HLSv3@ and has a segment duration set, or is not
--     included in a playlist, Elastic Transcoder creates an output
--     playlist file with a file extension of @.m3u8@, and a series of
--     @.ts@ files that include a five-digit sequential counter beginning
--     with 00000:
--
--     OutputKeyPrefix@Outputs:Key@.m3u8
--
--     OutputKeyPrefix@Outputs:Key@00000.ts
--
-- -   If your output is @HLSv4@, has a segment duration set, and is
--     included in an @HLSv4@ playlist, Elastic Transcoder creates an
--     output playlist file with a file extension of @_v4.m3u8@. If the
--     output is video, Elastic Transcoder also creates an output file with
--     an extension of @_iframe.m3u8@:
--
--     OutputKeyPrefix@Outputs:Key@_v4.m3u8
--
--     OutputKeyPrefix@Outputs:Key@_iframe.m3u8
--
--     OutputKeyPrefix@Outputs:Key@.ts
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name. If you include a file extension in Output Key, the file
-- name will have two extensions.
--
-- If you include more than one output in a playlist, any segment duration
-- settings, clip settings, or caption settings must be the same for all
-- outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@,
-- @Video:Profile@, and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio
-- must be the same for all outputs.
createJobPlaylist_outputKeys :: Lens.Lens' CreateJobPlaylist (Prelude.Maybe [Prelude.Text])
createJobPlaylist_outputKeys = Lens.lens (\CreateJobPlaylist' {outputKeys} -> outputKeys) (\s@CreateJobPlaylist' {} a -> s {outputKeys = a} :: CreateJobPlaylist) Prelude.. Lens.mapping Prelude._Coerce

-- | The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
createJobPlaylist_format :: Lens.Lens' CreateJobPlaylist (Prelude.Maybe Prelude.Text)
createJobPlaylist_format = Lens.lens (\CreateJobPlaylist' {format} -> format) (\s@CreateJobPlaylist' {} a -> s {format = a} :: CreateJobPlaylist)

-- | The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
createJobPlaylist_hlsContentProtection :: Lens.Lens' CreateJobPlaylist (Prelude.Maybe HlsContentProtection)
createJobPlaylist_hlsContentProtection = Lens.lens (\CreateJobPlaylist' {hlsContentProtection} -> hlsContentProtection) (\s@CreateJobPlaylist' {} a -> s {hlsContentProtection = a} :: CreateJobPlaylist)

-- | The name that you want Elastic Transcoder to assign to the master
-- playlist, for example, nyc-vacation.m3u8. If the name includes a @\/@
-- character, the section of the name before the last @\/@ must be
-- identical for all @Name@ objects. If you create more than one master
-- playlist, the values of all @Name@ objects must be unique.
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name (@.m3u8@ for @HLSv3@ and @HLSv4@ playlists, and @.ism@ and
-- @.ismc@ for @Smooth@ playlists). If you include a file extension in
-- @Name@, the file name will have two extensions.
createJobPlaylist_name :: Lens.Lens' CreateJobPlaylist (Prelude.Maybe Prelude.Text)
createJobPlaylist_name = Lens.lens (\CreateJobPlaylist' {name} -> name) (\s@CreateJobPlaylist' {} a -> s {name = a} :: CreateJobPlaylist)

instance Prelude.Hashable CreateJobPlaylist

instance Prelude.NFData CreateJobPlaylist

instance Prelude.ToJSON CreateJobPlaylist where
  toJSON CreateJobPlaylist' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlayReadyDrm" Prelude..=)
              Prelude.<$> playReadyDrm,
            ("OutputKeys" Prelude..=) Prelude.<$> outputKeys,
            ("Format" Prelude..=) Prelude.<$> format,
            ("HlsContentProtection" Prelude..=)
              Prelude.<$> hlsContentProtection,
            ("Name" Prelude..=) Prelude.<$> name
          ]
      )
