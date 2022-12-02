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
-- Module      : Amazonka.ElasticTranscoder.Types.Playlist
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Playlist where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.HlsContentProtection
import Amazonka.ElasticTranscoder.Types.PlayReadyDrm
import qualified Amazonka.Prelude as Prelude

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset
-- for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@
-- (MPEG-TS), Playlists contains information about the master playlists
-- that you want Elastic Transcoder to create. We recommend that you create
-- only one master playlist per output format. The maximum number of master
-- playlists in a job is 30.
--
-- /See:/ 'newPlaylist' smart constructor.
data Playlist = Playlist'
  { -- | The name that you want Elastic Transcoder to assign to the master
    -- playlist, for example, nyc-vacation.m3u8. If the name includes a @\/@
    -- character, the section of the name before the last @\/@ must be
    -- identical for all @Name@ objects. If you create more than one master
    -- playlist, the values of all @Name@ objects must be unique.
    --
    -- Elastic Transcoder automatically appends the relevant file extension to
    -- the file name (@.m3u8@ for @HLSv3@ and @HLSv4@ playlists, and @.ism@ and
    -- @.ismc@ for @Smooth@ playlists). If you include a file extension in
    -- @Name@, the file name will have two extensions.
    name :: Prelude.Maybe Prelude.Text,
    -- | The HLS content protection settings, if any, that you want Elastic
    -- Transcoder to apply to the output files associated with this playlist.
    hlsContentProtection :: Prelude.Maybe HlsContentProtection,
    -- | The DRM settings, if any, that you want Elastic Transcoder to apply to
    -- the output files associated with this playlist.
    playReadyDrm :: Prelude.Maybe PlayReadyDrm,
    -- | The format of the output playlist. Valid formats include @HLSv3@,
    -- @HLSv4@, and @Smooth@.
    format :: Prelude.Maybe Prelude.Text,
    -- | Information that further explains the status.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | The status of the job with which the playlist is associated.
    status :: Prelude.Maybe Prelude.Text,
    -- | For each output in this job that you want to include in a master
    -- playlist, the value of the Outputs:Key object.
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
    outputKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Playlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'playlist_name' - The name that you want Elastic Transcoder to assign to the master
-- playlist, for example, nyc-vacation.m3u8. If the name includes a @\/@
-- character, the section of the name before the last @\/@ must be
-- identical for all @Name@ objects. If you create more than one master
-- playlist, the values of all @Name@ objects must be unique.
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name (@.m3u8@ for @HLSv3@ and @HLSv4@ playlists, and @.ism@ and
-- @.ismc@ for @Smooth@ playlists). If you include a file extension in
-- @Name@, the file name will have two extensions.
--
-- 'hlsContentProtection', 'playlist_hlsContentProtection' - The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
--
-- 'playReadyDrm', 'playlist_playReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
--
-- 'format', 'playlist_format' - The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
--
-- 'statusDetail', 'playlist_statusDetail' - Information that further explains the status.
--
-- 'status', 'playlist_status' - The status of the job with which the playlist is associated.
--
-- 'outputKeys', 'playlist_outputKeys' - For each output in this job that you want to include in a master
-- playlist, the value of the Outputs:Key object.
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
newPlaylist ::
  Playlist
newPlaylist =
  Playlist'
    { name = Prelude.Nothing,
      hlsContentProtection = Prelude.Nothing,
      playReadyDrm = Prelude.Nothing,
      format = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      status = Prelude.Nothing,
      outputKeys = Prelude.Nothing
    }

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
playlist_name :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_name = Lens.lens (\Playlist' {name} -> name) (\s@Playlist' {} a -> s {name = a} :: Playlist)

-- | The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
playlist_hlsContentProtection :: Lens.Lens' Playlist (Prelude.Maybe HlsContentProtection)
playlist_hlsContentProtection = Lens.lens (\Playlist' {hlsContentProtection} -> hlsContentProtection) (\s@Playlist' {} a -> s {hlsContentProtection = a} :: Playlist)

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
playlist_playReadyDrm :: Lens.Lens' Playlist (Prelude.Maybe PlayReadyDrm)
playlist_playReadyDrm = Lens.lens (\Playlist' {playReadyDrm} -> playReadyDrm) (\s@Playlist' {} a -> s {playReadyDrm = a} :: Playlist)

-- | The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
playlist_format :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_format = Lens.lens (\Playlist' {format} -> format) (\s@Playlist' {} a -> s {format = a} :: Playlist)

-- | Information that further explains the status.
playlist_statusDetail :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_statusDetail = Lens.lens (\Playlist' {statusDetail} -> statusDetail) (\s@Playlist' {} a -> s {statusDetail = a} :: Playlist)

-- | The status of the job with which the playlist is associated.
playlist_status :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_status = Lens.lens (\Playlist' {status} -> status) (\s@Playlist' {} a -> s {status = a} :: Playlist)

-- | For each output in this job that you want to include in a master
-- playlist, the value of the Outputs:Key object.
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
playlist_outputKeys :: Lens.Lens' Playlist (Prelude.Maybe [Prelude.Text])
playlist_outputKeys = Lens.lens (\Playlist' {outputKeys} -> outputKeys) (\s@Playlist' {} a -> s {outputKeys = a} :: Playlist) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Playlist where
  parseJSON =
    Data.withObject
      "Playlist"
      ( \x ->
          Playlist'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "HlsContentProtection")
            Prelude.<*> (x Data..:? "PlayReadyDrm")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "StatusDetail")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "OutputKeys" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Playlist where
  hashWithSalt _salt Playlist' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` hlsContentProtection
      `Prelude.hashWithSalt` playReadyDrm
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` statusDetail
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` outputKeys

instance Prelude.NFData Playlist where
  rnf Playlist' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf hlsContentProtection
      `Prelude.seq` Prelude.rnf playReadyDrm
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf statusDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf outputKeys
