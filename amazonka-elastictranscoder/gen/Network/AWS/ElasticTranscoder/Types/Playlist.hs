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
-- Module      : Network.AWS.ElasticTranscoder.Types.Playlist
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Playlist where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset
-- for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@
-- (MPEG-TS), Playlists contains information about the master playlists
-- that you want Elastic Transcoder to create. We recommend that you create
-- only one master playlist per output format. The maximum number of master
-- playlists in a job is 30.
--
-- /See:/ 'newPlaylist' smart constructor.
data Playlist = Playlist'
  { -- | The status of the job with which the playlist is associated.
    status :: Prelude.Maybe Prelude.Text,
    -- | The DRM settings, if any, that you want Elastic Transcoder to apply to
    -- the output files associated with this playlist.
    playReadyDrm :: Prelude.Maybe PlayReadyDrm,
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
    outputKeys :: Prelude.Maybe [Prelude.Text],
    -- | The format of the output playlist. Valid formats include @HLSv3@,
    -- @HLSv4@, and @Smooth@.
    format :: Prelude.Maybe Prelude.Text,
    -- | The HLS content protection settings, if any, that you want Elastic
    -- Transcoder to apply to the output files associated with this playlist.
    hlsContentProtection :: Prelude.Maybe HlsContentProtection,
    -- | Information that further explains the status.
    statusDetail :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'Playlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'playlist_status' - The status of the job with which the playlist is associated.
--
-- 'playReadyDrm', 'playlist_playReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
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
--
-- 'format', 'playlist_format' - The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
--
-- 'hlsContentProtection', 'playlist_hlsContentProtection' - The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
--
-- 'statusDetail', 'playlist_statusDetail' - Information that further explains the status.
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
newPlaylist ::
  Playlist
newPlaylist =
  Playlist'
    { status = Prelude.Nothing,
      playReadyDrm = Prelude.Nothing,
      outputKeys = Prelude.Nothing,
      format = Prelude.Nothing,
      hlsContentProtection = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The status of the job with which the playlist is associated.
playlist_status :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_status = Lens.lens (\Playlist' {status} -> status) (\s@Playlist' {} a -> s {status = a} :: Playlist)

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
playlist_playReadyDrm :: Lens.Lens' Playlist (Prelude.Maybe PlayReadyDrm)
playlist_playReadyDrm = Lens.lens (\Playlist' {playReadyDrm} -> playReadyDrm) (\s@Playlist' {} a -> s {playReadyDrm = a} :: Playlist)

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
playlist_outputKeys = Lens.lens (\Playlist' {outputKeys} -> outputKeys) (\s@Playlist' {} a -> s {outputKeys = a} :: Playlist) Prelude.. Lens.mapping Prelude._Coerce

-- | The format of the output playlist. Valid formats include @HLSv3@,
-- @HLSv4@, and @Smooth@.
playlist_format :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_format = Lens.lens (\Playlist' {format} -> format) (\s@Playlist' {} a -> s {format = a} :: Playlist)

-- | The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to the output files associated with this playlist.
playlist_hlsContentProtection :: Lens.Lens' Playlist (Prelude.Maybe HlsContentProtection)
playlist_hlsContentProtection = Lens.lens (\Playlist' {hlsContentProtection} -> hlsContentProtection) (\s@Playlist' {} a -> s {hlsContentProtection = a} :: Playlist)

-- | Information that further explains the status.
playlist_statusDetail :: Lens.Lens' Playlist (Prelude.Maybe Prelude.Text)
playlist_statusDetail = Lens.lens (\Playlist' {statusDetail} -> statusDetail) (\s@Playlist' {} a -> s {statusDetail = a} :: Playlist)

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

instance Prelude.FromJSON Playlist where
  parseJSON =
    Prelude.withObject
      "Playlist"
      ( \x ->
          Playlist'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "PlayReadyDrm")
            Prelude.<*> ( x Prelude..:? "OutputKeys"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Format")
            Prelude.<*> (x Prelude..:? "HlsContentProtection")
            Prelude.<*> (x Prelude..:? "StatusDetail")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable Playlist

instance Prelude.NFData Playlist
