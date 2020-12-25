{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Playlist
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Playlist
  ( Playlist (..),

    -- * Smart constructor
    mkPlaylist,

    -- * Lenses
    pFormat,
    pHlsContentProtection,
    pName,
    pOutputKeys,
    pPlayReadyDrm,
    pStatus,
    pStatusDetail,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Description as Types
import qualified Network.AWS.ElasticTranscoder.Types.HlsContentProtection as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobStatus as Types
import qualified Network.AWS.ElasticTranscoder.Types.Key as Types
import qualified Network.AWS.ElasticTranscoder.Types.Name as Types
import qualified Network.AWS.ElasticTranscoder.Types.PlayReadyDrm as Types
import qualified Network.AWS.ElasticTranscoder.Types.PlaylistFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30.
--
-- /See:/ 'mkPlaylist' smart constructor.
data Playlist = Playlist'
  { -- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
    format :: Core.Maybe Types.PlaylistFormat,
    -- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
    hlsContentProtection :: Core.Maybe Types.HlsContentProtection,
    -- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
    name :: Core.Maybe Types.Name,
    -- | For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.
    --
    --
    --     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ :
    -- OutputKeyPrefix@Outputs:Key@
    --
    --
    --     * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000:
    -- OutputKeyPrefix@Outputs:Key@ .m3u8
    -- OutputKeyPrefix@Outputs:Key@ 00000.ts
    --
    --
    --     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ :
    -- OutputKeyPrefix@Outputs:Key@ _v4.m3u8
    -- OutputKeyPrefix@Outputs:Key@ _iframe.m3u8
    -- OutputKeyPrefix@Outputs:Key@ .ts
    --
    --
    -- Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions.
    -- If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
    outputKeys :: Core.Maybe [Types.Key],
    -- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
    playReadyDrm :: Core.Maybe Types.PlayReadyDrm,
    -- | The status of the job with which the playlist is associated.
    status :: Core.Maybe Types.JobStatus,
    -- | Information that further explains the status.
    statusDetail :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Playlist' value with any optional fields omitted.
mkPlaylist ::
  Playlist
mkPlaylist =
  Playlist'
    { format = Core.Nothing,
      hlsContentProtection = Core.Nothing,
      name = Core.Nothing,
      outputKeys = Core.Nothing,
      playReadyDrm = Core.Nothing,
      status = Core.Nothing,
      statusDetail = Core.Nothing
    }

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pFormat :: Lens.Lens' Playlist (Core.Maybe Types.PlaylistFormat)
pFormat = Lens.field @"format"
{-# DEPRECATED pFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'hlsContentProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHlsContentProtection :: Lens.Lens' Playlist (Core.Maybe Types.HlsContentProtection)
pHlsContentProtection = Lens.field @"hlsContentProtection"
{-# DEPRECATED pHlsContentProtection "Use generic-lens or generic-optics with 'hlsContentProtection' instead." #-}

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Playlist (Core.Maybe Types.Name)
pName = Lens.field @"name"
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.
--
--
--     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ :
-- OutputKeyPrefix@Outputs:Key@
--
--
--     * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000:
-- OutputKeyPrefix@Outputs:Key@ .m3u8
-- OutputKeyPrefix@Outputs:Key@ 00000.ts
--
--
--     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ :
-- OutputKeyPrefix@Outputs:Key@ _v4.m3u8
-- OutputKeyPrefix@Outputs:Key@ _iframe.m3u8
-- OutputKeyPrefix@Outputs:Key@ .ts
--
--
-- Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions.
-- If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
--
-- /Note:/ Consider using 'outputKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pOutputKeys :: Lens.Lens' Playlist (Core.Maybe [Types.Key])
pOutputKeys = Lens.field @"outputKeys"
{-# DEPRECATED pOutputKeys "Use generic-lens or generic-optics with 'outputKeys' instead." #-}

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'playReadyDrm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayReadyDrm :: Lens.Lens' Playlist (Core.Maybe Types.PlayReadyDrm)
pPlayReadyDrm = Lens.field @"playReadyDrm"
{-# DEPRECATED pPlayReadyDrm "Use generic-lens or generic-optics with 'playReadyDrm' instead." #-}

-- | The status of the job with which the playlist is associated.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStatus :: Lens.Lens' Playlist (Core.Maybe Types.JobStatus)
pStatus = Lens.field @"status"
{-# DEPRECATED pStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information that further explains the status.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStatusDetail :: Lens.Lens' Playlist (Core.Maybe Types.Description)
pStatusDetail = Lens.field @"statusDetail"
{-# DEPRECATED pStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

instance Core.FromJSON Playlist where
  parseJSON =
    Core.withObject "Playlist" Core.$
      \x ->
        Playlist'
          Core.<$> (x Core..:? "Format")
          Core.<*> (x Core..:? "HlsContentProtection")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "OutputKeys")
          Core.<*> (x Core..:? "PlayReadyDrm")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetail")
