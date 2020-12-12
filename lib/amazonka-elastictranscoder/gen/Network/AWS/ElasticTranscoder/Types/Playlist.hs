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
    pStatus,
    pPlayReadyDrm,
    pFormat,
    pOutputKeys,
    pName,
    pStatusDetail,
    pHlsContentProtection,
  )
where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30.
--
-- /See:/ 'mkPlaylist' smart constructor.
data Playlist = Playlist'
  { status :: Lude.Maybe Lude.Text,
    playReadyDrm :: Lude.Maybe PlayReadyDrm,
    format :: Lude.Maybe Lude.Text,
    outputKeys :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    statusDetail :: Lude.Maybe Lude.Text,
    hlsContentProtection :: Lude.Maybe HlsContentProtection
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Playlist' with the minimum fields required to make a request.
--
-- * 'format' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
-- * 'hlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
-- * 'name' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
-- * 'outputKeys' - For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.
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
-- * 'playReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
-- * 'status' - The status of the job with which the playlist is associated.
-- * 'statusDetail' - Information that further explains the status.
mkPlaylist ::
  Playlist
mkPlaylist =
  Playlist'
    { status = Lude.Nothing,
      playReadyDrm = Lude.Nothing,
      format = Lude.Nothing,
      outputKeys = Lude.Nothing,
      name = Lude.Nothing,
      statusDetail = Lude.Nothing,
      hlsContentProtection = Lude.Nothing
    }

-- | The status of the job with which the playlist is associated.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStatus :: Lens.Lens' Playlist (Lude.Maybe Lude.Text)
pStatus = Lens.lens (status :: Playlist -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Playlist)
{-# DEPRECATED pStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'playReadyDrm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPlayReadyDrm :: Lens.Lens' Playlist (Lude.Maybe PlayReadyDrm)
pPlayReadyDrm = Lens.lens (playReadyDrm :: Playlist -> Lude.Maybe PlayReadyDrm) (\s a -> s {playReadyDrm = a} :: Playlist)
{-# DEPRECATED pPlayReadyDrm "Use generic-lens or generic-optics with 'playReadyDrm' instead." #-}

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pFormat :: Lens.Lens' Playlist (Lude.Maybe Lude.Text)
pFormat = Lens.lens (format :: Playlist -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: Playlist)
{-# DEPRECATED pFormat "Use generic-lens or generic-optics with 'format' instead." #-}

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
pOutputKeys :: Lens.Lens' Playlist (Lude.Maybe [Lude.Text])
pOutputKeys = Lens.lens (outputKeys :: Playlist -> Lude.Maybe [Lude.Text]) (\s a -> s {outputKeys = a} :: Playlist)
{-# DEPRECATED pOutputKeys "Use generic-lens or generic-optics with 'outputKeys' instead." #-}

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Playlist (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Playlist -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Playlist)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information that further explains the status.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStatusDetail :: Lens.Lens' Playlist (Lude.Maybe Lude.Text)
pStatusDetail = Lens.lens (statusDetail :: Playlist -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: Playlist)
{-# DEPRECATED pStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'hlsContentProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHlsContentProtection :: Lens.Lens' Playlist (Lude.Maybe HlsContentProtection)
pHlsContentProtection = Lens.lens (hlsContentProtection :: Playlist -> Lude.Maybe HlsContentProtection) (\s a -> s {hlsContentProtection = a} :: Playlist)
{-# DEPRECATED pHlsContentProtection "Use generic-lens or generic-optics with 'hlsContentProtection' instead." #-}

instance Lude.FromJSON Playlist where
  parseJSON =
    Lude.withObject
      "Playlist"
      ( \x ->
          Playlist'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PlayReadyDrm")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "OutputKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "StatusDetail")
            Lude.<*> (x Lude..:? "HlsContentProtection")
      )
