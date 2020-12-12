{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
  ( CreateJobPlaylist (..),

    -- * Smart constructor
    mkCreateJobPlaylist,

    -- * Lenses
    cjpPlayReadyDrm,
    cjpFormat,
    cjpOutputKeys,
    cjpName,
    cjpHlsContentProtection,
  )
where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the master playlist.
--
-- /See:/ 'mkCreateJobPlaylist' smart constructor.
data CreateJobPlaylist = CreateJobPlaylist'
  { playReadyDrm ::
      Lude.Maybe PlayReadyDrm,
    format :: Lude.Maybe Lude.Text,
    outputKeys :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateJobPlaylist' with the minimum fields required to make a request.
--
-- * 'format' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
-- * 'hlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
-- * 'name' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
-- * 'outputKeys' - For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.
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
mkCreateJobPlaylist ::
  CreateJobPlaylist
mkCreateJobPlaylist =
  CreateJobPlaylist'
    { playReadyDrm = Lude.Nothing,
      format = Lude.Nothing,
      outputKeys = Lude.Nothing,
      name = Lude.Nothing,
      hlsContentProtection = Lude.Nothing
    }

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'playReadyDrm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpPlayReadyDrm :: Lens.Lens' CreateJobPlaylist (Lude.Maybe PlayReadyDrm)
cjpPlayReadyDrm = Lens.lens (playReadyDrm :: CreateJobPlaylist -> Lude.Maybe PlayReadyDrm) (\s a -> s {playReadyDrm = a} :: CreateJobPlaylist)
{-# DEPRECATED cjpPlayReadyDrm "Use generic-lens or generic-optics with 'playReadyDrm' instead." #-}

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpFormat :: Lens.Lens' CreateJobPlaylist (Lude.Maybe Lude.Text)
cjpFormat = Lens.lens (format :: CreateJobPlaylist -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: CreateJobPlaylist)
{-# DEPRECATED cjpFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.
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
cjpOutputKeys :: Lens.Lens' CreateJobPlaylist (Lude.Maybe [Lude.Text])
cjpOutputKeys = Lens.lens (outputKeys :: CreateJobPlaylist -> Lude.Maybe [Lude.Text]) (\s a -> s {outputKeys = a} :: CreateJobPlaylist)
{-# DEPRECATED cjpOutputKeys "Use generic-lens or generic-optics with 'outputKeys' instead." #-}

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpName :: Lens.Lens' CreateJobPlaylist (Lude.Maybe Lude.Text)
cjpName = Lens.lens (name :: CreateJobPlaylist -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateJobPlaylist)
{-# DEPRECATED cjpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'hlsContentProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpHlsContentProtection :: Lens.Lens' CreateJobPlaylist (Lude.Maybe HlsContentProtection)
cjpHlsContentProtection = Lens.lens (hlsContentProtection :: CreateJobPlaylist -> Lude.Maybe HlsContentProtection) (\s a -> s {hlsContentProtection = a} :: CreateJobPlaylist)
{-# DEPRECATED cjpHlsContentProtection "Use generic-lens or generic-optics with 'hlsContentProtection' instead." #-}

instance Lude.ToJSON CreateJobPlaylist where
  toJSON CreateJobPlaylist' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayReadyDrm" Lude..=) Lude.<$> playReadyDrm,
            ("Format" Lude..=) Lude.<$> format,
            ("OutputKeys" Lude..=) Lude.<$> outputKeys,
            ("Name" Lude..=) Lude.<$> name,
            ("HlsContentProtection" Lude..=) Lude.<$> hlsContentProtection
          ]
      )
