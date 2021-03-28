{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
  ( CreateJobPlaylist (..)
  -- * Smart constructor
  , mkCreateJobPlaylist
  -- * Lenses
  , cjpFormat
  , cjpHlsContentProtection
  , cjpName
  , cjpOutputKeys
  , cjpPlayReadyDrm
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Filename as Types
import qualified Network.AWS.ElasticTranscoder.Types.Format as Types
import qualified Network.AWS.ElasticTranscoder.Types.HlsContentProtection as Types
import qualified Network.AWS.ElasticTranscoder.Types.Key as Types
import qualified Network.AWS.ElasticTranscoder.Types.PlayReadyDrm as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the master playlist.
--
-- /See:/ 'mkCreateJobPlaylist' smart constructor.
data CreateJobPlaylist = CreateJobPlaylist'
  { format :: Core.Maybe Types.Format
    -- ^ The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
  , hlsContentProtection :: Core.Maybe Types.HlsContentProtection
    -- ^ The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
  , name :: Core.Maybe Types.Filename
    -- ^ The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
  , outputKeys :: Core.Maybe [Types.Key]
    -- ^ For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object. 
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
  , playReadyDrm :: Core.Maybe Types.PlayReadyDrm
    -- ^ The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobPlaylist' value with any optional fields omitted.
mkCreateJobPlaylist
    :: CreateJobPlaylist
mkCreateJobPlaylist
  = CreateJobPlaylist'{format = Core.Nothing,
                       hlsContentProtection = Core.Nothing, name = Core.Nothing,
                       outputKeys = Core.Nothing, playReadyDrm = Core.Nothing}

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpFormat :: Lens.Lens' CreateJobPlaylist (Core.Maybe Types.Format)
cjpFormat = Lens.field @"format"
{-# INLINEABLE cjpFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'hlsContentProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpHlsContentProtection :: Lens.Lens' CreateJobPlaylist (Core.Maybe Types.HlsContentProtection)
cjpHlsContentProtection = Lens.field @"hlsContentProtection"
{-# INLINEABLE cjpHlsContentProtection #-}
{-# DEPRECATED hlsContentProtection "Use generic-lens or generic-optics with 'hlsContentProtection' instead"  #-}

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpName :: Lens.Lens' CreateJobPlaylist (Core.Maybe Types.Filename)
cjpName = Lens.field @"name"
{-# INLINEABLE cjpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
cjpOutputKeys :: Lens.Lens' CreateJobPlaylist (Core.Maybe [Types.Key])
cjpOutputKeys = Lens.field @"outputKeys"
{-# INLINEABLE cjpOutputKeys #-}
{-# DEPRECATED outputKeys "Use generic-lens or generic-optics with 'outputKeys' instead"  #-}

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'playReadyDrm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjpPlayReadyDrm :: Lens.Lens' CreateJobPlaylist (Core.Maybe Types.PlayReadyDrm)
cjpPlayReadyDrm = Lens.field @"playReadyDrm"
{-# INLINEABLE cjpPlayReadyDrm #-}
{-# DEPRECATED playReadyDrm "Use generic-lens or generic-optics with 'playReadyDrm' instead"  #-}

instance Core.FromJSON CreateJobPlaylist where
        toJSON CreateJobPlaylist{..}
          = Core.object
              (Core.catMaybes
                 [("Format" Core..=) Core.<$> format,
                  ("HlsContentProtection" Core..=) Core.<$> hlsContentProtection,
                  ("Name" Core..=) Core.<$> name,
                  ("OutputKeys" Core..=) Core.<$> outputKeys,
                  ("PlayReadyDrm" Core..=) Core.<$> playReadyDrm])
