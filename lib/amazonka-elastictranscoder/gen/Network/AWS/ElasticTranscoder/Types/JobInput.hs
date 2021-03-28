{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.JobInput
  ( JobInput (..)
  -- * Smart constructor
  , mkJobInput
  -- * Lenses
  , jiAspectRatio
  , jiContainer
  , jiDetectedProperties
  , jiEncryption
  , jiFrameRate
  , jiInputCaptions
  , jiInterlaced
  , jiKey
  , jiResolution
  , jiTimeSpan
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.AspectRatio as Types
import qualified Network.AWS.ElasticTranscoder.Types.DetectedProperties as Types
import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.FrameRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.InputCaptions as Types
import qualified Network.AWS.ElasticTranscoder.Types.Interlaced as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobContainer as Types
import qualified Network.AWS.ElasticTranscoder.Types.LongKey as Types
import qualified Network.AWS.ElasticTranscoder.Types.Resolution as Types
import qualified Network.AWS.ElasticTranscoder.Types.TimeSpan as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the file that you're transcoding.
--
-- /See:/ 'mkJobInput' smart constructor.
data JobInput = JobInput'
  { aspectRatio :: Core.Maybe Types.AspectRatio
    -- ^ The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values: 
--
-- @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio. 
  , container :: Core.Maybe Types.JobContainer
    -- ^ The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values: 
--
-- @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@ 
  , detectedProperties :: Core.Maybe Types.DetectedProperties
    -- ^ The detected properties of the input file.
  , encryption :: Core.Maybe Types.Encryption
    -- ^ The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
  , frameRate :: Core.Maybe Types.FrameRate
    -- ^ The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values: 
--
-- @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
  , inputCaptions :: Core.Maybe Types.InputCaptions
    -- ^ You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
--
--
--     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file.
-- Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@ 
-- Valid outputs include: @mov-text@ 
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
--
--     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file.
-- Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@ 
-- Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ .
--
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
-- Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process.
-- To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array.
-- For more information on embedded files, see the Subtitles Wikipedia page.
-- For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
  , interlaced :: Core.Maybe Types.Interlaced
    -- ^ Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values:
--
-- @true@ , @false@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
  , key :: Core.Maybe Types.LongKey
    -- ^ The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from. 
--
-- If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
  , resolution :: Core.Maybe Types.Resolution
    -- ^ This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
  , timeSpan :: Core.Maybe Types.TimeSpan
    -- ^ Settings for clipping an input. Each input can have different clip settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobInput' value with any optional fields omitted.
mkJobInput
    :: JobInput
mkJobInput
  = JobInput'{aspectRatio = Core.Nothing, container = Core.Nothing,
              detectedProperties = Core.Nothing, encryption = Core.Nothing,
              frameRate = Core.Nothing, inputCaptions = Core.Nothing,
              interlaced = Core.Nothing, key = Core.Nothing,
              resolution = Core.Nothing, timeSpan = Core.Nothing}

-- | The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values: 
--
-- @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio. 
--
-- /Note:/ Consider using 'aspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiAspectRatio :: Lens.Lens' JobInput (Core.Maybe Types.AspectRatio)
jiAspectRatio = Lens.field @"aspectRatio"
{-# INLINEABLE jiAspectRatio #-}
{-# DEPRECATED aspectRatio "Use generic-lens or generic-optics with 'aspectRatio' instead"  #-}

-- | The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values: 
--
-- @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@ 
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiContainer :: Lens.Lens' JobInput (Core.Maybe Types.JobContainer)
jiContainer = Lens.field @"container"
{-# INLINEABLE jiContainer #-}
{-# DEPRECATED container "Use generic-lens or generic-optics with 'container' instead"  #-}

-- | The detected properties of the input file.
--
-- /Note:/ Consider using 'detectedProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiDetectedProperties :: Lens.Lens' JobInput (Core.Maybe Types.DetectedProperties)
jiDetectedProperties = Lens.field @"detectedProperties"
{-# INLINEABLE jiDetectedProperties #-}
{-# DEPRECATED detectedProperties "Use generic-lens or generic-optics with 'detectedProperties' instead"  #-}

-- | The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiEncryption :: Lens.Lens' JobInput (Core.Maybe Types.Encryption)
jiEncryption = Lens.field @"encryption"
{-# INLINEABLE jiEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values: 
--
-- @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiFrameRate :: Lens.Lens' JobInput (Core.Maybe Types.FrameRate)
jiFrameRate = Lens.field @"frameRate"
{-# INLINEABLE jiFrameRate #-}
{-# DEPRECATED frameRate "Use generic-lens or generic-optics with 'frameRate' instead"  #-}

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
--
--
--     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file.
-- Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@ 
-- Valid outputs include: @mov-text@ 
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
--
--     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file.
-- Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@ 
-- Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ .
--
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
-- Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process.
-- To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array.
-- For more information on embedded files, see the Subtitles Wikipedia page.
-- For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- /Note:/ Consider using 'inputCaptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiInputCaptions :: Lens.Lens' JobInput (Core.Maybe Types.InputCaptions)
jiInputCaptions = Lens.field @"inputCaptions"
{-# INLINEABLE jiInputCaptions #-}
{-# DEPRECATED inputCaptions "Use generic-lens or generic-optics with 'inputCaptions' instead"  #-}

-- | Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values:
--
-- @true@ , @false@ 
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
--
-- /Note:/ Consider using 'interlaced' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiInterlaced :: Lens.Lens' JobInput (Core.Maybe Types.Interlaced)
jiInterlaced = Lens.field @"interlaced"
{-# INLINEABLE jiInterlaced #-}
{-# DEPRECATED interlaced "Use generic-lens or generic-optics with 'interlaced' instead"  #-}

-- | The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from. 
--
-- If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiKey :: Lens.Lens' JobInput (Core.Maybe Types.LongKey)
jiKey = Lens.field @"key"
{-# INLINEABLE jiKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiResolution :: Lens.Lens' JobInput (Core.Maybe Types.Resolution)
jiResolution = Lens.field @"resolution"
{-# INLINEABLE jiResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Settings for clipping an input. Each input can have different clip settings.
--
-- /Note:/ Consider using 'timeSpan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiTimeSpan :: Lens.Lens' JobInput (Core.Maybe Types.TimeSpan)
jiTimeSpan = Lens.field @"timeSpan"
{-# INLINEABLE jiTimeSpan #-}
{-# DEPRECATED timeSpan "Use generic-lens or generic-optics with 'timeSpan' instead"  #-}

instance Core.FromJSON JobInput where
        toJSON JobInput{..}
          = Core.object
              (Core.catMaybes
                 [("AspectRatio" Core..=) Core.<$> aspectRatio,
                  ("Container" Core..=) Core.<$> container,
                  ("DetectedProperties" Core..=) Core.<$> detectedProperties,
                  ("Encryption" Core..=) Core.<$> encryption,
                  ("FrameRate" Core..=) Core.<$> frameRate,
                  ("InputCaptions" Core..=) Core.<$> inputCaptions,
                  ("Interlaced" Core..=) Core.<$> interlaced,
                  ("Key" Core..=) Core.<$> key,
                  ("Resolution" Core..=) Core.<$> resolution,
                  ("TimeSpan" Core..=) Core.<$> timeSpan])

instance Core.FromJSON JobInput where
        parseJSON
          = Core.withObject "JobInput" Core.$
              \ x ->
                JobInput' Core.<$>
                  (x Core..:? "AspectRatio") Core.<*> x Core..:? "Container" Core.<*>
                    x Core..:? "DetectedProperties"
                    Core.<*> x Core..:? "Encryption"
                    Core.<*> x Core..:? "FrameRate"
                    Core.<*> x Core..:? "InputCaptions"
                    Core.<*> x Core..:? "Interlaced"
                    Core.<*> x Core..:? "Key"
                    Core.<*> x Core..:? "Resolution"
                    Core.<*> x Core..:? "TimeSpan"
