-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobInput
  ( JobInput (..),

    -- * Smart constructor
    mkJobInput,

    -- * Lenses
    jiFrameRate,
    jiResolution,
    jiAspectRatio,
    jiTimeSpan,
    jiEncryption,
    jiKey,
    jiDetectedProperties,
    jiContainer,
    jiInterlaced,
    jiInputCaptions,
  )
where

import Network.AWS.ElasticTranscoder.Types.DetectedProperties
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.InputCaptions
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the file that you're transcoding.
--
-- /See:/ 'mkJobInput' smart constructor.
data JobInput = JobInput'
  { frameRate :: Lude.Maybe Lude.Text,
    resolution :: Lude.Maybe Lude.Text,
    aspectRatio :: Lude.Maybe Lude.Text,
    timeSpan :: Lude.Maybe TimeSpan,
    encryption :: Lude.Maybe Encryption,
    key :: Lude.Maybe Lude.Text,
    detectedProperties :: Lude.Maybe DetectedProperties,
    container :: Lude.Maybe Lude.Text,
    interlaced :: Lude.Maybe Lude.Text,
    inputCaptions :: Lude.Maybe InputCaptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobInput' with the minimum fields required to make a request.
--
-- * 'aspectRatio' - The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:
--
-- @1:1@ , @4:3@ , @3:2@ , @16:9@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
-- * 'container' - The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:
--
-- @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
-- * 'detectedProperties' - The detected properties of the input file.
-- * 'encryption' - The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
-- * 'frameRate' - The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:
--
-- @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
-- * 'inputCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
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
-- * 'interlaced' - Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values:
--
-- @true@ , @false@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
-- * 'key' - The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.
--
-- If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
-- * 'resolution' - This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
-- * 'timeSpan' - Settings for clipping an input. Each input can have different clip settings.
mkJobInput ::
  JobInput
mkJobInput =
  JobInput'
    { frameRate = Lude.Nothing,
      resolution = Lude.Nothing,
      aspectRatio = Lude.Nothing,
      timeSpan = Lude.Nothing,
      encryption = Lude.Nothing,
      key = Lude.Nothing,
      detectedProperties = Lude.Nothing,
      container = Lude.Nothing,
      interlaced = Lude.Nothing,
      inputCaptions = Lude.Nothing
    }

-- | The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:
--
-- @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiFrameRate :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiFrameRate = Lens.lens (frameRate :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {frameRate = a} :: JobInput)
{-# DEPRECATED jiFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiResolution :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiResolution = Lens.lens (resolution :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {resolution = a} :: JobInput)
{-# DEPRECATED jiResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:
--
-- @1:1@ , @4:3@ , @3:2@ , @16:9@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
--
-- /Note:/ Consider using 'aspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiAspectRatio :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiAspectRatio = Lens.lens (aspectRatio :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {aspectRatio = a} :: JobInput)
{-# DEPRECATED jiAspectRatio "Use generic-lens or generic-optics with 'aspectRatio' instead." #-}

-- | Settings for clipping an input. Each input can have different clip settings.
--
-- /Note:/ Consider using 'timeSpan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiTimeSpan :: Lens.Lens' JobInput (Lude.Maybe TimeSpan)
jiTimeSpan = Lens.lens (timeSpan :: JobInput -> Lude.Maybe TimeSpan) (\s a -> s {timeSpan = a} :: JobInput)
{-# DEPRECATED jiTimeSpan "Use generic-lens or generic-optics with 'timeSpan' instead." #-}

-- | The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiEncryption :: Lens.Lens' JobInput (Lude.Maybe Encryption)
jiEncryption = Lens.lens (encryption :: JobInput -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: JobInput)
{-# DEPRECATED jiEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.
--
-- If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiKey :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiKey = Lens.lens (key :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: JobInput)
{-# DEPRECATED jiKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The detected properties of the input file.
--
-- /Note:/ Consider using 'detectedProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiDetectedProperties :: Lens.Lens' JobInput (Lude.Maybe DetectedProperties)
jiDetectedProperties = Lens.lens (detectedProperties :: JobInput -> Lude.Maybe DetectedProperties) (\s a -> s {detectedProperties = a} :: JobInput)
{-# DEPRECATED jiDetectedProperties "Use generic-lens or generic-optics with 'detectedProperties' instead." #-}

-- | The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:
--
-- @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiContainer :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiContainer = Lens.lens (container :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {container = a} :: JobInput)
{-# DEPRECATED jiContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values:
--
-- @true@ , @false@
-- If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
--
-- /Note:/ Consider using 'interlaced' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiInterlaced :: Lens.Lens' JobInput (Lude.Maybe Lude.Text)
jiInterlaced = Lens.lens (interlaced :: JobInput -> Lude.Maybe Lude.Text) (\s a -> s {interlaced = a} :: JobInput)
{-# DEPRECATED jiInterlaced "Use generic-lens or generic-optics with 'interlaced' instead." #-}

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
jiInputCaptions :: Lens.Lens' JobInput (Lude.Maybe InputCaptions)
jiInputCaptions = Lens.lens (inputCaptions :: JobInput -> Lude.Maybe InputCaptions) (\s a -> s {inputCaptions = a} :: JobInput)
{-# DEPRECATED jiInputCaptions "Use generic-lens or generic-optics with 'inputCaptions' instead." #-}

instance Lude.FromJSON JobInput where
  parseJSON =
    Lude.withObject
      "JobInput"
      ( \x ->
          JobInput'
            Lude.<$> (x Lude..:? "FrameRate")
            Lude.<*> (x Lude..:? "Resolution")
            Lude.<*> (x Lude..:? "AspectRatio")
            Lude.<*> (x Lude..:? "TimeSpan")
            Lude.<*> (x Lude..:? "Encryption")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "DetectedProperties")
            Lude.<*> (x Lude..:? "Container")
            Lude.<*> (x Lude..:? "Interlaced")
            Lude.<*> (x Lude..:? "InputCaptions")
      )

instance Lude.ToJSON JobInput where
  toJSON JobInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FrameRate" Lude..=) Lude.<$> frameRate,
            ("Resolution" Lude..=) Lude.<$> resolution,
            ("AspectRatio" Lude..=) Lude.<$> aspectRatio,
            ("TimeSpan" Lude..=) Lude.<$> timeSpan,
            ("Encryption" Lude..=) Lude.<$> encryption,
            ("Key" Lude..=) Lude.<$> key,
            ("DetectedProperties" Lude..=) Lude.<$> detectedProperties,
            ("Container" Lude..=) Lude.<$> container,
            ("Interlaced" Lude..=) Lude.<$> interlaced,
            ("InputCaptions" Lude..=) Lude.<$> inputCaptions
          ]
      )
