{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.StartTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to transcribe speech to text.
module Network.AWS.Transcribe.StartTranscriptionJob
  ( -- * Creating a request
    StartTranscriptionJob (..),
    mkStartTranscriptionJob,

    -- ** Request lenses
    stjTranscriptionJobName,
    stjMedia,
    stjContentRedaction,
    stjIdentifyLanguage,
    stjJobExecutionSettings,
    stjLanguageCode,
    stjLanguageOptions,
    stjMediaFormat,
    stjMediaSampleRateHertz,
    stjModelSettings,
    stjOutputBucketName,
    stjOutputEncryptionKMSKeyId,
    stjOutputKey,
    stjSettings,

    -- * Destructuring the response
    StartTranscriptionJobResponse (..),
    mkStartTranscriptionJobResponse,

    -- ** Response lenses
    stjrrsTranscriptionJob,
    stjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkStartTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { -- | The name of the job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job, you get a @ConflictException@ error.
    transcriptionJobName :: Types.TranscriptionJobName,
    -- | An object that describes the input media for a transcription job.
    media :: Types.Media,
    -- | An object that contains the request parameters for content redaction.
    contentRedaction :: Core.Maybe Types.ContentRedaction,
    -- | Set this field to @true@ to enable automatic language identification. Automatic language identification is disabled by default. You receive a @BadRequestException@ error if you enter a value for a @LanguageCode@ .
    identifyLanguage :: Core.Maybe Core.Bool,
    -- | Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
    jobExecutionSettings :: Core.Maybe Types.JobExecutionSettings,
    -- | The language code for the language used in the input media file.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | An object containing a list of languages that might be present in your collection of audio files. Automatic language identification chooses a language that best matches the source audio from that list.
    languageOptions :: Core.Maybe (Core.NonEmpty Types.LanguageCode),
    -- | The format of the input media file.
    mediaFormat :: Core.Maybe Types.MediaFormat,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    --
    -- If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
    mediaSampleRateHertz :: Core.Maybe Core.Natural,
    -- | Choose the custom language model you use for your transcription job in this parameter.
    modelSettings :: Core.Maybe Types.ModelSettings,
    -- | The location where the transcription is stored.
    --
    -- If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
    -- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
    -- If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
    outputBucketName :: Core.Maybe Types.OutputBucketName,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the @StartTranscriptionJob@ operation must have permission to use the specified KMS key.
    --
    -- You can use either of the following to identify a KMS key in the current account:
    --
    --     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"
    --
    --
    --     * KMS Key Alias: "alias/ExampleAlias"
    --
    --
    -- You can use either of the following to identify a KMS key in the current account or another account:
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"
    --
    --
    --     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias"
    --
    --
    -- If you don't specify an encryption key, the output of the transcription job is encrypted with the default Amazon S3 key (SSE-S3).
    -- If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
    outputEncryptionKMSKeyId :: Core.Maybe Types.KMSKeyId,
    -- | You can specify a location in an Amazon S3 bucket to store the output of your transcription job.
    --
    -- If you don't specify an output key, Amazon Transcribe stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
    -- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
    -- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
    outputKey :: Core.Maybe Types.OutputKey,
    -- | A @Settings@ object that provides optional settings for a transcription job.
    settings :: Core.Maybe Types.Settings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTranscriptionJob' value with any optional fields omitted.
mkStartTranscriptionJob ::
  -- | 'transcriptionJobName'
  Types.TranscriptionJobName ->
  -- | 'media'
  Types.Media ->
  StartTranscriptionJob
mkStartTranscriptionJob transcriptionJobName media =
  StartTranscriptionJob'
    { transcriptionJobName,
      media,
      contentRedaction = Core.Nothing,
      identifyLanguage = Core.Nothing,
      jobExecutionSettings = Core.Nothing,
      languageCode = Core.Nothing,
      languageOptions = Core.Nothing,
      mediaFormat = Core.Nothing,
      mediaSampleRateHertz = Core.Nothing,
      modelSettings = Core.Nothing,
      outputBucketName = Core.Nothing,
      outputEncryptionKMSKeyId = Core.Nothing,
      outputKey = Core.Nothing,
      settings = Core.Nothing
    }

-- | The name of the job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTranscriptionJobName :: Lens.Lens' StartTranscriptionJob Types.TranscriptionJobName
stjTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# DEPRECATED stjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

-- | An object that describes the input media for a transcription job.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMedia :: Lens.Lens' StartTranscriptionJob Types.Media
stjMedia = Lens.field @"media"
{-# DEPRECATED stjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | An object that contains the request parameters for content redaction.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjContentRedaction :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.ContentRedaction)
stjContentRedaction = Lens.field @"contentRedaction"
{-# DEPRECATED stjContentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead." #-}

-- | Set this field to @true@ to enable automatic language identification. Automatic language identification is disabled by default. You receive a @BadRequestException@ error if you enter a value for a @LanguageCode@ .
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjIdentifyLanguage :: Lens.Lens' StartTranscriptionJob (Core.Maybe Core.Bool)
stjIdentifyLanguage = Lens.field @"identifyLanguage"
{-# DEPRECATED stjIdentifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead." #-}

-- | Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
--
-- /Note:/ Consider using 'jobExecutionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjJobExecutionSettings :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.JobExecutionSettings)
stjJobExecutionSettings = Lens.field @"jobExecutionSettings"
{-# DEPRECATED stjJobExecutionSettings "Use generic-lens or generic-optics with 'jobExecutionSettings' instead." #-}

-- | The language code for the language used in the input media file.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjLanguageCode :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.LanguageCode)
stjLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED stjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | An object containing a list of languages that might be present in your collection of audio files. Automatic language identification chooses a language that best matches the source audio from that list.
--
-- /Note:/ Consider using 'languageOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjLanguageOptions :: Lens.Lens' StartTranscriptionJob (Core.Maybe (Core.NonEmpty Types.LanguageCode))
stjLanguageOptions = Lens.field @"languageOptions"
{-# DEPRECATED stjLanguageOptions "Use generic-lens or generic-optics with 'languageOptions' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMediaFormat :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.MediaFormat)
stjMediaFormat = Lens.field @"mediaFormat"
{-# DEPRECATED stjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMediaSampleRateHertz :: Lens.Lens' StartTranscriptionJob (Core.Maybe Core.Natural)
stjMediaSampleRateHertz = Lens.field @"mediaSampleRateHertz"
{-# DEPRECATED stjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

-- | Choose the custom language model you use for your transcription job in this parameter.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjModelSettings :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.ModelSettings)
stjModelSettings = Lens.field @"modelSettings"
{-# DEPRECATED stjModelSettings "Use generic-lens or generic-optics with 'modelSettings' instead." #-}

-- | The location where the transcription is stored.
--
-- If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
-- If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
--
-- /Note:/ Consider using 'outputBucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjOutputBucketName :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.OutputBucketName)
stjOutputBucketName = Lens.field @"outputBucketName"
{-# DEPRECATED stjOutputBucketName "Use generic-lens or generic-optics with 'outputBucketName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the @StartTranscriptionJob@ operation must have permission to use the specified KMS key.
--
-- You can use either of the following to identify a KMS key in the current account:
--
--     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * KMS Key Alias: "alias/ExampleAlias"
--
--
-- You can use either of the following to identify a KMS key in the current account or another account:
--
--     * Amazon Resource Name (ARN) of a KMS Key: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias"
--
--
-- If you don't specify an encryption key, the output of the transcription job is encrypted with the default Amazon S3 key (SSE-S3).
-- If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjOutputEncryptionKMSKeyId :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.KMSKeyId)
stjOutputEncryptionKMSKeyId = Lens.field @"outputEncryptionKMSKeyId"
{-# DEPRECATED stjOutputEncryptionKMSKeyId "Use generic-lens or generic-optics with 'outputEncryptionKMSKeyId' instead." #-}

-- | You can specify a location in an Amazon S3 bucket to store the output of your transcription job.
--
-- If you don't specify an output key, Amazon Transcribe stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjOutputKey :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.OutputKey)
stjOutputKey = Lens.field @"outputKey"
{-# DEPRECATED stjOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | A @Settings@ object that provides optional settings for a transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjSettings :: Lens.Lens' StartTranscriptionJob (Core.Maybe Types.Settings)
stjSettings = Lens.field @"settings"
{-# DEPRECATED stjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

instance Core.FromJSON StartTranscriptionJob where
  toJSON StartTranscriptionJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TranscriptionJobName" Core..= transcriptionJobName),
            Core.Just ("Media" Core..= media),
            ("ContentRedaction" Core..=) Core.<$> contentRedaction,
            ("IdentifyLanguage" Core..=) Core.<$> identifyLanguage,
            ("JobExecutionSettings" Core..=) Core.<$> jobExecutionSettings,
            ("LanguageCode" Core..=) Core.<$> languageCode,
            ("LanguageOptions" Core..=) Core.<$> languageOptions,
            ("MediaFormat" Core..=) Core.<$> mediaFormat,
            ("MediaSampleRateHertz" Core..=) Core.<$> mediaSampleRateHertz,
            ("ModelSettings" Core..=) Core.<$> modelSettings,
            ("OutputBucketName" Core..=) Core.<$> outputBucketName,
            ("OutputEncryptionKMSKeyId" Core..=)
              Core.<$> outputEncryptionKMSKeyId,
            ("OutputKey" Core..=) Core.<$> outputKey,
            ("Settings" Core..=) Core.<$> settings
          ]
      )

instance Core.AWSRequest StartTranscriptionJob where
  type Rs StartTranscriptionJob = StartTranscriptionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.StartTranscriptionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTranscriptionJobResponse'
            Core.<$> (x Core..:? "TranscriptionJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'
  { -- | An object containing details of the asynchronous transcription job.
    transcriptionJob :: Core.Maybe Types.TranscriptionJob,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartTranscriptionJobResponse' value with any optional fields omitted.
mkStartTranscriptionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartTranscriptionJobResponse
mkStartTranscriptionJobResponse responseStatus =
  StartTranscriptionJobResponse'
    { transcriptionJob = Core.Nothing,
      responseStatus
    }

-- | An object containing details of the asynchronous transcription job.
--
-- /Note:/ Consider using 'transcriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjrrsTranscriptionJob :: Lens.Lens' StartTranscriptionJobResponse (Core.Maybe Types.TranscriptionJob)
stjrrsTranscriptionJob = Lens.field @"transcriptionJob"
{-# DEPRECATED stjrrsTranscriptionJob "Use generic-lens or generic-optics with 'transcriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjrrsResponseStatus :: Lens.Lens' StartTranscriptionJobResponse Core.Int
stjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED stjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
