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
    stjContentRedaction,
    stjLanguageCode,
    stjLanguageOptions,
    stjSettings,
    stjOutputBucketName,
    stjMedia,
    stjMediaFormat,
    stjOutputEncryptionKMSKeyId,
    stjModelSettings,
    stjJobExecutionSettings,
    stjOutputKey,
    stjTranscriptionJobName,
    stjIdentifyLanguage,
    stjMediaSampleRateHertz,

    -- * Destructuring the response
    StartTranscriptionJobResponse (..),
    mkStartTranscriptionJobResponse,

    -- ** Response lenses
    stjrsTranscriptionJob,
    stjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkStartTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { -- | An object that contains the request parameters for content redaction.
    contentRedaction :: Lude.Maybe ContentRedaction,
    -- | The language code for the language used in the input media file.
    languageCode :: Lude.Maybe LanguageCode,
    -- | An object containing a list of languages that might be present in your collection of audio files. Automatic language identification chooses a language that best matches the source audio from that list.
    languageOptions :: Lude.Maybe (Lude.NonEmpty LanguageCode),
    -- | A @Settings@ object that provides optional settings for a transcription job.
    settings :: Lude.Maybe Settings,
    -- | The location where the transcription is stored.
    --
    -- If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
    -- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
    -- If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
    outputBucketName :: Lude.Maybe Lude.Text,
    -- | An object that describes the input media for a transcription job.
    media :: Media,
    -- | The format of the input media file.
    mediaFormat :: Lude.Maybe MediaFormat,
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
    outputEncryptionKMSKeyId :: Lude.Maybe Lude.Text,
    -- | Choose the custom language model you use for your transcription job in this parameter.
    modelSettings :: Lude.Maybe ModelSettings,
    -- | Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
    jobExecutionSettings :: Lude.Maybe JobExecutionSettings,
    -- | You can specify a location in an Amazon S3 bucket to store the output of your transcription job.
    --
    -- If you don't specify an output key, Amazon Transcribe stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
    -- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
    -- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
    outputKey :: Lude.Maybe Lude.Text,
    -- | The name of the job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job, you get a @ConflictException@ error.
    transcriptionJobName :: Lude.Text,
    -- | Set this field to @true@ to enable automatic language identification. Automatic language identification is disabled by default. You receive a @BadRequestException@ error if you enter a value for a @LanguageCode@ .
    identifyLanguage :: Lude.Maybe Lude.Bool,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    --
    -- If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
    mediaSampleRateHertz :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'contentRedaction' - An object that contains the request parameters for content redaction.
-- * 'languageCode' - The language code for the language used in the input media file.
-- * 'languageOptions' - An object containing a list of languages that might be present in your collection of audio files. Automatic language identification chooses a language that best matches the source audio from that list.
-- * 'settings' - A @Settings@ object that provides optional settings for a transcription job.
-- * 'outputBucketName' - The location where the transcription is stored.
--
-- If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
-- If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
-- * 'media' - An object that describes the input media for a transcription job.
-- * 'mediaFormat' - The format of the input media file.
-- * 'outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the @StartTranscriptionJob@ operation must have permission to use the specified KMS key.
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
-- * 'modelSettings' - Choose the custom language model you use for your transcription job in this parameter.
-- * 'jobExecutionSettings' - Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
-- * 'outputKey' - You can specify a location in an Amazon S3 bucket to store the output of your transcription job.
--
-- If you don't specify an output key, Amazon Transcribe stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
-- * 'transcriptionJobName' - The name of the job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job, you get a @ConflictException@ error.
-- * 'identifyLanguage' - Set this field to @true@ to enable automatic language identification. Automatic language identification is disabled by default. You receive a @BadRequestException@ error if you enter a value for a @LanguageCode@ .
-- * 'mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
mkStartTranscriptionJob ::
  -- | 'media'
  Media ->
  -- | 'transcriptionJobName'
  Lude.Text ->
  StartTranscriptionJob
mkStartTranscriptionJob pMedia_ pTranscriptionJobName_ =
  StartTranscriptionJob'
    { contentRedaction = Lude.Nothing,
      languageCode = Lude.Nothing,
      languageOptions = Lude.Nothing,
      settings = Lude.Nothing,
      outputBucketName = Lude.Nothing,
      media = pMedia_,
      mediaFormat = Lude.Nothing,
      outputEncryptionKMSKeyId = Lude.Nothing,
      modelSettings = Lude.Nothing,
      jobExecutionSettings = Lude.Nothing,
      outputKey = Lude.Nothing,
      transcriptionJobName = pTranscriptionJobName_,
      identifyLanguage = Lude.Nothing,
      mediaSampleRateHertz = Lude.Nothing
    }

-- | An object that contains the request parameters for content redaction.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjContentRedaction :: Lens.Lens' StartTranscriptionJob (Lude.Maybe ContentRedaction)
stjContentRedaction = Lens.lens (contentRedaction :: StartTranscriptionJob -> Lude.Maybe ContentRedaction) (\s a -> s {contentRedaction = a} :: StartTranscriptionJob)
{-# DEPRECATED stjContentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead." #-}

-- | The language code for the language used in the input media file.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjLanguageCode :: Lens.Lens' StartTranscriptionJob (Lude.Maybe LanguageCode)
stjLanguageCode = Lens.lens (languageCode :: StartTranscriptionJob -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: StartTranscriptionJob)
{-# DEPRECATED stjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | An object containing a list of languages that might be present in your collection of audio files. Automatic language identification chooses a language that best matches the source audio from that list.
--
-- /Note:/ Consider using 'languageOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjLanguageOptions :: Lens.Lens' StartTranscriptionJob (Lude.Maybe (Lude.NonEmpty LanguageCode))
stjLanguageOptions = Lens.lens (languageOptions :: StartTranscriptionJob -> Lude.Maybe (Lude.NonEmpty LanguageCode)) (\s a -> s {languageOptions = a} :: StartTranscriptionJob)
{-# DEPRECATED stjLanguageOptions "Use generic-lens or generic-optics with 'languageOptions' instead." #-}

-- | A @Settings@ object that provides optional settings for a transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjSettings :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Settings)
stjSettings = Lens.lens (settings :: StartTranscriptionJob -> Lude.Maybe Settings) (\s a -> s {settings = a} :: StartTranscriptionJob)
{-# DEPRECATED stjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The location where the transcription is stored.
--
-- If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
-- If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
--
-- /Note:/ Consider using 'outputBucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjOutputBucketName :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Lude.Text)
stjOutputBucketName = Lens.lens (outputBucketName :: StartTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {outputBucketName = a} :: StartTranscriptionJob)
{-# DEPRECATED stjOutputBucketName "Use generic-lens or generic-optics with 'outputBucketName' instead." #-}

-- | An object that describes the input media for a transcription job.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMedia :: Lens.Lens' StartTranscriptionJob Media
stjMedia = Lens.lens (media :: StartTranscriptionJob -> Media) (\s a -> s {media = a} :: StartTranscriptionJob)
{-# DEPRECATED stjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMediaFormat :: Lens.Lens' StartTranscriptionJob (Lude.Maybe MediaFormat)
stjMediaFormat = Lens.lens (mediaFormat :: StartTranscriptionJob -> Lude.Maybe MediaFormat) (\s a -> s {mediaFormat = a} :: StartTranscriptionJob)
{-# DEPRECATED stjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

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
stjOutputEncryptionKMSKeyId :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Lude.Text)
stjOutputEncryptionKMSKeyId = Lens.lens (outputEncryptionKMSKeyId :: StartTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {outputEncryptionKMSKeyId = a} :: StartTranscriptionJob)
{-# DEPRECATED stjOutputEncryptionKMSKeyId "Use generic-lens or generic-optics with 'outputEncryptionKMSKeyId' instead." #-}

-- | Choose the custom language model you use for your transcription job in this parameter.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjModelSettings :: Lens.Lens' StartTranscriptionJob (Lude.Maybe ModelSettings)
stjModelSettings = Lens.lens (modelSettings :: StartTranscriptionJob -> Lude.Maybe ModelSettings) (\s a -> s {modelSettings = a} :: StartTranscriptionJob)
{-# DEPRECATED stjModelSettings "Use generic-lens or generic-optics with 'modelSettings' instead." #-}

-- | Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
--
-- /Note:/ Consider using 'jobExecutionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjJobExecutionSettings :: Lens.Lens' StartTranscriptionJob (Lude.Maybe JobExecutionSettings)
stjJobExecutionSettings = Lens.lens (jobExecutionSettings :: StartTranscriptionJob -> Lude.Maybe JobExecutionSettings) (\s a -> s {jobExecutionSettings = a} :: StartTranscriptionJob)
{-# DEPRECATED stjJobExecutionSettings "Use generic-lens or generic-optics with 'jobExecutionSettings' instead." #-}

-- | You can specify a location in an Amazon S3 bucket to store the output of your transcription job.
--
-- If you don't specify an output key, Amazon Transcribe stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjOutputKey :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Lude.Text)
stjOutputKey = Lens.lens (outputKey :: StartTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {outputKey = a} :: StartTranscriptionJob)
{-# DEPRECATED stjOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | The name of the job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTranscriptionJobName :: Lens.Lens' StartTranscriptionJob Lude.Text
stjTranscriptionJobName = Lens.lens (transcriptionJobName :: StartTranscriptionJob -> Lude.Text) (\s a -> s {transcriptionJobName = a} :: StartTranscriptionJob)
{-# DEPRECATED stjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

-- | Set this field to @true@ to enable automatic language identification. Automatic language identification is disabled by default. You receive a @BadRequestException@ error if you enter a value for a @LanguageCode@ .
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjIdentifyLanguage :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Lude.Bool)
stjIdentifyLanguage = Lens.lens (identifyLanguage :: StartTranscriptionJob -> Lude.Maybe Lude.Bool) (\s a -> s {identifyLanguage = a} :: StartTranscriptionJob)
{-# DEPRECATED stjIdentifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead." #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjMediaSampleRateHertz :: Lens.Lens' StartTranscriptionJob (Lude.Maybe Lude.Natural)
stjMediaSampleRateHertz = Lens.lens (mediaSampleRateHertz :: StartTranscriptionJob -> Lude.Maybe Lude.Natural) (\s a -> s {mediaSampleRateHertz = a} :: StartTranscriptionJob)
{-# DEPRECATED stjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

instance Lude.AWSRequest StartTranscriptionJob where
  type Rs StartTranscriptionJob = StartTranscriptionJobResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTranscriptionJobResponse'
            Lude.<$> (x Lude..?> "TranscriptionJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTranscriptionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.StartTranscriptionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTranscriptionJob where
  toJSON StartTranscriptionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContentRedaction" Lude..=) Lude.<$> contentRedaction,
            ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("LanguageOptions" Lude..=) Lude.<$> languageOptions,
            ("Settings" Lude..=) Lude.<$> settings,
            ("OutputBucketName" Lude..=) Lude.<$> outputBucketName,
            Lude.Just ("Media" Lude..= media),
            ("MediaFormat" Lude..=) Lude.<$> mediaFormat,
            ("OutputEncryptionKMSKeyId" Lude..=)
              Lude.<$> outputEncryptionKMSKeyId,
            ("ModelSettings" Lude..=) Lude.<$> modelSettings,
            ("JobExecutionSettings" Lude..=) Lude.<$> jobExecutionSettings,
            ("OutputKey" Lude..=) Lude.<$> outputKey,
            Lude.Just ("TranscriptionJobName" Lude..= transcriptionJobName),
            ("IdentifyLanguage" Lude..=) Lude.<$> identifyLanguage,
            ("MediaSampleRateHertz" Lude..=) Lude.<$> mediaSampleRateHertz
          ]
      )

instance Lude.ToPath StartTranscriptionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTranscriptionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'
  { -- | An object containing details of the asynchronous transcription job.
    transcriptionJob :: Lude.Maybe TranscriptionJob,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- * 'transcriptionJob' - An object containing details of the asynchronous transcription job.
-- * 'responseStatus' - The response status code.
mkStartTranscriptionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTranscriptionJobResponse
mkStartTranscriptionJobResponse pResponseStatus_ =
  StartTranscriptionJobResponse'
    { transcriptionJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object containing details of the asynchronous transcription job.
--
-- /Note:/ Consider using 'transcriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjrsTranscriptionJob :: Lens.Lens' StartTranscriptionJobResponse (Lude.Maybe TranscriptionJob)
stjrsTranscriptionJob = Lens.lens (transcriptionJob :: StartTranscriptionJobResponse -> Lude.Maybe TranscriptionJob) (\s a -> s {transcriptionJob = a} :: StartTranscriptionJobResponse)
{-# DEPRECATED stjrsTranscriptionJob "Use generic-lens or generic-optics with 'transcriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjrsResponseStatus :: Lens.Lens' StartTranscriptionJobResponse Lude.Int
stjrsResponseStatus = Lens.lens (responseStatus :: StartTranscriptionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTranscriptionJobResponse)
{-# DEPRECATED stjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
