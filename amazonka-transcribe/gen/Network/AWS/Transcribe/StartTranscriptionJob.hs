{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.StartTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to transcribe speech to text.
module Network.AWS.Transcribe.StartTranscriptionJob
  ( -- * Creating a Request
    StartTranscriptionJob (..),
    newStartTranscriptionJob,

    -- * Request Lenses
    startTranscriptionJob_languageCode,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_settings,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_transcriptionJobName,
    startTranscriptionJob_media,

    -- * Destructuring the Response
    StartTranscriptionJobResponse (..),
    newStartTranscriptionJobResponse,

    -- * Response Lenses
    startTranscriptionJobResponse_transcriptionJob,
    startTranscriptionJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newStartTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { -- | The language code for the language used in the input media file.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | An object that contains the request parameters for content redaction.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | Set this field to @true@ to enable automatic language identification.
    -- Automatic language identification is disabled by default. You receive a
    -- @BadRequestException@ error if you enter a value for a @LanguageCode@.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | You can specify a location in an Amazon S3 bucket to store the output of
    -- your transcription job.
    --
    -- If you don\'t specify an output key, Amazon Transcribe stores the output
    -- of your transcription job in the Amazon S3 bucket you specified. By
    -- default, the object key is \"your-transcription-job-name.json\".
    --
    -- You can use output keys to specify the Amazon S3 prefix and file name of
    -- the transcription output. For example, specifying the Amazon S3 prefix,
    -- \"folder1\/folder2\/\", as an output key would lead to the output being
    -- stored as \"folder1\/folder2\/your-transcription-job-name.json\". If you
    -- specify \"my-other-job-name.json\" as the output key, the object key is
    -- changed to \"my-other-job-name.json\". You can use an output key to
    -- change both the prefix and the file name, for example
    -- \"folder\/my-other-job-name.json\".
    --
    -- If you specify an output key, you must also specify an S3 bucket in the
    -- @OutputBucketName@ parameter.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | Choose the custom language model you use for your transcription job in
    -- this parameter.
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS)
    -- key used to encrypt the output of the transcription job. The user
    -- calling the @StartTranscriptionJob@ operation must have permission to
    -- use the specified KMS key.
    --
    -- You can use either of the following to identify a KMS key in the current
    -- account:
    --
    -- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
    --
    -- -   KMS Key Alias: \"alias\/ExampleAlias\"
    --
    -- You can use either of the following to identify a KMS key in the current
    -- account or another account:
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     \"arn:aws:kms:region:account
    --     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
    --
    -- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
    --     ID:alias\/ExampleAlias\"
    --
    -- If you don\'t specify an encryption key, the output of the transcription
    -- job is encrypted with the default Amazon S3 key (SSE-S3).
    --
    -- If you specify a KMS key to encrypt your output, you must also specify
    -- an output location in the @OutputBucketName@ parameter.
    outputEncryptionKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    --
    -- If you do not specify the media sample rate, Amazon Transcribe
    -- determines the sample rate. If you specify the sample rate, it must
    -- match the sample rate detected by Amazon Transcribe. In most cases, you
    -- should leave the @MediaSampleRateHertz@ field blank and let Amazon
    -- Transcribe determine the sample rate.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The location where the transcription is stored.
    --
    -- If you set the @OutputBucketName@, Amazon Transcribe puts the transcript
    -- in the specified S3 bucket. When you call the GetTranscriptionJob
    -- operation, the operation returns this location in the
    -- @TranscriptFileUri@ field. If you enable content redaction, the redacted
    -- transcript appears in @RedactedTranscriptFileUri@. If you enable content
    -- redaction and choose to output an unredacted transcript, that
    -- transcript\'s location still appears in the @TranscriptFileUri@. The S3
    -- bucket must have permissions that allow Amazon Transcribe to put files
    -- in the bucket. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
    --
    -- You can specify an AWS Key Management Service (KMS) key to encrypt the
    -- output of your transcription using the @OutputEncryptionKMSKeyId@
    -- parameter. If you don\'t specify a KMS key, Amazon Transcribe uses the
    -- default Amazon S3 key for server-side encryption of transcripts that are
    -- placed in your S3 bucket.
    --
    -- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
    -- pre-signed URL, a shareable URL that provides secure access to your
    -- transcription, and returns it in the @TranscriptFileUri@ field. Use this
    -- URL to download the transcription.
    outputBucketName :: Prelude.Maybe Prelude.Text,
    -- | Provides information about how a transcription job is executed. Use this
    -- field to indicate that the job can be queued for deferred execution if
    -- the concurrency limit is reached and there are no slots available to
    -- immediately run the job.
    jobExecutionSettings :: Prelude.Maybe JobExecutionSettings,
    -- | A @Settings@ object that provides optional settings for a transcription
    -- job.
    settings :: Prelude.Maybe Settings,
    -- | An object containing a list of languages that might be present in your
    -- collection of audio files. Automatic language identification chooses a
    -- language that best matches the source audio from that list.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
    -- themselves as the job name. The name must also be unique within an AWS
    -- account. If you try to create a transcription job with the same name as
    -- a previous transcription job, you get a @ConflictException@ error.
    transcriptionJobName :: Prelude.Text,
    -- | An object that describes the input media for a transcription job.
    media :: Media
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'startTranscriptionJob_languageCode' - The language code for the language used in the input media file.
--
-- 'mediaFormat', 'startTranscriptionJob_mediaFormat' - The format of the input media file.
--
-- 'contentRedaction', 'startTranscriptionJob_contentRedaction' - An object that contains the request parameters for content redaction.
--
-- 'identifyLanguage', 'startTranscriptionJob_identifyLanguage' - Set this field to @true@ to enable automatic language identification.
-- Automatic language identification is disabled by default. You receive a
-- @BadRequestException@ error if you enter a value for a @LanguageCode@.
--
-- 'outputKey', 'startTranscriptionJob_outputKey' - You can specify a location in an Amazon S3 bucket to store the output of
-- your transcription job.
--
-- If you don\'t specify an output key, Amazon Transcribe stores the output
-- of your transcription job in the Amazon S3 bucket you specified. By
-- default, the object key is \"your-transcription-job-name.json\".
--
-- You can use output keys to specify the Amazon S3 prefix and file name of
-- the transcription output. For example, specifying the Amazon S3 prefix,
-- \"folder1\/folder2\/\", as an output key would lead to the output being
-- stored as \"folder1\/folder2\/your-transcription-job-name.json\". If you
-- specify \"my-other-job-name.json\" as the output key, the object key is
-- changed to \"my-other-job-name.json\". You can use an output key to
-- change both the prefix and the file name, for example
-- \"folder\/my-other-job-name.json\".
--
-- If you specify an output key, you must also specify an S3 bucket in the
-- @OutputBucketName@ parameter.
--
-- 'modelSettings', 'startTranscriptionJob_modelSettings' - Choose the custom language model you use for your transcription job in
-- this parameter.
--
-- 'outputEncryptionKMSKeyId', 'startTranscriptionJob_outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS)
-- key used to encrypt the output of the transcription job. The user
-- calling the @StartTranscriptionJob@ operation must have permission to
-- use the specified KMS key.
--
-- You can use either of the following to identify a KMS key in the current
-- account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the transcription
-- job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputBucketName@ parameter.
--
-- 'mediaSampleRateHertz', 'startTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe
-- determines the sample rate. If you specify the sample rate, it must
-- match the sample rate detected by Amazon Transcribe. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe determine the sample rate.
--
-- 'outputBucketName', 'startTranscriptionJob_outputBucketName' - The location where the transcription is stored.
--
-- If you set the @OutputBucketName@, Amazon Transcribe puts the transcript
-- in the specified S3 bucket. When you call the GetTranscriptionJob
-- operation, the operation returns this location in the
-- @TranscriptFileUri@ field. If you enable content redaction, the redacted
-- transcript appears in @RedactedTranscriptFileUri@. If you enable content
-- redaction and choose to output an unredacted transcript, that
-- transcript\'s location still appears in the @TranscriptFileUri@. The S3
-- bucket must have permissions that allow Amazon Transcribe to put files
-- in the bucket. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
--
-- You can specify an AWS Key Management Service (KMS) key to encrypt the
-- output of your transcription using the @OutputEncryptionKMSKeyId@
-- parameter. If you don\'t specify a KMS key, Amazon Transcribe uses the
-- default Amazon S3 key for server-side encryption of transcripts that are
-- placed in your S3 bucket.
--
-- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
-- pre-signed URL, a shareable URL that provides secure access to your
-- transcription, and returns it in the @TranscriptFileUri@ field. Use this
-- URL to download the transcription.
--
-- 'jobExecutionSettings', 'startTranscriptionJob_jobExecutionSettings' - Provides information about how a transcription job is executed. Use this
-- field to indicate that the job can be queued for deferred execution if
-- the concurrency limit is reached and there are no slots available to
-- immediately run the job.
--
-- 'settings', 'startTranscriptionJob_settings' - A @Settings@ object that provides optional settings for a transcription
-- job.
--
-- 'languageOptions', 'startTranscriptionJob_languageOptions' - An object containing a list of languages that might be present in your
-- collection of audio files. Automatic language identification chooses a
-- language that best matches the source audio from that list.
--
-- 'transcriptionJobName', 'startTranscriptionJob_transcriptionJobName' - The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
-- themselves as the job name. The name must also be unique within an AWS
-- account. If you try to create a transcription job with the same name as
-- a previous transcription job, you get a @ConflictException@ error.
--
-- 'media', 'startTranscriptionJob_media' - An object that describes the input media for a transcription job.
newStartTranscriptionJob ::
  -- | 'transcriptionJobName'
  Prelude.Text ->
  -- | 'media'
  Media ->
  StartTranscriptionJob
newStartTranscriptionJob
  pTranscriptionJobName_
  pMedia_ =
    StartTranscriptionJob'
      { languageCode =
          Prelude.Nothing,
        mediaFormat = Prelude.Nothing,
        contentRedaction = Prelude.Nothing,
        identifyLanguage = Prelude.Nothing,
        outputKey = Prelude.Nothing,
        modelSettings = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        mediaSampleRateHertz = Prelude.Nothing,
        outputBucketName = Prelude.Nothing,
        jobExecutionSettings = Prelude.Nothing,
        settings = Prelude.Nothing,
        languageOptions = Prelude.Nothing,
        transcriptionJobName = pTranscriptionJobName_,
        media = pMedia_
      }

-- | The language code for the language used in the input media file.
startTranscriptionJob_languageCode :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe LanguageCode)
startTranscriptionJob_languageCode = Lens.lens (\StartTranscriptionJob' {languageCode} -> languageCode) (\s@StartTranscriptionJob' {} a -> s {languageCode = a} :: StartTranscriptionJob)

-- | The format of the input media file.
startTranscriptionJob_mediaFormat :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe MediaFormat)
startTranscriptionJob_mediaFormat = Lens.lens (\StartTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@StartTranscriptionJob' {} a -> s {mediaFormat = a} :: StartTranscriptionJob)

-- | An object that contains the request parameters for content redaction.
startTranscriptionJob_contentRedaction :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ContentRedaction)
startTranscriptionJob_contentRedaction = Lens.lens (\StartTranscriptionJob' {contentRedaction} -> contentRedaction) (\s@StartTranscriptionJob' {} a -> s {contentRedaction = a} :: StartTranscriptionJob)

-- | Set this field to @true@ to enable automatic language identification.
-- Automatic language identification is disabled by default. You receive a
-- @BadRequestException@ error if you enter a value for a @LanguageCode@.
startTranscriptionJob_identifyLanguage :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Bool)
startTranscriptionJob_identifyLanguage = Lens.lens (\StartTranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@StartTranscriptionJob' {} a -> s {identifyLanguage = a} :: StartTranscriptionJob)

-- | You can specify a location in an Amazon S3 bucket to store the output of
-- your transcription job.
--
-- If you don\'t specify an output key, Amazon Transcribe stores the output
-- of your transcription job in the Amazon S3 bucket you specified. By
-- default, the object key is \"your-transcription-job-name.json\".
--
-- You can use output keys to specify the Amazon S3 prefix and file name of
-- the transcription output. For example, specifying the Amazon S3 prefix,
-- \"folder1\/folder2\/\", as an output key would lead to the output being
-- stored as \"folder1\/folder2\/your-transcription-job-name.json\". If you
-- specify \"my-other-job-name.json\" as the output key, the object key is
-- changed to \"my-other-job-name.json\". You can use an output key to
-- change both the prefix and the file name, for example
-- \"folder\/my-other-job-name.json\".
--
-- If you specify an output key, you must also specify an S3 bucket in the
-- @OutputBucketName@ parameter.
startTranscriptionJob_outputKey :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputKey = Lens.lens (\StartTranscriptionJob' {outputKey} -> outputKey) (\s@StartTranscriptionJob' {} a -> s {outputKey = a} :: StartTranscriptionJob)

-- | Choose the custom language model you use for your transcription job in
-- this parameter.
startTranscriptionJob_modelSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ModelSettings)
startTranscriptionJob_modelSettings = Lens.lens (\StartTranscriptionJob' {modelSettings} -> modelSettings) (\s@StartTranscriptionJob' {} a -> s {modelSettings = a} :: StartTranscriptionJob)

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS)
-- key used to encrypt the output of the transcription job. The user
-- calling the @StartTranscriptionJob@ operation must have permission to
-- use the specified KMS key.
--
-- You can use either of the following to identify a KMS key in the current
-- account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the transcription
-- job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputBucketName@ parameter.
startTranscriptionJob_outputEncryptionKMSKeyId :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputEncryptionKMSKeyId = Lens.lens (\StartTranscriptionJob' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@StartTranscriptionJob' {} a -> s {outputEncryptionKMSKeyId = a} :: StartTranscriptionJob)

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe
-- determines the sample rate. If you specify the sample rate, it must
-- match the sample rate detected by Amazon Transcribe. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe determine the sample rate.
startTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Natural)
startTranscriptionJob_mediaSampleRateHertz = Lens.lens (\StartTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@StartTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: StartTranscriptionJob)

-- | The location where the transcription is stored.
--
-- If you set the @OutputBucketName@, Amazon Transcribe puts the transcript
-- in the specified S3 bucket. When you call the GetTranscriptionJob
-- operation, the operation returns this location in the
-- @TranscriptFileUri@ field. If you enable content redaction, the redacted
-- transcript appears in @RedactedTranscriptFileUri@. If you enable content
-- redaction and choose to output an unredacted transcript, that
-- transcript\'s location still appears in the @TranscriptFileUri@. The S3
-- bucket must have permissions that allow Amazon Transcribe to put files
-- in the bucket. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
--
-- You can specify an AWS Key Management Service (KMS) key to encrypt the
-- output of your transcription using the @OutputEncryptionKMSKeyId@
-- parameter. If you don\'t specify a KMS key, Amazon Transcribe uses the
-- default Amazon S3 key for server-side encryption of transcripts that are
-- placed in your S3 bucket.
--
-- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
-- pre-signed URL, a shareable URL that provides secure access to your
-- transcription, and returns it in the @TranscriptFileUri@ field. Use this
-- URL to download the transcription.
startTranscriptionJob_outputBucketName :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputBucketName = Lens.lens (\StartTranscriptionJob' {outputBucketName} -> outputBucketName) (\s@StartTranscriptionJob' {} a -> s {outputBucketName = a} :: StartTranscriptionJob)

-- | Provides information about how a transcription job is executed. Use this
-- field to indicate that the job can be queued for deferred execution if
-- the concurrency limit is reached and there are no slots available to
-- immediately run the job.
startTranscriptionJob_jobExecutionSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe JobExecutionSettings)
startTranscriptionJob_jobExecutionSettings = Lens.lens (\StartTranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@StartTranscriptionJob' {} a -> s {jobExecutionSettings = a} :: StartTranscriptionJob)

-- | A @Settings@ object that provides optional settings for a transcription
-- job.
startTranscriptionJob_settings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Settings)
startTranscriptionJob_settings = Lens.lens (\StartTranscriptionJob' {settings} -> settings) (\s@StartTranscriptionJob' {} a -> s {settings = a} :: StartTranscriptionJob)

-- | An object containing a list of languages that might be present in your
-- collection of audio files. Automatic language identification chooses a
-- language that best matches the source audio from that list.
startTranscriptionJob_languageOptions :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
startTranscriptionJob_languageOptions = Lens.lens (\StartTranscriptionJob' {languageOptions} -> languageOptions) (\s@StartTranscriptionJob' {} a -> s {languageOptions = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
-- themselves as the job name. The name must also be unique within an AWS
-- account. If you try to create a transcription job with the same name as
-- a previous transcription job, you get a @ConflictException@ error.
startTranscriptionJob_transcriptionJobName :: Lens.Lens' StartTranscriptionJob Prelude.Text
startTranscriptionJob_transcriptionJobName = Lens.lens (\StartTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@StartTranscriptionJob' {} a -> s {transcriptionJobName = a} :: StartTranscriptionJob)

-- | An object that describes the input media for a transcription job.
startTranscriptionJob_media :: Lens.Lens' StartTranscriptionJob Media
startTranscriptionJob_media = Lens.lens (\StartTranscriptionJob' {media} -> media) (\s@StartTranscriptionJob' {} a -> s {media = a} :: StartTranscriptionJob)

instance Core.AWSRequest StartTranscriptionJob where
  type
    AWSResponse StartTranscriptionJob =
      StartTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTranscriptionJobResponse'
            Prelude.<$> (x Core..?> "TranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTranscriptionJob

instance Prelude.NFData StartTranscriptionJob

instance Core.ToHeaders StartTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.StartTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartTranscriptionJob where
  toJSON StartTranscriptionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("MediaFormat" Core..=) Prelude.<$> mediaFormat,
            ("ContentRedaction" Core..=)
              Prelude.<$> contentRedaction,
            ("IdentifyLanguage" Core..=)
              Prelude.<$> identifyLanguage,
            ("OutputKey" Core..=) Prelude.<$> outputKey,
            ("ModelSettings" Core..=) Prelude.<$> modelSettings,
            ("OutputEncryptionKMSKeyId" Core..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("MediaSampleRateHertz" Core..=)
              Prelude.<$> mediaSampleRateHertz,
            ("OutputBucketName" Core..=)
              Prelude.<$> outputBucketName,
            ("JobExecutionSettings" Core..=)
              Prelude.<$> jobExecutionSettings,
            ("Settings" Core..=) Prelude.<$> settings,
            ("LanguageOptions" Core..=)
              Prelude.<$> languageOptions,
            Prelude.Just
              ( "TranscriptionJobName"
                  Core..= transcriptionJobName
              ),
            Prelude.Just ("Media" Core..= media)
          ]
      )

instance Core.ToPath StartTranscriptionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'
  { -- | An object containing details of the asynchronous transcription job.
    transcriptionJob :: Prelude.Maybe TranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJob', 'startTranscriptionJobResponse_transcriptionJob' - An object containing details of the asynchronous transcription job.
--
-- 'httpStatus', 'startTranscriptionJobResponse_httpStatus' - The response's http status code.
newStartTranscriptionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTranscriptionJobResponse
newStartTranscriptionJobResponse pHttpStatus_ =
  StartTranscriptionJobResponse'
    { transcriptionJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing details of the asynchronous transcription job.
startTranscriptionJobResponse_transcriptionJob :: Lens.Lens' StartTranscriptionJobResponse (Prelude.Maybe TranscriptionJob)
startTranscriptionJobResponse_transcriptionJob = Lens.lens (\StartTranscriptionJobResponse' {transcriptionJob} -> transcriptionJob) (\s@StartTranscriptionJobResponse' {} a -> s {transcriptionJob = a} :: StartTranscriptionJobResponse)

-- | The response's http status code.
startTranscriptionJobResponse_httpStatus :: Lens.Lens' StartTranscriptionJobResponse Prelude.Int
startTranscriptionJobResponse_httpStatus = Lens.lens (\StartTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@StartTranscriptionJobResponse' {} a -> s {httpStatus = a} :: StartTranscriptionJobResponse)

instance Prelude.NFData StartTranscriptionJobResponse
