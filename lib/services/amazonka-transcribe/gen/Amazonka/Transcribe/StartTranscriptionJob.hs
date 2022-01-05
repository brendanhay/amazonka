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
-- Module      : Amazonka.Transcribe.StartTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to transcribe speech to text.
module Amazonka.Transcribe.StartTranscriptionJob
  ( -- * Creating a Request
    StartTranscriptionJob (..),
    newStartTranscriptionJob,

    -- * Request Lenses
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_languageCode,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_settings,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_tags,
    startTranscriptionJob_mediaSampleRateHertz,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newStartTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { -- | An object that contains the request parameters for content redaction.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | Add subtitles to your batch transcription job.
    subtitles :: Prelude.Maybe Subtitles,
    -- | The language code for the language used in the input media file.
    --
    -- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
    -- video file must be encoded at a sample rate of 16,000 Hz or higher.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | An object containing a list of languages that might be present in your
    -- collection of audio files. Automatic language identification chooses a
    -- language that best matches the source audio from that list.
    --
    -- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
    -- video file must be encoded at a sample rate of 16,000 Hz or higher.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | A @Settings@ object that provides optional settings for a transcription
    -- job.
    settings :: Prelude.Maybe Settings,
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
    -- You can specify an Amazon Web Services Key Management Service (KMS) key
    -- to encrypt the output of your transcription using the
    -- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
    -- Amazon Transcribe uses the default Amazon S3 key for server-side
    -- encryption of transcripts that are placed in your S3 bucket.
    --
    -- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
    -- pre-signed URL, a shareable URL that provides secure access to your
    -- transcription, and returns it in the @TranscriptFileUri@ field. Use this
    -- URL to download the transcription.
    outputBucketName :: Prelude.Maybe Prelude.Text,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
    -- Service (KMS) key used to encrypt the output of the transcription job.
    -- The user calling the @StartTranscriptionJob@ operation must have
    -- permission to use the specified KMS key.
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
    -- | Choose the custom language model you use for your transcription job in
    -- this parameter.
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | A map of plain text, non-secret key:value pairs, known as encryption
    -- context pairs, that provide an added layer of security for your data.
    kmsEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Provides information about how a transcription job is executed. Use this
    -- field to indicate that the job can be queued for deferred execution if
    -- the concurrency limit is reached and there are no slots available to
    -- immediately run the job.
    jobExecutionSettings :: Prelude.Maybe JobExecutionSettings,
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
    -- | Set this field to @true@ to enable automatic language identification.
    -- Automatic language identification is disabled by default. You receive a
    -- @BadRequestException@ error if you enter a value for a @LanguageCode@.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | Add tags to an Amazon Transcribe transcription job.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    --
    -- If you do not specify the media sample rate, Amazon Transcribe
    -- determines the sample rate. If you specify the sample rate, it must
    -- match the sample rate detected by Amazon Transcribe. In most cases, you
    -- should leave the @MediaSampleRateHertz@ field blank and let Amazon
    -- Transcribe determine the sample rate.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
    -- themselves as the job name. The name must also be unique within an
    -- Amazon Web Services account. If you try to create a transcription job
    -- with the same name as a previous transcription job, you get a
    -- @ConflictException@ error.
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
-- 'contentRedaction', 'startTranscriptionJob_contentRedaction' - An object that contains the request parameters for content redaction.
--
-- 'subtitles', 'startTranscriptionJob_subtitles' - Add subtitles to your batch transcription job.
--
-- 'languageCode', 'startTranscriptionJob_languageCode' - The language code for the language used in the input media file.
--
-- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
-- video file must be encoded at a sample rate of 16,000 Hz or higher.
--
-- 'languageOptions', 'startTranscriptionJob_languageOptions' - An object containing a list of languages that might be present in your
-- collection of audio files. Automatic language identification chooses a
-- language that best matches the source audio from that list.
--
-- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
-- video file must be encoded at a sample rate of 16,000 Hz or higher.
--
-- 'settings', 'startTranscriptionJob_settings' - A @Settings@ object that provides optional settings for a transcription
-- job.
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
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of your transcription using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe uses the default Amazon S3 key for server-side
-- encryption of transcripts that are placed in your S3 bucket.
--
-- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
-- pre-signed URL, a shareable URL that provides secure access to your
-- transcription, and returns it in the @TranscriptFileUri@ field. Use this
-- URL to download the transcription.
--
-- 'mediaFormat', 'startTranscriptionJob_mediaFormat' - The format of the input media file.
--
-- 'outputEncryptionKMSKeyId', 'startTranscriptionJob_outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service (KMS) key used to encrypt the output of the transcription job.
-- The user calling the @StartTranscriptionJob@ operation must have
-- permission to use the specified KMS key.
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
-- 'modelSettings', 'startTranscriptionJob_modelSettings' - Choose the custom language model you use for your transcription job in
-- this parameter.
--
-- 'kmsEncryptionContext', 'startTranscriptionJob_kmsEncryptionContext' - A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
--
-- 'jobExecutionSettings', 'startTranscriptionJob_jobExecutionSettings' - Provides information about how a transcription job is executed. Use this
-- field to indicate that the job can be queued for deferred execution if
-- the concurrency limit is reached and there are no slots available to
-- immediately run the job.
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
-- 'identifyLanguage', 'startTranscriptionJob_identifyLanguage' - Set this field to @true@ to enable automatic language identification.
-- Automatic language identification is disabled by default. You receive a
-- @BadRequestException@ error if you enter a value for a @LanguageCode@.
--
-- 'tags', 'startTranscriptionJob_tags' - Add tags to an Amazon Transcribe transcription job.
--
-- 'mediaSampleRateHertz', 'startTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe
-- determines the sample rate. If you specify the sample rate, it must
-- match the sample rate detected by Amazon Transcribe. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe determine the sample rate.
--
-- 'transcriptionJobName', 'startTranscriptionJob_transcriptionJobName' - The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
-- themselves as the job name. The name must also be unique within an
-- Amazon Web Services account. If you try to create a transcription job
-- with the same name as a previous transcription job, you get a
-- @ConflictException@ error.
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
      { contentRedaction =
          Prelude.Nothing,
        subtitles = Prelude.Nothing,
        languageCode = Prelude.Nothing,
        languageOptions = Prelude.Nothing,
        settings = Prelude.Nothing,
        outputBucketName = Prelude.Nothing,
        mediaFormat = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        modelSettings = Prelude.Nothing,
        kmsEncryptionContext = Prelude.Nothing,
        jobExecutionSettings = Prelude.Nothing,
        outputKey = Prelude.Nothing,
        identifyLanguage = Prelude.Nothing,
        tags = Prelude.Nothing,
        mediaSampleRateHertz = Prelude.Nothing,
        transcriptionJobName = pTranscriptionJobName_,
        media = pMedia_
      }

-- | An object that contains the request parameters for content redaction.
startTranscriptionJob_contentRedaction :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ContentRedaction)
startTranscriptionJob_contentRedaction = Lens.lens (\StartTranscriptionJob' {contentRedaction} -> contentRedaction) (\s@StartTranscriptionJob' {} a -> s {contentRedaction = a} :: StartTranscriptionJob)

-- | Add subtitles to your batch transcription job.
startTranscriptionJob_subtitles :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Subtitles)
startTranscriptionJob_subtitles = Lens.lens (\StartTranscriptionJob' {subtitles} -> subtitles) (\s@StartTranscriptionJob' {} a -> s {subtitles = a} :: StartTranscriptionJob)

-- | The language code for the language used in the input media file.
--
-- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
-- video file must be encoded at a sample rate of 16,000 Hz or higher.
startTranscriptionJob_languageCode :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe LanguageCode)
startTranscriptionJob_languageCode = Lens.lens (\StartTranscriptionJob' {languageCode} -> languageCode) (\s@StartTranscriptionJob' {} a -> s {languageCode = a} :: StartTranscriptionJob)

-- | An object containing a list of languages that might be present in your
-- collection of audio files. Automatic language identification chooses a
-- language that best matches the source audio from that list.
--
-- To transcribe speech in Modern Standard Arabic (ar-SA), your audio or
-- video file must be encoded at a sample rate of 16,000 Hz or higher.
startTranscriptionJob_languageOptions :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
startTranscriptionJob_languageOptions = Lens.lens (\StartTranscriptionJob' {languageOptions} -> languageOptions) (\s@StartTranscriptionJob' {} a -> s {languageOptions = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | A @Settings@ object that provides optional settings for a transcription
-- job.
startTranscriptionJob_settings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Settings)
startTranscriptionJob_settings = Lens.lens (\StartTranscriptionJob' {settings} -> settings) (\s@StartTranscriptionJob' {} a -> s {settings = a} :: StartTranscriptionJob)

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
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of your transcription using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe uses the default Amazon S3 key for server-side
-- encryption of transcripts that are placed in your S3 bucket.
--
-- If you don\'t set the @OutputBucketName@, Amazon Transcribe generates a
-- pre-signed URL, a shareable URL that provides secure access to your
-- transcription, and returns it in the @TranscriptFileUri@ field. Use this
-- URL to download the transcription.
startTranscriptionJob_outputBucketName :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputBucketName = Lens.lens (\StartTranscriptionJob' {outputBucketName} -> outputBucketName) (\s@StartTranscriptionJob' {} a -> s {outputBucketName = a} :: StartTranscriptionJob)

-- | The format of the input media file.
startTranscriptionJob_mediaFormat :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe MediaFormat)
startTranscriptionJob_mediaFormat = Lens.lens (\StartTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@StartTranscriptionJob' {} a -> s {mediaFormat = a} :: StartTranscriptionJob)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service (KMS) key used to encrypt the output of the transcription job.
-- The user calling the @StartTranscriptionJob@ operation must have
-- permission to use the specified KMS key.
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

-- | Choose the custom language model you use for your transcription job in
-- this parameter.
startTranscriptionJob_modelSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ModelSettings)
startTranscriptionJob_modelSettings = Lens.lens (\StartTranscriptionJob' {modelSettings} -> modelSettings) (\s@StartTranscriptionJob' {} a -> s {modelSettings = a} :: StartTranscriptionJob)

-- | A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
startTranscriptionJob_kmsEncryptionContext :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTranscriptionJob_kmsEncryptionContext = Lens.lens (\StartTranscriptionJob' {kmsEncryptionContext} -> kmsEncryptionContext) (\s@StartTranscriptionJob' {} a -> s {kmsEncryptionContext = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Provides information about how a transcription job is executed. Use this
-- field to indicate that the job can be queued for deferred execution if
-- the concurrency limit is reached and there are no slots available to
-- immediately run the job.
startTranscriptionJob_jobExecutionSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe JobExecutionSettings)
startTranscriptionJob_jobExecutionSettings = Lens.lens (\StartTranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@StartTranscriptionJob' {} a -> s {jobExecutionSettings = a} :: StartTranscriptionJob)

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

-- | Set this field to @true@ to enable automatic language identification.
-- Automatic language identification is disabled by default. You receive a
-- @BadRequestException@ error if you enter a value for a @LanguageCode@.
startTranscriptionJob_identifyLanguage :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Bool)
startTranscriptionJob_identifyLanguage = Lens.lens (\StartTranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@StartTranscriptionJob' {} a -> s {identifyLanguage = a} :: StartTranscriptionJob)

-- | Add tags to an Amazon Transcribe transcription job.
startTranscriptionJob_tags :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
startTranscriptionJob_tags = Lens.lens (\StartTranscriptionJob' {tags} -> tags) (\s@StartTranscriptionJob' {} a -> s {tags = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe
-- determines the sample rate. If you specify the sample rate, it must
-- match the sample rate detected by Amazon Transcribe. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe determine the sample rate.
startTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Natural)
startTranscriptionJob_mediaSampleRateHertz = Lens.lens (\StartTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@StartTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: StartTranscriptionJob)

-- | The name of the job. You can\'t use the strings \"@.@\" or \"@..@\" by
-- themselves as the job name. The name must also be unique within an
-- Amazon Web Services account. If you try to create a transcription job
-- with the same name as a previous transcription job, you get a
-- @ConflictException@ error.
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

instance Prelude.Hashable StartTranscriptionJob where
  hashWithSalt _salt StartTranscriptionJob' {..} =
    _salt `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` subtitles
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageOptions
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` outputBucketName
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` outputEncryptionKMSKeyId
      `Prelude.hashWithSalt` modelSettings
      `Prelude.hashWithSalt` kmsEncryptionContext
      `Prelude.hashWithSalt` jobExecutionSettings
      `Prelude.hashWithSalt` outputKey
      `Prelude.hashWithSalt` identifyLanguage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` transcriptionJobName
      `Prelude.hashWithSalt` media

instance Prelude.NFData StartTranscriptionJob where
  rnf StartTranscriptionJob' {..} =
    Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf subtitles
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageOptions
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf outputBucketName
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf outputEncryptionKMSKeyId
      `Prelude.seq` Prelude.rnf modelSettings
      `Prelude.seq` Prelude.rnf kmsEncryptionContext
      `Prelude.seq` Prelude.rnf jobExecutionSettings
      `Prelude.seq` Prelude.rnf outputKey
      `Prelude.seq` Prelude.rnf identifyLanguage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf transcriptionJobName
      `Prelude.seq` Prelude.rnf media

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
          [ ("ContentRedaction" Core..=)
              Prelude.<$> contentRedaction,
            ("Subtitles" Core..=) Prelude.<$> subtitles,
            ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("LanguageOptions" Core..=)
              Prelude.<$> languageOptions,
            ("Settings" Core..=) Prelude.<$> settings,
            ("OutputBucketName" Core..=)
              Prelude.<$> outputBucketName,
            ("MediaFormat" Core..=) Prelude.<$> mediaFormat,
            ("OutputEncryptionKMSKeyId" Core..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("ModelSettings" Core..=) Prelude.<$> modelSettings,
            ("KMSEncryptionContext" Core..=)
              Prelude.<$> kmsEncryptionContext,
            ("JobExecutionSettings" Core..=)
              Prelude.<$> jobExecutionSettings,
            ("OutputKey" Core..=) Prelude.<$> outputKey,
            ("IdentifyLanguage" Core..=)
              Prelude.<$> identifyLanguage,
            ("Tags" Core..=) Prelude.<$> tags,
            ("MediaSampleRateHertz" Core..=)
              Prelude.<$> mediaSampleRateHertz,
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

instance Prelude.NFData StartTranscriptionJobResponse where
  rnf StartTranscriptionJobResponse' {..} =
    Prelude.rnf transcriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
