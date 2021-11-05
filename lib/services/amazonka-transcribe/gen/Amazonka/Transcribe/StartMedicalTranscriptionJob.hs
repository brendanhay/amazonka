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
-- Module      : Amazonka.Transcribe.StartMedicalTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a batch job to transcribe medical speech to text.
module Amazonka.Transcribe.StartMedicalTranscriptionJob
  ( -- * Creating a Request
    StartMedicalTranscriptionJob (..),
    newStartMedicalTranscriptionJob,

    -- * Request Lenses
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_tags,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_medicalTranscriptionJobName,
    startMedicalTranscriptionJob_languageCode,
    startMedicalTranscriptionJob_media,
    startMedicalTranscriptionJob_outputBucketName,
    startMedicalTranscriptionJob_specialty,
    startMedicalTranscriptionJob_type,

    -- * Destructuring the Response
    StartMedicalTranscriptionJobResponse (..),
    newStartMedicalTranscriptionJobResponse,

    -- * Response Lenses
    startMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    startMedicalTranscriptionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newStartMedicalTranscriptionJob' smart constructor.
data StartMedicalTranscriptionJob = StartMedicalTranscriptionJob'
  { -- | Optional settings for the medical transcription job.
    settings :: Prelude.Maybe MedicalTranscriptionSetting,
    -- | The audio format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
    -- Service (KMS) key used to encrypt the output of the transcription job.
    -- The user calling the StartMedicalTranscriptionJob operation must have
    -- permission to use the specified KMS key.
    --
    -- You use either of the following to identify a KMS key in the current
    -- account:
    --
    -- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
    --
    -- -   KMS Key Alias: \"alias\/ExampleAlias\"
    --
    -- You can use either of the following to identify a KMS key in the current
    -- account or another account:
    --
    -- -   Amazon Resource Name (ARN) of a KMS key in the current account or
    --     another account: \"arn:aws:kms:region:account
    --     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
    --
    -- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
    --     ID:alias\/ExampleAlias\"
    --
    -- If you don\'t specify an encryption key, the output of the medical
    -- transcription job is encrypted with the default Amazon S3 key (SSE-S3).
    --
    -- If you specify a KMS key to encrypt your output, you must also specify
    -- an output location in the @OutputBucketName@ parameter.
    outputEncryptionKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | A map of plain text, non-secret key:value pairs, known as encryption
    -- context pairs, that provide an added layer of security for your data.
    kmsEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | You can specify a location in an Amazon S3 bucket to store the output of
    -- your medical transcription job.
    --
    -- If you don\'t specify an output key, Amazon Transcribe Medical stores
    -- the output of your transcription job in the Amazon S3 bucket you
    -- specified. By default, the object key is
    -- \"your-transcription-job-name.json\".
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
    -- | You can configure Amazon Transcribe Medical to label content in the
    -- transcription output. If you specify @PHI@, Amazon Transcribe Medical
    -- labels the personal health information (PHI) that it identifies in the
    -- transcription output.
    contentIdentificationType :: Prelude.Maybe MedicalContentIdentificationType,
    -- | Add tags to an Amazon Transcribe medical transcription job.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    --
    -- If you do not specify the media sample rate, Amazon Transcribe Medical
    -- determines the sample rate. If you specify the sample rate, it must
    -- match the rate detected by Amazon Transcribe Medical. In most cases, you
    -- should leave the @MediaSampleRateHertz@ field blank and let Amazon
    -- Transcribe Medical determine the sample rate.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The name of the medical transcription job. You can\'t use the strings
    -- \"@.@\" or \"@..@\" by themselves as the job name. The name must also be
    -- unique within an Amazon Web Services account. If you try to create a
    -- medical transcription job with the same name as a previous medical
    -- transcription job, you get a @ConflictException@ error.
    medicalTranscriptionJobName :: Prelude.Text,
    -- | The language code for the language spoken in the input media file. US
    -- English (en-US) is the valid value for medical transcription jobs. Any
    -- other value you enter for language code results in a
    -- @BadRequestException@ error.
    languageCode :: LanguageCode,
    media :: Media,
    -- | The Amazon S3 location where the transcription is stored.
    --
    -- You must set @OutputBucketName@ for Amazon Transcribe Medical to store
    -- the transcription results. Your transcript appears in the S3 location
    -- you specify. When you call the GetMedicalTranscriptionJob, the operation
    -- returns this location in the @TranscriptFileUri@ field. The S3 bucket
    -- must have permissions that allow Amazon Transcribe Medical to put files
    -- in the bucket. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
    --
    -- You can specify an Amazon Web Services Key Management Service (KMS) key
    -- to encrypt the output of your transcription using the
    -- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
    -- Amazon Transcribe Medical uses the default Amazon S3 key for server-side
    -- encryption of transcripts that are placed in your S3 bucket.
    outputBucketName :: Prelude.Text,
    -- | The medical specialty of any clinician speaking in the input media.
    specialty :: Specialty,
    -- | The type of speech in the input audio. @CONVERSATION@ refers to
    -- conversations between two or more speakers, e.g., a conversations
    -- between doctors and patients. @DICTATION@ refers to single-speaker
    -- dictated speech, such as clinical notes.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'startMedicalTranscriptionJob_settings' - Optional settings for the medical transcription job.
--
-- 'mediaFormat', 'startMedicalTranscriptionJob_mediaFormat' - The audio format of the input media file.
--
-- 'outputEncryptionKMSKeyId', 'startMedicalTranscriptionJob_outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service (KMS) key used to encrypt the output of the transcription job.
-- The user calling the StartMedicalTranscriptionJob operation must have
-- permission to use the specified KMS key.
--
-- You use either of the following to identify a KMS key in the current
-- account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS key in the current account or
--     another account: \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the medical
-- transcription job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputBucketName@ parameter.
--
-- 'kmsEncryptionContext', 'startMedicalTranscriptionJob_kmsEncryptionContext' - A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
--
-- 'outputKey', 'startMedicalTranscriptionJob_outputKey' - You can specify a location in an Amazon S3 bucket to store the output of
-- your medical transcription job.
--
-- If you don\'t specify an output key, Amazon Transcribe Medical stores
-- the output of your transcription job in the Amazon S3 bucket you
-- specified. By default, the object key is
-- \"your-transcription-job-name.json\".
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
-- 'contentIdentificationType', 'startMedicalTranscriptionJob_contentIdentificationType' - You can configure Amazon Transcribe Medical to label content in the
-- transcription output. If you specify @PHI@, Amazon Transcribe Medical
-- labels the personal health information (PHI) that it identifies in the
-- transcription output.
--
-- 'tags', 'startMedicalTranscriptionJob_tags' - Add tags to an Amazon Transcribe medical transcription job.
--
-- 'mediaSampleRateHertz', 'startMedicalTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical
-- determines the sample rate. If you specify the sample rate, it must
-- match the rate detected by Amazon Transcribe Medical. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe Medical determine the sample rate.
--
-- 'medicalTranscriptionJobName', 'startMedicalTranscriptionJob_medicalTranscriptionJobName' - The name of the medical transcription job. You can\'t use the strings
-- \"@.@\" or \"@..@\" by themselves as the job name. The name must also be
-- unique within an Amazon Web Services account. If you try to create a
-- medical transcription job with the same name as a previous medical
-- transcription job, you get a @ConflictException@ error.
--
-- 'languageCode', 'startMedicalTranscriptionJob_languageCode' - The language code for the language spoken in the input media file. US
-- English (en-US) is the valid value for medical transcription jobs. Any
-- other value you enter for language code results in a
-- @BadRequestException@ error.
--
-- 'media', 'startMedicalTranscriptionJob_media' - Undocumented member.
--
-- 'outputBucketName', 'startMedicalTranscriptionJob_outputBucketName' - The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store
-- the transcription results. Your transcript appears in the S3 location
-- you specify. When you call the GetMedicalTranscriptionJob, the operation
-- returns this location in the @TranscriptFileUri@ field. The S3 bucket
-- must have permissions that allow Amazon Transcribe Medical to put files
-- in the bucket. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
--
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of your transcription using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe Medical uses the default Amazon S3 key for server-side
-- encryption of transcripts that are placed in your S3 bucket.
--
-- 'specialty', 'startMedicalTranscriptionJob_specialty' - The medical specialty of any clinician speaking in the input media.
--
-- 'type'', 'startMedicalTranscriptionJob_type' - The type of speech in the input audio. @CONVERSATION@ refers to
-- conversations between two or more speakers, e.g., a conversations
-- between doctors and patients. @DICTATION@ refers to single-speaker
-- dictated speech, such as clinical notes.
newStartMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'media'
  Media ->
  -- | 'outputBucketName'
  Prelude.Text ->
  -- | 'specialty'
  Specialty ->
  -- | 'type''
  Type ->
  StartMedicalTranscriptionJob
newStartMedicalTranscriptionJob
  pMedicalTranscriptionJobName_
  pLanguageCode_
  pMedia_
  pOutputBucketName_
  pSpecialty_
  pType_ =
    StartMedicalTranscriptionJob'
      { settings =
          Prelude.Nothing,
        mediaFormat = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        kmsEncryptionContext = Prelude.Nothing,
        outputKey = Prelude.Nothing,
        contentIdentificationType = Prelude.Nothing,
        tags = Prelude.Nothing,
        mediaSampleRateHertz = Prelude.Nothing,
        medicalTranscriptionJobName =
          pMedicalTranscriptionJobName_,
        languageCode = pLanguageCode_,
        media = pMedia_,
        outputBucketName = pOutputBucketName_,
        specialty = pSpecialty_,
        type' = pType_
      }

-- | Optional settings for the medical transcription job.
startMedicalTranscriptionJob_settings :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MedicalTranscriptionSetting)
startMedicalTranscriptionJob_settings = Lens.lens (\StartMedicalTranscriptionJob' {settings} -> settings) (\s@StartMedicalTranscriptionJob' {} a -> s {settings = a} :: StartMedicalTranscriptionJob)

-- | The audio format of the input media file.
startMedicalTranscriptionJob_mediaFormat :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MediaFormat)
startMedicalTranscriptionJob_mediaFormat = Lens.lens (\StartMedicalTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@StartMedicalTranscriptionJob' {} a -> s {mediaFormat = a} :: StartMedicalTranscriptionJob)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service (KMS) key used to encrypt the output of the transcription job.
-- The user calling the StartMedicalTranscriptionJob operation must have
-- permission to use the specified KMS key.
--
-- You use either of the following to identify a KMS key in the current
-- account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS key in the current account or
--     another account: \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the medical
-- transcription job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputBucketName@ parameter.
startMedicalTranscriptionJob_outputEncryptionKMSKeyId :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
startMedicalTranscriptionJob_outputEncryptionKMSKeyId = Lens.lens (\StartMedicalTranscriptionJob' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@StartMedicalTranscriptionJob' {} a -> s {outputEncryptionKMSKeyId = a} :: StartMedicalTranscriptionJob)

-- | A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
startMedicalTranscriptionJob_kmsEncryptionContext :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startMedicalTranscriptionJob_kmsEncryptionContext = Lens.lens (\StartMedicalTranscriptionJob' {kmsEncryptionContext} -> kmsEncryptionContext) (\s@StartMedicalTranscriptionJob' {} a -> s {kmsEncryptionContext = a} :: StartMedicalTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | You can specify a location in an Amazon S3 bucket to store the output of
-- your medical transcription job.
--
-- If you don\'t specify an output key, Amazon Transcribe Medical stores
-- the output of your transcription job in the Amazon S3 bucket you
-- specified. By default, the object key is
-- \"your-transcription-job-name.json\".
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
startMedicalTranscriptionJob_outputKey :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
startMedicalTranscriptionJob_outputKey = Lens.lens (\StartMedicalTranscriptionJob' {outputKey} -> outputKey) (\s@StartMedicalTranscriptionJob' {} a -> s {outputKey = a} :: StartMedicalTranscriptionJob)

-- | You can configure Amazon Transcribe Medical to label content in the
-- transcription output. If you specify @PHI@, Amazon Transcribe Medical
-- labels the personal health information (PHI) that it identifies in the
-- transcription output.
startMedicalTranscriptionJob_contentIdentificationType :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MedicalContentIdentificationType)
startMedicalTranscriptionJob_contentIdentificationType = Lens.lens (\StartMedicalTranscriptionJob' {contentIdentificationType} -> contentIdentificationType) (\s@StartMedicalTranscriptionJob' {} a -> s {contentIdentificationType = a} :: StartMedicalTranscriptionJob)

-- | Add tags to an Amazon Transcribe medical transcription job.
startMedicalTranscriptionJob_tags :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
startMedicalTranscriptionJob_tags = Lens.lens (\StartMedicalTranscriptionJob' {tags} -> tags) (\s@StartMedicalTranscriptionJob' {} a -> s {tags = a} :: StartMedicalTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical
-- determines the sample rate. If you specify the sample rate, it must
-- match the rate detected by Amazon Transcribe Medical. In most cases, you
-- should leave the @MediaSampleRateHertz@ field blank and let Amazon
-- Transcribe Medical determine the sample rate.
startMedicalTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Natural)
startMedicalTranscriptionJob_mediaSampleRateHertz = Lens.lens (\StartMedicalTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@StartMedicalTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: StartMedicalTranscriptionJob)

-- | The name of the medical transcription job. You can\'t use the strings
-- \"@.@\" or \"@..@\" by themselves as the job name. The name must also be
-- unique within an Amazon Web Services account. If you try to create a
-- medical transcription job with the same name as a previous medical
-- transcription job, you get a @ConflictException@ error.
startMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' StartMedicalTranscriptionJob Prelude.Text
startMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\StartMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@StartMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: StartMedicalTranscriptionJob)

-- | The language code for the language spoken in the input media file. US
-- English (en-US) is the valid value for medical transcription jobs. Any
-- other value you enter for language code results in a
-- @BadRequestException@ error.
startMedicalTranscriptionJob_languageCode :: Lens.Lens' StartMedicalTranscriptionJob LanguageCode
startMedicalTranscriptionJob_languageCode = Lens.lens (\StartMedicalTranscriptionJob' {languageCode} -> languageCode) (\s@StartMedicalTranscriptionJob' {} a -> s {languageCode = a} :: StartMedicalTranscriptionJob)

-- | Undocumented member.
startMedicalTranscriptionJob_media :: Lens.Lens' StartMedicalTranscriptionJob Media
startMedicalTranscriptionJob_media = Lens.lens (\StartMedicalTranscriptionJob' {media} -> media) (\s@StartMedicalTranscriptionJob' {} a -> s {media = a} :: StartMedicalTranscriptionJob)

-- | The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store
-- the transcription results. Your transcript appears in the S3 location
-- you specify. When you call the GetMedicalTranscriptionJob, the operation
-- returns this location in the @TranscriptFileUri@ field. The S3 bucket
-- must have permissions that allow Amazon Transcribe Medical to put files
-- in the bucket. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
--
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of your transcription using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe Medical uses the default Amazon S3 key for server-side
-- encryption of transcripts that are placed in your S3 bucket.
startMedicalTranscriptionJob_outputBucketName :: Lens.Lens' StartMedicalTranscriptionJob Prelude.Text
startMedicalTranscriptionJob_outputBucketName = Lens.lens (\StartMedicalTranscriptionJob' {outputBucketName} -> outputBucketName) (\s@StartMedicalTranscriptionJob' {} a -> s {outputBucketName = a} :: StartMedicalTranscriptionJob)

-- | The medical specialty of any clinician speaking in the input media.
startMedicalTranscriptionJob_specialty :: Lens.Lens' StartMedicalTranscriptionJob Specialty
startMedicalTranscriptionJob_specialty = Lens.lens (\StartMedicalTranscriptionJob' {specialty} -> specialty) (\s@StartMedicalTranscriptionJob' {} a -> s {specialty = a} :: StartMedicalTranscriptionJob)

-- | The type of speech in the input audio. @CONVERSATION@ refers to
-- conversations between two or more speakers, e.g., a conversations
-- between doctors and patients. @DICTATION@ refers to single-speaker
-- dictated speech, such as clinical notes.
startMedicalTranscriptionJob_type :: Lens.Lens' StartMedicalTranscriptionJob Type
startMedicalTranscriptionJob_type = Lens.lens (\StartMedicalTranscriptionJob' {type'} -> type') (\s@StartMedicalTranscriptionJob' {} a -> s {type' = a} :: StartMedicalTranscriptionJob)

instance Core.AWSRequest StartMedicalTranscriptionJob where
  type
    AWSResponse StartMedicalTranscriptionJob =
      StartMedicalTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMedicalTranscriptionJobResponse'
            Prelude.<$> (x Core..?> "MedicalTranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartMedicalTranscriptionJob

instance Prelude.NFData StartMedicalTranscriptionJob

instance Core.ToHeaders StartMedicalTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.StartMedicalTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartMedicalTranscriptionJob where
  toJSON StartMedicalTranscriptionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Settings" Core..=) Prelude.<$> settings,
            ("MediaFormat" Core..=) Prelude.<$> mediaFormat,
            ("OutputEncryptionKMSKeyId" Core..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("KMSEncryptionContext" Core..=)
              Prelude.<$> kmsEncryptionContext,
            ("OutputKey" Core..=) Prelude.<$> outputKey,
            ("ContentIdentificationType" Core..=)
              Prelude.<$> contentIdentificationType,
            ("Tags" Core..=) Prelude.<$> tags,
            ("MediaSampleRateHertz" Core..=)
              Prelude.<$> mediaSampleRateHertz,
            Prelude.Just
              ( "MedicalTranscriptionJobName"
                  Core..= medicalTranscriptionJobName
              ),
            Prelude.Just ("LanguageCode" Core..= languageCode),
            Prelude.Just ("Media" Core..= media),
            Prelude.Just
              ("OutputBucketName" Core..= outputBucketName),
            Prelude.Just ("Specialty" Core..= specialty),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath StartMedicalTranscriptionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartMedicalTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMedicalTranscriptionJobResponse' smart constructor.
data StartMedicalTranscriptionJobResponse = StartMedicalTranscriptionJobResponse'
  { -- | A batch job submitted to transcribe medical speech to text.
    medicalTranscriptionJob :: Prelude.Maybe MedicalTranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMedicalTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJob', 'startMedicalTranscriptionJobResponse_medicalTranscriptionJob' - A batch job submitted to transcribe medical speech to text.
--
-- 'httpStatus', 'startMedicalTranscriptionJobResponse_httpStatus' - The response's http status code.
newStartMedicalTranscriptionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMedicalTranscriptionJobResponse
newStartMedicalTranscriptionJobResponse pHttpStatus_ =
  StartMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A batch job submitted to transcribe medical speech to text.
startMedicalTranscriptionJobResponse_medicalTranscriptionJob :: Lens.Lens' StartMedicalTranscriptionJobResponse (Prelude.Maybe MedicalTranscriptionJob)
startMedicalTranscriptionJobResponse_medicalTranscriptionJob = Lens.lens (\StartMedicalTranscriptionJobResponse' {medicalTranscriptionJob} -> medicalTranscriptionJob) (\s@StartMedicalTranscriptionJobResponse' {} a -> s {medicalTranscriptionJob = a} :: StartMedicalTranscriptionJobResponse)

-- | The response's http status code.
startMedicalTranscriptionJobResponse_httpStatus :: Lens.Lens' StartMedicalTranscriptionJobResponse Prelude.Int
startMedicalTranscriptionJobResponse_httpStatus = Lens.lens (\StartMedicalTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@StartMedicalTranscriptionJobResponse' {} a -> s {httpStatus = a} :: StartMedicalTranscriptionJobResponse)

instance
  Prelude.NFData
    StartMedicalTranscriptionJobResponse
