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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transcribes the audio from a medical dictation or conversation and
-- applies any additional Request Parameters you choose to include in your
-- request.
--
-- In addition to many standard transcription features, Amazon Transcribe
-- Medical provides you with a robust medical vocabulary and, optionally,
-- content identification, which adds flags to personal health information
-- (PHI). To learn more about these features, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works-med.html How Amazon Transcribe Medical works>.
--
-- To make a @StartMedicalTranscriptionJob@ request, you must first upload
-- your media file into an Amazon S3 bucket; you can then specify the S3
-- location of the file using the @Media@ parameter.
--
-- You must include the following parameters in your
-- @StartMedicalTranscriptionJob@ request:
--
-- -   @region@: The Amazon Web Services Region where you are making your
--     request. For a list of Amazon Web Services Regions supported with
--     Amazon Transcribe, refer to
--     <https://docs.aws.amazon.com/general/latest/gr/transcribe.html Amazon Transcribe endpoints and quotas>.
--
-- -   @MedicalTranscriptionJobName@: A custom name you create for your
--     transcription job that is unique within your Amazon Web Services
--     account.
--
-- -   @Media@ (@MediaFileUri@): The Amazon S3 location of your media file.
--
-- -   @LanguageCode@: This must be @en-US@.
--
-- -   @OutputBucketName@: The Amazon S3 bucket where you want your
--     transcript stored. If you want your output stored in a sub-folder of
--     this bucket, you must also include @OutputKey@.
--
-- -   @Specialty@: This must be @PRIMARYCARE@.
--
-- -   @Type@: Choose whether your audio is a conversation or a dictation.
module Amazonka.Transcribe.StartMedicalTranscriptionJob
  ( -- * Creating a Request
    StartMedicalTranscriptionJob (..),
    newStartMedicalTranscriptionJob,

    -- * Request Lenses
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_tags,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newStartMedicalTranscriptionJob' smart constructor.
data StartMedicalTranscriptionJob = StartMedicalTranscriptionJob'
  { -- | Labels all personal health information (PHI) identified in your
    -- transcript. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
    contentIdentificationType :: Prelude.Maybe MedicalContentIdentificationType,
    -- | A map of plain text, non-secret key:value pairs, known as encryption
    -- context pairs, that provide an added layer of security for your data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
    -- and
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
    kmsEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specify the format of your input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The sample rate, in hertz, of the audio track in your input media file.
    --
    -- If you don\'t specify the media sample rate, Amazon Transcribe Medical
    -- determines it for you. If you specify the sample rate, it must match the
    -- rate detected by Amazon Transcribe Medical; if there\'s a mismatch
    -- between the value that you specify and the value detected, your job
    -- fails. Therefore, in most cases, it\'s advised to omit
    -- @MediaSampleRateHertz@ and let Amazon Transcribe Medical determine the
    -- sample rate.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The KMS key you want to use to encrypt your medical transcription
    -- output.
    --
    -- If using a key located in the __current__ Amazon Web Services account,
    -- you can specify your KMS key in one of four ways:
    --
    -- 1.  Use the KMS key ID itself. For example,
    --     @1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- 2.  Use an alias for the KMS key ID. For example, @alias\/ExampleAlias@.
    --
    -- 3.  Use the Amazon Resource Name (ARN) for the KMS key ID. For example,
    --     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- 4.  Use the ARN for the KMS key alias. For example,
    --     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
    --
    -- If using a key located in a __different__ Amazon Web Services account
    -- than the current Amazon Web Services account, you can specify your KMS
    -- key in one of two ways:
    --
    -- 1.  Use the ARN for the KMS key ID. For example,
    --     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- 2.  Use the ARN for the KMS key alias. For example,
    --     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
    --
    -- If you don\'t specify an encryption key, your output is encrypted with
    -- the default Amazon S3 key (SSE-S3).
    --
    -- If you specify a KMS key to encrypt your output, you must also specify
    -- an output location using the @OutputLocation@ parameter.
    --
    -- Note that the user making the request must have permission to use the
    -- specified KMS key.
    outputEncryptionKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | Use in combination with @OutputBucketName@ to specify the output
    -- location of your transcript and, optionally, a unique name for your
    -- output file. The default name for your transcription output is the same
    -- as the name you specified for your medical transcription job
    -- (@MedicalTranscriptionJobName@).
    --
    -- Here are some examples of how you can use @OutputKey@:
    --
    -- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
    --     \'my-transcript.json\' as the @OutputKey@, your transcription output
    --     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
    --
    -- -   If you specify \'my-first-transcription\' as the
    --     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
    --     @OutputBucketName@, and \'my-transcript\' as the @OutputKey@, your
    --     transcription output path is
    --     @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript\/my-first-transcription.json@.
    --
    -- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
    --     \'test-files\/my-transcript.json\' as the @OutputKey@, your
    --     transcription output path is
    --     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript.json@.
    --
    -- -   If you specify \'my-first-transcription\' as the
    --     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
    --     @OutputBucketName@, and \'test-files\/my-transcript\' as the
    --     @OutputKey@, your transcription output path is
    --     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
    --
    -- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
    -- exist, one is created for you.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | Specify additional optional settings in your request, including channel
    -- identification, alternative transcriptions, and speaker partitioning.
    -- You can use that to apply custom vocabularies to your transcription job.
    settings :: Prelude.Maybe MedicalTranscriptionSetting,
    -- | Adds one or more custom tags, each in the form of a key:value pair, to a
    -- new medical transcription job at the time you start this new job.
    --
    -- To learn more about using tags with Amazon Transcribe, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A unique name, chosen by you, for your medical transcription job. The
    -- name that you specify is also used as the default name of your
    -- transcription output file. If you want to specify a different name for
    -- your transcription output, use the @OutputKey@ parameter.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account. If you try to create a new job
    -- with the same name as an existing job, you get a @ConflictException@
    -- error.
    medicalTranscriptionJobName :: Prelude.Text,
    -- | The language code that represents the language spoken in the input media
    -- file. US English (@en-US@) is the only valid value for medical
    -- transcription jobs. Any other value you enter for language code results
    -- in a @BadRequestException@ error.
    languageCode :: LanguageCode,
    media :: Media,
    -- | The name of the Amazon S3 bucket where you want your medical
    -- transcription output stored. Do not include the @S3:\/\/@ prefix of the
    -- specified bucket.
    --
    -- If you want your output to go to a sub-folder of this bucket, specify it
    -- using the @OutputKey@ parameter; @OutputBucketName@ only accepts the
    -- name of a bucket.
    --
    -- For example, if you want your output stored in
    -- @S3:\/\/DOC-EXAMPLE-BUCKET@, set @OutputBucketName@ to
    -- @DOC-EXAMPLE-BUCKET@. However, if you want your output stored in
    -- @S3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/@, set @OutputBucketName@ to
    -- @DOC-EXAMPLE-BUCKET@ and @OutputKey@ to @test-files\/@.
    --
    -- Note that Amazon Transcribe must have permission to use the specified
    -- location. You can change Amazon S3 permissions using the
    -- <https://console.aws.amazon.com/s3 Amazon Web Services Management Console>.
    -- See also
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
    outputBucketName :: Prelude.Text,
    -- | Specify the predominant medical specialty represented in your media. For
    -- batch transcriptions, @PRIMARYCARE@ is the only valid value. If you
    -- require additional specialties, refer to .
    specialty :: Specialty,
    -- | Specify whether your input media contains only one person (@DICTATION@)
    -- or contains a conversation between two people (@CONVERSATION@).
    --
    -- For example, @DICTATION@ could be used for a medical professional
    -- wanting to transcribe voice memos; @CONVERSATION@ could be used for
    -- transcribing the doctor-patient dialogue during the patient\'s office
    -- visit.
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
-- 'contentIdentificationType', 'startMedicalTranscriptionJob_contentIdentificationType' - Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
--
-- 'kmsEncryptionContext', 'startMedicalTranscriptionJob_kmsEncryptionContext' - A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
-- and
-- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
--
-- 'mediaFormat', 'startMedicalTranscriptionJob_mediaFormat' - Specify the format of your input media file.
--
-- 'mediaSampleRateHertz', 'startMedicalTranscriptionJob_mediaSampleRateHertz' - The sample rate, in hertz, of the audio track in your input media file.
--
-- If you don\'t specify the media sample rate, Amazon Transcribe Medical
-- determines it for you. If you specify the sample rate, it must match the
-- rate detected by Amazon Transcribe Medical; if there\'s a mismatch
-- between the value that you specify and the value detected, your job
-- fails. Therefore, in most cases, it\'s advised to omit
-- @MediaSampleRateHertz@ and let Amazon Transcribe Medical determine the
-- sample rate.
--
-- 'outputEncryptionKMSKeyId', 'startMedicalTranscriptionJob_outputEncryptionKMSKeyId' - The KMS key you want to use to encrypt your medical transcription
-- output.
--
-- If using a key located in the __current__ Amazon Web Services account,
-- you can specify your KMS key in one of four ways:
--
-- 1.  Use the KMS key ID itself. For example,
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 2.  Use an alias for the KMS key ID. For example, @alias\/ExampleAlias@.
--
-- 3.  Use the Amazon Resource Name (ARN) for the KMS key ID. For example,
--     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 4.  Use the ARN for the KMS key alias. For example,
--     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
--
-- If using a key located in a __different__ Amazon Web Services account
-- than the current Amazon Web Services account, you can specify your KMS
-- key in one of two ways:
--
-- 1.  Use the ARN for the KMS key ID. For example,
--     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 2.  Use the ARN for the KMS key alias. For example,
--     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
--
-- If you don\'t specify an encryption key, your output is encrypted with
-- the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location using the @OutputLocation@ parameter.
--
-- Note that the user making the request must have permission to use the
-- specified KMS key.
--
-- 'outputKey', 'startMedicalTranscriptionJob_outputKey' - Use in combination with @OutputBucketName@ to specify the output
-- location of your transcript and, optionally, a unique name for your
-- output file. The default name for your transcription output is the same
-- as the name you specified for your medical transcription job
-- (@MedicalTranscriptionJobName@).
--
-- Here are some examples of how you can use @OutputKey@:
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'my-transcript.json\' as the @OutputKey@, your transcription output
--     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'my-transcript\' as the @OutputKey@, your
--     transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript\/my-first-transcription.json@.
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'test-files\/my-transcript.json\' as the @OutputKey@, your
--     transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'test-files\/my-transcript\' as the
--     @OutputKey@, your transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
--
-- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
-- exist, one is created for you.
--
-- 'settings', 'startMedicalTranscriptionJob_settings' - Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, and speaker partitioning.
-- You can use that to apply custom vocabularies to your transcription job.
--
-- 'tags', 'startMedicalTranscriptionJob_tags' - Adds one or more custom tags, each in the form of a key:value pair, to a
-- new medical transcription job at the time you start this new job.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- 'medicalTranscriptionJobName', 'startMedicalTranscriptionJob_medicalTranscriptionJobName' - A unique name, chosen by you, for your medical transcription job. The
-- name that you specify is also used as the default name of your
-- transcription output file. If you want to specify a different name for
-- your transcription output, use the @OutputKey@ parameter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new job
-- with the same name as an existing job, you get a @ConflictException@
-- error.
--
-- 'languageCode', 'startMedicalTranscriptionJob_languageCode' - The language code that represents the language spoken in the input media
-- file. US English (@en-US@) is the only valid value for medical
-- transcription jobs. Any other value you enter for language code results
-- in a @BadRequestException@ error.
--
-- 'media', 'startMedicalTranscriptionJob_media' - Undocumented member.
--
-- 'outputBucketName', 'startMedicalTranscriptionJob_outputBucketName' - The name of the Amazon S3 bucket where you want your medical
-- transcription output stored. Do not include the @S3:\/\/@ prefix of the
-- specified bucket.
--
-- If you want your output to go to a sub-folder of this bucket, specify it
-- using the @OutputKey@ parameter; @OutputBucketName@ only accepts the
-- name of a bucket.
--
-- For example, if you want your output stored in
-- @S3:\/\/DOC-EXAMPLE-BUCKET@, set @OutputBucketName@ to
-- @DOC-EXAMPLE-BUCKET@. However, if you want your output stored in
-- @S3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/@, set @OutputBucketName@ to
-- @DOC-EXAMPLE-BUCKET@ and @OutputKey@ to @test-files\/@.
--
-- Note that Amazon Transcribe must have permission to use the specified
-- location. You can change Amazon S3 permissions using the
-- <https://console.aws.amazon.com/s3 Amazon Web Services Management Console>.
-- See also
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
--
-- 'specialty', 'startMedicalTranscriptionJob_specialty' - Specify the predominant medical specialty represented in your media. For
-- batch transcriptions, @PRIMARYCARE@ is the only valid value. If you
-- require additional specialties, refer to .
--
-- 'type'', 'startMedicalTranscriptionJob_type' - Specify whether your input media contains only one person (@DICTATION@)
-- or contains a conversation between two people (@CONVERSATION@).
--
-- For example, @DICTATION@ could be used for a medical professional
-- wanting to transcribe voice memos; @CONVERSATION@ could be used for
-- transcribing the doctor-patient dialogue during the patient\'s office
-- visit.
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
      { contentIdentificationType =
          Prelude.Nothing,
        kmsEncryptionContext = Prelude.Nothing,
        mediaFormat = Prelude.Nothing,
        mediaSampleRateHertz = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        outputKey = Prelude.Nothing,
        settings = Prelude.Nothing,
        tags = Prelude.Nothing,
        medicalTranscriptionJobName =
          pMedicalTranscriptionJobName_,
        languageCode = pLanguageCode_,
        media = pMedia_,
        outputBucketName = pOutputBucketName_,
        specialty = pSpecialty_,
        type' = pType_
      }

-- | Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
startMedicalTranscriptionJob_contentIdentificationType :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MedicalContentIdentificationType)
startMedicalTranscriptionJob_contentIdentificationType = Lens.lens (\StartMedicalTranscriptionJob' {contentIdentificationType} -> contentIdentificationType) (\s@StartMedicalTranscriptionJob' {} a -> s {contentIdentificationType = a} :: StartMedicalTranscriptionJob)

-- | A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
-- and
-- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
startMedicalTranscriptionJob_kmsEncryptionContext :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startMedicalTranscriptionJob_kmsEncryptionContext = Lens.lens (\StartMedicalTranscriptionJob' {kmsEncryptionContext} -> kmsEncryptionContext) (\s@StartMedicalTranscriptionJob' {} a -> s {kmsEncryptionContext = a} :: StartMedicalTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Specify the format of your input media file.
startMedicalTranscriptionJob_mediaFormat :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MediaFormat)
startMedicalTranscriptionJob_mediaFormat = Lens.lens (\StartMedicalTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@StartMedicalTranscriptionJob' {} a -> s {mediaFormat = a} :: StartMedicalTranscriptionJob)

-- | The sample rate, in hertz, of the audio track in your input media file.
--
-- If you don\'t specify the media sample rate, Amazon Transcribe Medical
-- determines it for you. If you specify the sample rate, it must match the
-- rate detected by Amazon Transcribe Medical; if there\'s a mismatch
-- between the value that you specify and the value detected, your job
-- fails. Therefore, in most cases, it\'s advised to omit
-- @MediaSampleRateHertz@ and let Amazon Transcribe Medical determine the
-- sample rate.
startMedicalTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Natural)
startMedicalTranscriptionJob_mediaSampleRateHertz = Lens.lens (\StartMedicalTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@StartMedicalTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: StartMedicalTranscriptionJob)

-- | The KMS key you want to use to encrypt your medical transcription
-- output.
--
-- If using a key located in the __current__ Amazon Web Services account,
-- you can specify your KMS key in one of four ways:
--
-- 1.  Use the KMS key ID itself. For example,
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 2.  Use an alias for the KMS key ID. For example, @alias\/ExampleAlias@.
--
-- 3.  Use the Amazon Resource Name (ARN) for the KMS key ID. For example,
--     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 4.  Use the ARN for the KMS key alias. For example,
--     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
--
-- If using a key located in a __different__ Amazon Web Services account
-- than the current Amazon Web Services account, you can specify your KMS
-- key in one of two ways:
--
-- 1.  Use the ARN for the KMS key ID. For example,
--     @arn:aws:kms:region:account-ID:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 2.  Use the ARN for the KMS key alias. For example,
--     @arn:aws:kms:region:account-ID:alias\/ExampleAlias@.
--
-- If you don\'t specify an encryption key, your output is encrypted with
-- the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location using the @OutputLocation@ parameter.
--
-- Note that the user making the request must have permission to use the
-- specified KMS key.
startMedicalTranscriptionJob_outputEncryptionKMSKeyId :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
startMedicalTranscriptionJob_outputEncryptionKMSKeyId = Lens.lens (\StartMedicalTranscriptionJob' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@StartMedicalTranscriptionJob' {} a -> s {outputEncryptionKMSKeyId = a} :: StartMedicalTranscriptionJob)

-- | Use in combination with @OutputBucketName@ to specify the output
-- location of your transcript and, optionally, a unique name for your
-- output file. The default name for your transcription output is the same
-- as the name you specified for your medical transcription job
-- (@MedicalTranscriptionJobName@).
--
-- Here are some examples of how you can use @OutputKey@:
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'my-transcript.json\' as the @OutputKey@, your transcription output
--     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'my-transcript\' as the @OutputKey@, your
--     transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript\/my-first-transcription.json@.
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'test-files\/my-transcript.json\' as the @OutputKey@, your
--     transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @MedicalTranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'test-files\/my-transcript\' as the
--     @OutputKey@, your transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
--
-- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
-- exist, one is created for you.
startMedicalTranscriptionJob_outputKey :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
startMedicalTranscriptionJob_outputKey = Lens.lens (\StartMedicalTranscriptionJob' {outputKey} -> outputKey) (\s@StartMedicalTranscriptionJob' {} a -> s {outputKey = a} :: StartMedicalTranscriptionJob)

-- | Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, and speaker partitioning.
-- You can use that to apply custom vocabularies to your transcription job.
startMedicalTranscriptionJob_settings :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe MedicalTranscriptionSetting)
startMedicalTranscriptionJob_settings = Lens.lens (\StartMedicalTranscriptionJob' {settings} -> settings) (\s@StartMedicalTranscriptionJob' {} a -> s {settings = a} :: StartMedicalTranscriptionJob)

-- | Adds one or more custom tags, each in the form of a key:value pair, to a
-- new medical transcription job at the time you start this new job.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
startMedicalTranscriptionJob_tags :: Lens.Lens' StartMedicalTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
startMedicalTranscriptionJob_tags = Lens.lens (\StartMedicalTranscriptionJob' {tags} -> tags) (\s@StartMedicalTranscriptionJob' {} a -> s {tags = a} :: StartMedicalTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | A unique name, chosen by you, for your medical transcription job. The
-- name that you specify is also used as the default name of your
-- transcription output file. If you want to specify a different name for
-- your transcription output, use the @OutputKey@ parameter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new job
-- with the same name as an existing job, you get a @ConflictException@
-- error.
startMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' StartMedicalTranscriptionJob Prelude.Text
startMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\StartMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@StartMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: StartMedicalTranscriptionJob)

-- | The language code that represents the language spoken in the input media
-- file. US English (@en-US@) is the only valid value for medical
-- transcription jobs. Any other value you enter for language code results
-- in a @BadRequestException@ error.
startMedicalTranscriptionJob_languageCode :: Lens.Lens' StartMedicalTranscriptionJob LanguageCode
startMedicalTranscriptionJob_languageCode = Lens.lens (\StartMedicalTranscriptionJob' {languageCode} -> languageCode) (\s@StartMedicalTranscriptionJob' {} a -> s {languageCode = a} :: StartMedicalTranscriptionJob)

-- | Undocumented member.
startMedicalTranscriptionJob_media :: Lens.Lens' StartMedicalTranscriptionJob Media
startMedicalTranscriptionJob_media = Lens.lens (\StartMedicalTranscriptionJob' {media} -> media) (\s@StartMedicalTranscriptionJob' {} a -> s {media = a} :: StartMedicalTranscriptionJob)

-- | The name of the Amazon S3 bucket where you want your medical
-- transcription output stored. Do not include the @S3:\/\/@ prefix of the
-- specified bucket.
--
-- If you want your output to go to a sub-folder of this bucket, specify it
-- using the @OutputKey@ parameter; @OutputBucketName@ only accepts the
-- name of a bucket.
--
-- For example, if you want your output stored in
-- @S3:\/\/DOC-EXAMPLE-BUCKET@, set @OutputBucketName@ to
-- @DOC-EXAMPLE-BUCKET@. However, if you want your output stored in
-- @S3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/@, set @OutputBucketName@ to
-- @DOC-EXAMPLE-BUCKET@ and @OutputKey@ to @test-files\/@.
--
-- Note that Amazon Transcribe must have permission to use the specified
-- location. You can change Amazon S3 permissions using the
-- <https://console.aws.amazon.com/s3 Amazon Web Services Management Console>.
-- See also
-- <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles>.
startMedicalTranscriptionJob_outputBucketName :: Lens.Lens' StartMedicalTranscriptionJob Prelude.Text
startMedicalTranscriptionJob_outputBucketName = Lens.lens (\StartMedicalTranscriptionJob' {outputBucketName} -> outputBucketName) (\s@StartMedicalTranscriptionJob' {} a -> s {outputBucketName = a} :: StartMedicalTranscriptionJob)

-- | Specify the predominant medical specialty represented in your media. For
-- batch transcriptions, @PRIMARYCARE@ is the only valid value. If you
-- require additional specialties, refer to .
startMedicalTranscriptionJob_specialty :: Lens.Lens' StartMedicalTranscriptionJob Specialty
startMedicalTranscriptionJob_specialty = Lens.lens (\StartMedicalTranscriptionJob' {specialty} -> specialty) (\s@StartMedicalTranscriptionJob' {} a -> s {specialty = a} :: StartMedicalTranscriptionJob)

-- | Specify whether your input media contains only one person (@DICTATION@)
-- or contains a conversation between two people (@CONVERSATION@).
--
-- For example, @DICTATION@ could be used for a medical professional
-- wanting to transcribe voice memos; @CONVERSATION@ could be used for
-- transcribing the doctor-patient dialogue during the patient\'s office
-- visit.
startMedicalTranscriptionJob_type :: Lens.Lens' StartMedicalTranscriptionJob Type
startMedicalTranscriptionJob_type = Lens.lens (\StartMedicalTranscriptionJob' {type'} -> type') (\s@StartMedicalTranscriptionJob' {} a -> s {type' = a} :: StartMedicalTranscriptionJob)

instance Core.AWSRequest StartMedicalTranscriptionJob where
  type
    AWSResponse StartMedicalTranscriptionJob =
      StartMedicalTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMedicalTranscriptionJobResponse'
            Prelude.<$> (x Data..?> "MedicalTranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartMedicalTranscriptionJob
  where
  hashWithSalt _salt StartMedicalTranscriptionJob' {..} =
    _salt
      `Prelude.hashWithSalt` contentIdentificationType
      `Prelude.hashWithSalt` kmsEncryptionContext
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` outputEncryptionKMSKeyId
      `Prelude.hashWithSalt` outputKey
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` medicalTranscriptionJobName
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` outputBucketName
      `Prelude.hashWithSalt` specialty
      `Prelude.hashWithSalt` type'

instance Prelude.NFData StartMedicalTranscriptionJob where
  rnf StartMedicalTranscriptionJob' {..} =
    Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf kmsEncryptionContext
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf outputEncryptionKMSKeyId
      `Prelude.seq` Prelude.rnf outputKey
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf medicalTranscriptionJobName
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf media
      `Prelude.seq` Prelude.rnf outputBucketName
      `Prelude.seq` Prelude.rnf specialty
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders StartMedicalTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.StartMedicalTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMedicalTranscriptionJob where
  toJSON StartMedicalTranscriptionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentIdentificationType" Data..=)
              Prelude.<$> contentIdentificationType,
            ("KMSEncryptionContext" Data..=)
              Prelude.<$> kmsEncryptionContext,
            ("MediaFormat" Data..=) Prelude.<$> mediaFormat,
            ("MediaSampleRateHertz" Data..=)
              Prelude.<$> mediaSampleRateHertz,
            ("OutputEncryptionKMSKeyId" Data..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("OutputKey" Data..=) Prelude.<$> outputKey,
            ("Settings" Data..=) Prelude.<$> settings,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "MedicalTranscriptionJobName"
                  Data..= medicalTranscriptionJobName
              ),
            Prelude.Just ("LanguageCode" Data..= languageCode),
            Prelude.Just ("Media" Data..= media),
            Prelude.Just
              ("OutputBucketName" Data..= outputBucketName),
            Prelude.Just ("Specialty" Data..= specialty),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath StartMedicalTranscriptionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMedicalTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMedicalTranscriptionJobResponse' smart constructor.
data StartMedicalTranscriptionJobResponse = StartMedicalTranscriptionJobResponse'
  { -- | Provides detailed information about the current medical transcription
    -- job, including job status and, if applicable, failure reason.
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
-- 'medicalTranscriptionJob', 'startMedicalTranscriptionJobResponse_medicalTranscriptionJob' - Provides detailed information about the current medical transcription
-- job, including job status and, if applicable, failure reason.
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

-- | Provides detailed information about the current medical transcription
-- job, including job status and, if applicable, failure reason.
startMedicalTranscriptionJobResponse_medicalTranscriptionJob :: Lens.Lens' StartMedicalTranscriptionJobResponse (Prelude.Maybe MedicalTranscriptionJob)
startMedicalTranscriptionJobResponse_medicalTranscriptionJob = Lens.lens (\StartMedicalTranscriptionJobResponse' {medicalTranscriptionJob} -> medicalTranscriptionJob) (\s@StartMedicalTranscriptionJobResponse' {} a -> s {medicalTranscriptionJob = a} :: StartMedicalTranscriptionJobResponse)

-- | The response's http status code.
startMedicalTranscriptionJobResponse_httpStatus :: Lens.Lens' StartMedicalTranscriptionJobResponse Prelude.Int
startMedicalTranscriptionJobResponse_httpStatus = Lens.lens (\StartMedicalTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@StartMedicalTranscriptionJobResponse' {} a -> s {httpStatus = a} :: StartMedicalTranscriptionJobResponse)

instance
  Prelude.NFData
    StartMedicalTranscriptionJobResponse
  where
  rnf StartMedicalTranscriptionJobResponse' {..} =
    Prelude.rnf medicalTranscriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
