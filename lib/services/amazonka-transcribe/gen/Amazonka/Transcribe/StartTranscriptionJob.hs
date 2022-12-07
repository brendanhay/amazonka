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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transcribes the audio from a media file and applies any additional
-- Request Parameters you choose to include in your request.
--
-- To make a @StartTranscriptionJob@ request, you must first upload your
-- media file into an Amazon S3 bucket; you can then specify the Amazon S3
-- location of the file using the @Media@ parameter.
--
-- You must include the following parameters in your
-- @StartTranscriptionJob@ request:
--
-- -   @region@: The Amazon Web Services Region where you are making your
--     request. For a list of Amazon Web Services Regions supported with
--     Amazon Transcribe, refer to
--     <https://docs.aws.amazon.com/general/latest/gr/transcribe.html Amazon Transcribe endpoints and quotas>.
--
-- -   @TranscriptionJobName@: A custom name you create for your
--     transcription job that is unique within your Amazon Web Services
--     account.
--
-- -   @Media@ (@MediaFileUri@): The Amazon S3 location of your media file.
--
-- -   One of @LanguageCode@, @IdentifyLanguage@, or
--     @IdentifyMultipleLanguages@: If you know the language of your media
--     file, specify it using the @LanguageCode@ parameter; you can find
--     all valid language codes in the
--     <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
--     table. If you don\'t know the languages spoken in your media, use
--     either @IdentifyLanguage@ or @IdentifyMultipleLanguages@ and let
--     Amazon Transcribe identify the languages for you.
module Amazonka.Transcribe.StartTranscriptionJob
  ( -- * Creating a Request
    StartTranscriptionJob (..),
    newStartTranscriptionJob,

    -- * Request Lenses
    startTranscriptionJob_tags,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_identifyMultipleLanguages,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_languageIdSettings,
    startTranscriptionJob_settings,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_languageCode,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_modelSettings,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newStartTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { -- | Adds one or more custom tags, each in the form of a key:value pair, to a
    -- new transcription job at the time you start this new job.
    --
    -- To learn more about using tags with Amazon Transcribe, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A map of plain text, non-secret key:value pairs, known as encryption
    -- context pairs, that provide an added layer of security for your data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
    -- and
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
    kmsEncryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Enables automatic multi-language identification in your transcription
    -- job request. Use this parameter if your media file contains more than
    -- one language.
    --
    -- If you include @IdentifyMultipleLanguages@, you can optionally include a
    -- list of language codes, using @LanguageOptions@, that you think may be
    -- present in your media file. Including language options can improve
    -- transcription accuracy.
    --
    -- If you want to apply a custom vocabulary or a custom vocabulary filter
    -- to your automatic language identification request, include
    -- @LanguageIdSettings@ with the relevant sub-parameters (@VocabularyName@
    -- and @VocabularyFilterName@).
    --
    -- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
    -- @IdentifyMultipleLanguages@ in your request. If you include more than
    -- one of these parameters, your transcription job fails.
    identifyMultipleLanguages :: Prelude.Maybe Prelude.Bool,
    -- | Specify the format of your input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | Enables automatic language identification in your transcription job
    -- request.
    --
    -- If you include @IdentifyLanguage@, you can optionally include a list of
    -- language codes, using @LanguageOptions@, that you think may be present
    -- in your media file. Including language options can improve transcription
    -- accuracy.
    --
    -- If you want to apply a custom language model, a custom vocabulary, or a
    -- custom vocabulary filter to your automatic language identification
    -- request, include @LanguageIdSettings@ with the relevant sub-parameters
    -- (@VocabularyName@, @LanguageModelName@, and @VocabularyFilterName@).
    --
    -- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
    -- @IdentifyMultipleLanguages@ in your request. If you include more than
    -- one of these parameters, your transcription job fails.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | Allows you to redact or flag specified personally identifiable
    -- information (PII) in your transcript. If you use @ContentRedaction@, you
    -- must also include the sub-parameters: @PiiEntityTypes@,
    -- @RedactionOutput@, and @RedactionType@.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | Use in combination with @OutputBucketName@ to specify the output
    -- location of your transcript and, optionally, a unique name for your
    -- output file. The default name for your transcription output is the same
    -- as the name you specified for your transcription job
    -- (@TranscriptionJobName@).
    --
    -- Here are some examples of how you can use @OutputKey@:
    --
    -- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
    --     \'my-transcript.json\' as the @OutputKey@, your transcription output
    --     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
    --
    -- -   If you specify \'my-first-transcription\' as the
    --     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
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
    --     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
    --     @OutputBucketName@, and \'test-files\/my-transcript\' as the
    --     @OutputKey@, your transcription output path is
    --     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
    --
    -- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
    -- exist, one is created for you.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | Produces subtitle files for your input media. You can specify WebVTT
    -- (*.vtt) and SubRip (*.srt) formats.
    subtitles :: Prelude.Maybe Subtitles,
    -- | If using automatic language identification (@IdentifyLanguage@) in your
    -- request and you want to apply a custom language model, a custom
    -- vocabulary, or a custom vocabulary filter, include @LanguageIdSettings@
    -- with the relevant sub-parameters (@VocabularyName@, @LanguageModelName@,
    -- and @VocabularyFilterName@).
    --
    -- You can specify two or more language codes that represent the languages
    -- you think may be present in your media; including more than five is not
    -- recommended. Each language code you include can have an associated
    -- custom language model, custom vocabulary, and custom vocabulary filter.
    -- The languages you specify must match the languages of the specified
    -- custom language models, custom vocabularies, and custom vocabulary
    -- filters.
    --
    -- To include language options using @IdentifyLanguage@ __without__
    -- including a custom language model, a custom vocabulary, or a custom
    -- vocabulary filter, use @LanguageOptions@ instead of
    -- @LanguageIdSettings@. Including language options can improve the
    -- accuracy of automatic language identification.
    --
    -- If you want to include a custom language model with your request but
    -- __do not__ want to use automatic language identification, use instead
    -- the @@ parameter with the @LanguageModelName@ sub-parameter.
    --
    -- If you want to include a custom vocabulary or a custom vocabulary filter
    -- (or both) with your request but __do not__ want to use automatic
    -- language identification, use instead the @@ parameter with the
    -- @VocabularyName@ or @VocabularyFilterName@ (or both) sub-parameter.
    languageIdSettings :: Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings),
    -- | Specify additional optional settings in your request, including channel
    -- identification, alternative transcriptions, speaker labeling; allows you
    -- to apply custom vocabularies and vocabulary filters.
    --
    -- If you want to include a custom vocabulary or a custom vocabulary filter
    -- (or both) with your request but __do not__ want to use automatic
    -- language identification, use @Settings@ with the @VocabularyName@ or
    -- @VocabularyFilterName@ (or both) sub-parameter.
    --
    -- If you\'re using automatic language identification with your request and
    -- want to include a custom language model, a custom vocabulary, or a
    -- custom vocabulary filter, use instead the @@ parameter with the
    -- @LanguageModelName@, @VocabularyName@ or @VocabularyFilterName@
    -- sub-parameters.
    settings :: Prelude.Maybe Settings,
    -- | The sample rate, in Hertz, of the audio track in your input media file.
    --
    -- If you don\'t specify the media sample rate, Amazon Transcribe
    -- determines it for you. If you specify the sample rate, it must match the
    -- rate detected by Amazon Transcribe; if there\'s a mismatch between the
    -- value you specify and the value detected, your job fails. Therefore, in
    -- most cases, it\'s advised to omit @MediaSampleRateHertz@ and let Amazon
    -- Transcribe determine the sample rate.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Amazon S3 bucket where you want your transcription
    -- output stored. Do not include the @S3:\/\/@ prefix of the specified
    -- bucket.
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
    -- If you don\'t specify @OutputBucketName@, your transcript is placed in a
    -- service-managed Amazon S3 bucket and you are provided with a URI to
    -- access your transcript.
    outputBucketName :: Prelude.Maybe Prelude.Text,
    -- | The language code that represents the language spoken in the input media
    -- file.
    --
    -- If you\'re unsure of the language spoken in your media file, consider
    -- using @IdentifyLanguage@ or @IdentifyMultipleLanguages@ to enable
    -- automatic language identification.
    --
    -- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
    -- @IdentifyMultipleLanguages@ in your request. If you include more than
    -- one of these parameters, your transcription job fails.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    --
    -- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
    -- file must be encoded at a sample rate of 16,000 Hz or higher.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Allows you to control how your transcription job is processed.
    -- Currently, the only @JobExecutionSettings@ modification you can choose
    -- is enabling job queueing using the @AllowDeferredExecution@
    -- sub-parameter.
    --
    -- If you include @JobExecutionSettings@ in your request, you must also
    -- include the sub-parameters: @AllowDeferredExecution@ and
    -- @DataAccessRoleArn@.
    jobExecutionSettings :: Prelude.Maybe JobExecutionSettings,
    -- | The KMS key you want to use to encrypt your transcription output.
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
    -- | Specify the custom language model you want to include with your
    -- transcription job. If you include @ModelSettings@ in your request, you
    -- must include the @LanguageModelName@ sub-parameter.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>.
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | You can specify two or more language codes that represent the languages
    -- you think may be present in your media; including more than five is not
    -- recommended. If you\'re unsure what languages are present, do not
    -- include this parameter.
    --
    -- If you include @LanguageOptions@ in your request, you must also include
    -- @IdentifyLanguage@.
    --
    -- For more information, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>.
    --
    -- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
    -- file must be encoded at a sample rate of 16,000 Hz or higher.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | A unique name, chosen by you, for your transcription job. The name you
    -- specify is also used as the default name of your transcription output
    -- file. If you want to specify a different name for your transcription
    -- output, use the @OutputKey@ parameter.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account. If you try to create a new job
    -- with the same name as an existing job, you get a @ConflictException@
    -- error.
    transcriptionJobName :: Prelude.Text,
    -- | Describes the Amazon S3 location of the media file you want to use in
    -- your request.
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
-- 'tags', 'startTranscriptionJob_tags' - Adds one or more custom tags, each in the form of a key:value pair, to a
-- new transcription job at the time you start this new job.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- 'kmsEncryptionContext', 'startTranscriptionJob_kmsEncryptionContext' - A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
-- and
-- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
--
-- 'identifyMultipleLanguages', 'startTranscriptionJob_identifyMultipleLanguages' - Enables automatic multi-language identification in your transcription
-- job request. Use this parameter if your media file contains more than
-- one language.
--
-- If you include @IdentifyMultipleLanguages@, you can optionally include a
-- list of language codes, using @LanguageOptions@, that you think may be
-- present in your media file. Including language options can improve
-- transcription accuracy.
--
-- If you want to apply a custom vocabulary or a custom vocabulary filter
-- to your automatic language identification request, include
-- @LanguageIdSettings@ with the relevant sub-parameters (@VocabularyName@
-- and @VocabularyFilterName@).
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
--
-- 'mediaFormat', 'startTranscriptionJob_mediaFormat' - Specify the format of your input media file.
--
-- 'identifyLanguage', 'startTranscriptionJob_identifyLanguage' - Enables automatic language identification in your transcription job
-- request.
--
-- If you include @IdentifyLanguage@, you can optionally include a list of
-- language codes, using @LanguageOptions@, that you think may be present
-- in your media file. Including language options can improve transcription
-- accuracy.
--
-- If you want to apply a custom language model, a custom vocabulary, or a
-- custom vocabulary filter to your automatic language identification
-- request, include @LanguageIdSettings@ with the relevant sub-parameters
-- (@VocabularyName@, @LanguageModelName@, and @VocabularyFilterName@).
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
--
-- 'contentRedaction', 'startTranscriptionJob_contentRedaction' - Allows you to redact or flag specified personally identifiable
-- information (PII) in your transcript. If you use @ContentRedaction@, you
-- must also include the sub-parameters: @PiiEntityTypes@,
-- @RedactionOutput@, and @RedactionType@.
--
-- 'outputKey', 'startTranscriptionJob_outputKey' - Use in combination with @OutputBucketName@ to specify the output
-- location of your transcript and, optionally, a unique name for your
-- output file. The default name for your transcription output is the same
-- as the name you specified for your transcription job
-- (@TranscriptionJobName@).
--
-- Here are some examples of how you can use @OutputKey@:
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'my-transcript.json\' as the @OutputKey@, your transcription output
--     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
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
--     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'test-files\/my-transcript\' as the
--     @OutputKey@, your transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
--
-- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
-- exist, one is created for you.
--
-- 'subtitles', 'startTranscriptionJob_subtitles' - Produces subtitle files for your input media. You can specify WebVTT
-- (*.vtt) and SubRip (*.srt) formats.
--
-- 'languageIdSettings', 'startTranscriptionJob_languageIdSettings' - If using automatic language identification (@IdentifyLanguage@) in your
-- request and you want to apply a custom language model, a custom
-- vocabulary, or a custom vocabulary filter, include @LanguageIdSettings@
-- with the relevant sub-parameters (@VocabularyName@, @LanguageModelName@,
-- and @VocabularyFilterName@).
--
-- You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. Each language code you include can have an associated
-- custom language model, custom vocabulary, and custom vocabulary filter.
-- The languages you specify must match the languages of the specified
-- custom language models, custom vocabularies, and custom vocabulary
-- filters.
--
-- To include language options using @IdentifyLanguage@ __without__
-- including a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, use @LanguageOptions@ instead of
-- @LanguageIdSettings@. Including language options can improve the
-- accuracy of automatic language identification.
--
-- If you want to include a custom language model with your request but
-- __do not__ want to use automatic language identification, use instead
-- the @@ parameter with the @LanguageModelName@ sub-parameter.
--
-- If you want to include a custom vocabulary or a custom vocabulary filter
-- (or both) with your request but __do not__ want to use automatic
-- language identification, use instead the @@ parameter with the
-- @VocabularyName@ or @VocabularyFilterName@ (or both) sub-parameter.
--
-- 'settings', 'startTranscriptionJob_settings' - Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, speaker labeling; allows you
-- to apply custom vocabularies and vocabulary filters.
--
-- If you want to include a custom vocabulary or a custom vocabulary filter
-- (or both) with your request but __do not__ want to use automatic
-- language identification, use @Settings@ with the @VocabularyName@ or
-- @VocabularyFilterName@ (or both) sub-parameter.
--
-- If you\'re using automatic language identification with your request and
-- want to include a custom language model, a custom vocabulary, or a
-- custom vocabulary filter, use instead the @@ parameter with the
-- @LanguageModelName@, @VocabularyName@ or @VocabularyFilterName@
-- sub-parameters.
--
-- 'mediaSampleRateHertz', 'startTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in your input media file.
--
-- If you don\'t specify the media sample rate, Amazon Transcribe
-- determines it for you. If you specify the sample rate, it must match the
-- rate detected by Amazon Transcribe; if there\'s a mismatch between the
-- value you specify and the value detected, your job fails. Therefore, in
-- most cases, it\'s advised to omit @MediaSampleRateHertz@ and let Amazon
-- Transcribe determine the sample rate.
--
-- 'outputBucketName', 'startTranscriptionJob_outputBucketName' - The name of the Amazon S3 bucket where you want your transcription
-- output stored. Do not include the @S3:\/\/@ prefix of the specified
-- bucket.
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
-- If you don\'t specify @OutputBucketName@, your transcript is placed in a
-- service-managed Amazon S3 bucket and you are provided with a URI to
-- access your transcript.
--
-- 'languageCode', 'startTranscriptionJob_languageCode' - The language code that represents the language spoken in the input media
-- file.
--
-- If you\'re unsure of the language spoken in your media file, consider
-- using @IdentifyLanguage@ or @IdentifyMultipleLanguages@ to enable
-- automatic language identification.
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
--
-- 'jobExecutionSettings', 'startTranscriptionJob_jobExecutionSettings' - Allows you to control how your transcription job is processed.
-- Currently, the only @JobExecutionSettings@ modification you can choose
-- is enabling job queueing using the @AllowDeferredExecution@
-- sub-parameter.
--
-- If you include @JobExecutionSettings@ in your request, you must also
-- include the sub-parameters: @AllowDeferredExecution@ and
-- @DataAccessRoleArn@.
--
-- 'outputEncryptionKMSKeyId', 'startTranscriptionJob_outputEncryptionKMSKeyId' - The KMS key you want to use to encrypt your transcription output.
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
-- 'modelSettings', 'startTranscriptionJob_modelSettings' - Specify the custom language model you want to include with your
-- transcription job. If you include @ModelSettings@ in your request, you
-- must include the @LanguageModelName@ sub-parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>.
--
-- 'languageOptions', 'startTranscriptionJob_languageOptions' - You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- If you include @LanguageOptions@ in your request, you must also include
-- @IdentifyLanguage@.
--
-- For more information, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
--
-- 'transcriptionJobName', 'startTranscriptionJob_transcriptionJobName' - A unique name, chosen by you, for your transcription job. The name you
-- specify is also used as the default name of your transcription output
-- file. If you want to specify a different name for your transcription
-- output, use the @OutputKey@ parameter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new job
-- with the same name as an existing job, you get a @ConflictException@
-- error.
--
-- 'media', 'startTranscriptionJob_media' - Describes the Amazon S3 location of the media file you want to use in
-- your request.
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
      { tags = Prelude.Nothing,
        kmsEncryptionContext = Prelude.Nothing,
        identifyMultipleLanguages = Prelude.Nothing,
        mediaFormat = Prelude.Nothing,
        identifyLanguage = Prelude.Nothing,
        contentRedaction = Prelude.Nothing,
        outputKey = Prelude.Nothing,
        subtitles = Prelude.Nothing,
        languageIdSettings = Prelude.Nothing,
        settings = Prelude.Nothing,
        mediaSampleRateHertz = Prelude.Nothing,
        outputBucketName = Prelude.Nothing,
        languageCode = Prelude.Nothing,
        jobExecutionSettings = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        modelSettings = Prelude.Nothing,
        languageOptions = Prelude.Nothing,
        transcriptionJobName = pTranscriptionJobName_,
        media = pMedia_
      }

-- | Adds one or more custom tags, each in the form of a key:value pair, to a
-- new transcription job at the time you start this new job.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
startTranscriptionJob_tags :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
startTranscriptionJob_tags = Lens.lens (\StartTranscriptionJob' {tags} -> tags) (\s@StartTranscriptionJob' {} a -> s {tags = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | A map of plain text, non-secret key:value pairs, known as encryption
-- context pairs, that provide an added layer of security for your data.
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/key-management.html#kms-context KMS encryption context>
-- and
-- <https://docs.aws.amazon.com/transcribe/latest/dg/symmetric-asymmetric.html Asymmetric keys in KMS>.
startTranscriptionJob_kmsEncryptionContext :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTranscriptionJob_kmsEncryptionContext = Lens.lens (\StartTranscriptionJob' {kmsEncryptionContext} -> kmsEncryptionContext) (\s@StartTranscriptionJob' {} a -> s {kmsEncryptionContext = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Enables automatic multi-language identification in your transcription
-- job request. Use this parameter if your media file contains more than
-- one language.
--
-- If you include @IdentifyMultipleLanguages@, you can optionally include a
-- list of language codes, using @LanguageOptions@, that you think may be
-- present in your media file. Including language options can improve
-- transcription accuracy.
--
-- If you want to apply a custom vocabulary or a custom vocabulary filter
-- to your automatic language identification request, include
-- @LanguageIdSettings@ with the relevant sub-parameters (@VocabularyName@
-- and @VocabularyFilterName@).
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
startTranscriptionJob_identifyMultipleLanguages :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Bool)
startTranscriptionJob_identifyMultipleLanguages = Lens.lens (\StartTranscriptionJob' {identifyMultipleLanguages} -> identifyMultipleLanguages) (\s@StartTranscriptionJob' {} a -> s {identifyMultipleLanguages = a} :: StartTranscriptionJob)

-- | Specify the format of your input media file.
startTranscriptionJob_mediaFormat :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe MediaFormat)
startTranscriptionJob_mediaFormat = Lens.lens (\StartTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@StartTranscriptionJob' {} a -> s {mediaFormat = a} :: StartTranscriptionJob)

-- | Enables automatic language identification in your transcription job
-- request.
--
-- If you include @IdentifyLanguage@, you can optionally include a list of
-- language codes, using @LanguageOptions@, that you think may be present
-- in your media file. Including language options can improve transcription
-- accuracy.
--
-- If you want to apply a custom language model, a custom vocabulary, or a
-- custom vocabulary filter to your automatic language identification
-- request, include @LanguageIdSettings@ with the relevant sub-parameters
-- (@VocabularyName@, @LanguageModelName@, and @VocabularyFilterName@).
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
startTranscriptionJob_identifyLanguage :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Bool)
startTranscriptionJob_identifyLanguage = Lens.lens (\StartTranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@StartTranscriptionJob' {} a -> s {identifyLanguage = a} :: StartTranscriptionJob)

-- | Allows you to redact or flag specified personally identifiable
-- information (PII) in your transcript. If you use @ContentRedaction@, you
-- must also include the sub-parameters: @PiiEntityTypes@,
-- @RedactionOutput@, and @RedactionType@.
startTranscriptionJob_contentRedaction :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ContentRedaction)
startTranscriptionJob_contentRedaction = Lens.lens (\StartTranscriptionJob' {contentRedaction} -> contentRedaction) (\s@StartTranscriptionJob' {} a -> s {contentRedaction = a} :: StartTranscriptionJob)

-- | Use in combination with @OutputBucketName@ to specify the output
-- location of your transcript and, optionally, a unique name for your
-- output file. The default name for your transcription output is the same
-- as the name you specified for your transcription job
-- (@TranscriptionJobName@).
--
-- Here are some examples of how you can use @OutputKey@:
--
-- -   If you specify \'DOC-EXAMPLE-BUCKET\' as the @OutputBucketName@ and
--     \'my-transcript.json\' as the @OutputKey@, your transcription output
--     path is @s3:\/\/DOC-EXAMPLE-BUCKET\/my-transcript.json@.
--
-- -   If you specify \'my-first-transcription\' as the
--     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
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
--     @TranscriptionJobName@, \'DOC-EXAMPLE-BUCKET\' as the
--     @OutputBucketName@, and \'test-files\/my-transcript\' as the
--     @OutputKey@, your transcription output path is
--     @s3:\/\/DOC-EXAMPLE-BUCKET\/test-files\/my-transcript\/my-first-transcription.json@.
--
-- If you specify the name of an Amazon S3 bucket sub-folder that doesn\'t
-- exist, one is created for you.
startTranscriptionJob_outputKey :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputKey = Lens.lens (\StartTranscriptionJob' {outputKey} -> outputKey) (\s@StartTranscriptionJob' {} a -> s {outputKey = a} :: StartTranscriptionJob)

-- | Produces subtitle files for your input media. You can specify WebVTT
-- (*.vtt) and SubRip (*.srt) formats.
startTranscriptionJob_subtitles :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Subtitles)
startTranscriptionJob_subtitles = Lens.lens (\StartTranscriptionJob' {subtitles} -> subtitles) (\s@StartTranscriptionJob' {} a -> s {subtitles = a} :: StartTranscriptionJob)

-- | If using automatic language identification (@IdentifyLanguage@) in your
-- request and you want to apply a custom language model, a custom
-- vocabulary, or a custom vocabulary filter, include @LanguageIdSettings@
-- with the relevant sub-parameters (@VocabularyName@, @LanguageModelName@,
-- and @VocabularyFilterName@).
--
-- You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. Each language code you include can have an associated
-- custom language model, custom vocabulary, and custom vocabulary filter.
-- The languages you specify must match the languages of the specified
-- custom language models, custom vocabularies, and custom vocabulary
-- filters.
--
-- To include language options using @IdentifyLanguage@ __without__
-- including a custom language model, a custom vocabulary, or a custom
-- vocabulary filter, use @LanguageOptions@ instead of
-- @LanguageIdSettings@. Including language options can improve the
-- accuracy of automatic language identification.
--
-- If you want to include a custom language model with your request but
-- __do not__ want to use automatic language identification, use instead
-- the @@ parameter with the @LanguageModelName@ sub-parameter.
--
-- If you want to include a custom vocabulary or a custom vocabulary filter
-- (or both) with your request but __do not__ want to use automatic
-- language identification, use instead the @@ parameter with the
-- @VocabularyName@ or @VocabularyFilterName@ (or both) sub-parameter.
startTranscriptionJob_languageIdSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings))
startTranscriptionJob_languageIdSettings = Lens.lens (\StartTranscriptionJob' {languageIdSettings} -> languageIdSettings) (\s@StartTranscriptionJob' {} a -> s {languageIdSettings = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, speaker labeling; allows you
-- to apply custom vocabularies and vocabulary filters.
--
-- If you want to include a custom vocabulary or a custom vocabulary filter
-- (or both) with your request but __do not__ want to use automatic
-- language identification, use @Settings@ with the @VocabularyName@ or
-- @VocabularyFilterName@ (or both) sub-parameter.
--
-- If you\'re using automatic language identification with your request and
-- want to include a custom language model, a custom vocabulary, or a
-- custom vocabulary filter, use instead the @@ parameter with the
-- @LanguageModelName@, @VocabularyName@ or @VocabularyFilterName@
-- sub-parameters.
startTranscriptionJob_settings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Settings)
startTranscriptionJob_settings = Lens.lens (\StartTranscriptionJob' {settings} -> settings) (\s@StartTranscriptionJob' {} a -> s {settings = a} :: StartTranscriptionJob)

-- | The sample rate, in Hertz, of the audio track in your input media file.
--
-- If you don\'t specify the media sample rate, Amazon Transcribe
-- determines it for you. If you specify the sample rate, it must match the
-- rate detected by Amazon Transcribe; if there\'s a mismatch between the
-- value you specify and the value detected, your job fails. Therefore, in
-- most cases, it\'s advised to omit @MediaSampleRateHertz@ and let Amazon
-- Transcribe determine the sample rate.
startTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Natural)
startTranscriptionJob_mediaSampleRateHertz = Lens.lens (\StartTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@StartTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: StartTranscriptionJob)

-- | The name of the Amazon S3 bucket where you want your transcription
-- output stored. Do not include the @S3:\/\/@ prefix of the specified
-- bucket.
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
-- If you don\'t specify @OutputBucketName@, your transcript is placed in a
-- service-managed Amazon S3 bucket and you are provided with a URI to
-- access your transcript.
startTranscriptionJob_outputBucketName :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputBucketName = Lens.lens (\StartTranscriptionJob' {outputBucketName} -> outputBucketName) (\s@StartTranscriptionJob' {} a -> s {outputBucketName = a} :: StartTranscriptionJob)

-- | The language code that represents the language spoken in the input media
-- file.
--
-- If you\'re unsure of the language spoken in your media file, consider
-- using @IdentifyLanguage@ or @IdentifyMultipleLanguages@ to enable
-- automatic language identification.
--
-- Note that you must include one of @LanguageCode@, @IdentifyLanguage@, or
-- @IdentifyMultipleLanguages@ in your request. If you include more than
-- one of these parameters, your transcription job fails.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
startTranscriptionJob_languageCode :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe LanguageCode)
startTranscriptionJob_languageCode = Lens.lens (\StartTranscriptionJob' {languageCode} -> languageCode) (\s@StartTranscriptionJob' {} a -> s {languageCode = a} :: StartTranscriptionJob)

-- | Allows you to control how your transcription job is processed.
-- Currently, the only @JobExecutionSettings@ modification you can choose
-- is enabling job queueing using the @AllowDeferredExecution@
-- sub-parameter.
--
-- If you include @JobExecutionSettings@ in your request, you must also
-- include the sub-parameters: @AllowDeferredExecution@ and
-- @DataAccessRoleArn@.
startTranscriptionJob_jobExecutionSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe JobExecutionSettings)
startTranscriptionJob_jobExecutionSettings = Lens.lens (\StartTranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@StartTranscriptionJob' {} a -> s {jobExecutionSettings = a} :: StartTranscriptionJob)

-- | The KMS key you want to use to encrypt your transcription output.
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
startTranscriptionJob_outputEncryptionKMSKeyId :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe Prelude.Text)
startTranscriptionJob_outputEncryptionKMSKeyId = Lens.lens (\StartTranscriptionJob' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@StartTranscriptionJob' {} a -> s {outputEncryptionKMSKeyId = a} :: StartTranscriptionJob)

-- | Specify the custom language model you want to include with your
-- transcription job. If you include @ModelSettings@ in your request, you
-- must include the @LanguageModelName@ sub-parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>.
startTranscriptionJob_modelSettings :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe ModelSettings)
startTranscriptionJob_modelSettings = Lens.lens (\StartTranscriptionJob' {modelSettings} -> modelSettings) (\s@StartTranscriptionJob' {} a -> s {modelSettings = a} :: StartTranscriptionJob)

-- | You can specify two or more language codes that represent the languages
-- you think may be present in your media; including more than five is not
-- recommended. If you\'re unsure what languages are present, do not
-- include this parameter.
--
-- If you include @LanguageOptions@ in your request, you must also include
-- @IdentifyLanguage@.
--
-- For more information, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>.
--
-- To transcribe speech in Modern Standard Arabic (@ar-SA@), your media
-- file must be encoded at a sample rate of 16,000 Hz or higher.
startTranscriptionJob_languageOptions :: Lens.Lens' StartTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
startTranscriptionJob_languageOptions = Lens.lens (\StartTranscriptionJob' {languageOptions} -> languageOptions) (\s@StartTranscriptionJob' {} a -> s {languageOptions = a} :: StartTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | A unique name, chosen by you, for your transcription job. The name you
-- specify is also used as the default name of your transcription output
-- file. If you want to specify a different name for your transcription
-- output, use the @OutputKey@ parameter.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new job
-- with the same name as an existing job, you get a @ConflictException@
-- error.
startTranscriptionJob_transcriptionJobName :: Lens.Lens' StartTranscriptionJob Prelude.Text
startTranscriptionJob_transcriptionJobName = Lens.lens (\StartTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@StartTranscriptionJob' {} a -> s {transcriptionJobName = a} :: StartTranscriptionJob)

-- | Describes the Amazon S3 location of the media file you want to use in
-- your request.
startTranscriptionJob_media :: Lens.Lens' StartTranscriptionJob Media
startTranscriptionJob_media = Lens.lens (\StartTranscriptionJob' {media} -> media) (\s@StartTranscriptionJob' {} a -> s {media = a} :: StartTranscriptionJob)

instance Core.AWSRequest StartTranscriptionJob where
  type
    AWSResponse StartTranscriptionJob =
      StartTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTranscriptionJobResponse'
            Prelude.<$> (x Data..?> "TranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTranscriptionJob where
  hashWithSalt _salt StartTranscriptionJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kmsEncryptionContext
      `Prelude.hashWithSalt` identifyMultipleLanguages
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` identifyLanguage
      `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` outputKey
      `Prelude.hashWithSalt` subtitles
      `Prelude.hashWithSalt` languageIdSettings
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` outputBucketName
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` jobExecutionSettings
      `Prelude.hashWithSalt` outputEncryptionKMSKeyId
      `Prelude.hashWithSalt` modelSettings
      `Prelude.hashWithSalt` languageOptions
      `Prelude.hashWithSalt` transcriptionJobName
      `Prelude.hashWithSalt` media

instance Prelude.NFData StartTranscriptionJob where
  rnf StartTranscriptionJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf kmsEncryptionContext
      `Prelude.seq` Prelude.rnf identifyMultipleLanguages
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf identifyLanguage
      `Prelude.seq` Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf outputKey
      `Prelude.seq` Prelude.rnf subtitles
      `Prelude.seq` Prelude.rnf languageIdSettings
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf outputBucketName
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf jobExecutionSettings
      `Prelude.seq` Prelude.rnf outputEncryptionKMSKeyId
      `Prelude.seq` Prelude.rnf modelSettings
      `Prelude.seq` Prelude.rnf languageOptions
      `Prelude.seq` Prelude.rnf transcriptionJobName
      `Prelude.seq` Prelude.rnf media

instance Data.ToHeaders StartTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.StartTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTranscriptionJob where
  toJSON StartTranscriptionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("KMSEncryptionContext" Data..=)
              Prelude.<$> kmsEncryptionContext,
            ("IdentifyMultipleLanguages" Data..=)
              Prelude.<$> identifyMultipleLanguages,
            ("MediaFormat" Data..=) Prelude.<$> mediaFormat,
            ("IdentifyLanguage" Data..=)
              Prelude.<$> identifyLanguage,
            ("ContentRedaction" Data..=)
              Prelude.<$> contentRedaction,
            ("OutputKey" Data..=) Prelude.<$> outputKey,
            ("Subtitles" Data..=) Prelude.<$> subtitles,
            ("LanguageIdSettings" Data..=)
              Prelude.<$> languageIdSettings,
            ("Settings" Data..=) Prelude.<$> settings,
            ("MediaSampleRateHertz" Data..=)
              Prelude.<$> mediaSampleRateHertz,
            ("OutputBucketName" Data..=)
              Prelude.<$> outputBucketName,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("JobExecutionSettings" Data..=)
              Prelude.<$> jobExecutionSettings,
            ("OutputEncryptionKMSKeyId" Data..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("ModelSettings" Data..=) Prelude.<$> modelSettings,
            ("LanguageOptions" Data..=)
              Prelude.<$> languageOptions,
            Prelude.Just
              ( "TranscriptionJobName"
                  Data..= transcriptionJobName
              ),
            Prelude.Just ("Media" Data..= media)
          ]
      )

instance Data.ToPath StartTranscriptionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'
  { -- | Provides detailed information about the current transcription job,
    -- including job status and, if applicable, failure reason.
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
-- 'transcriptionJob', 'startTranscriptionJobResponse_transcriptionJob' - Provides detailed information about the current transcription job,
-- including job status and, if applicable, failure reason.
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

-- | Provides detailed information about the current transcription job,
-- including job status and, if applicable, failure reason.
startTranscriptionJobResponse_transcriptionJob :: Lens.Lens' StartTranscriptionJobResponse (Prelude.Maybe TranscriptionJob)
startTranscriptionJobResponse_transcriptionJob = Lens.lens (\StartTranscriptionJobResponse' {transcriptionJob} -> transcriptionJob) (\s@StartTranscriptionJobResponse' {} a -> s {transcriptionJob = a} :: StartTranscriptionJobResponse)

-- | The response's http status code.
startTranscriptionJobResponse_httpStatus :: Lens.Lens' StartTranscriptionJobResponse Prelude.Int
startTranscriptionJobResponse_httpStatus = Lens.lens (\StartTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@StartTranscriptionJobResponse' {} a -> s {httpStatus = a} :: StartTranscriptionJobResponse)

instance Prelude.NFData StartTranscriptionJobResponse where
  rnf StartTranscriptionJobResponse' {..} =
    Prelude.rnf transcriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
