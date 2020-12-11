{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.StartMedicalTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a batch job to transcribe medical speech to text.
module Network.AWS.Transcribe.StartMedicalTranscriptionJob
  ( -- * Creating a request
    StartMedicalTranscriptionJob (..),
    mkStartMedicalTranscriptionJob,

    -- ** Request lenses
    smtjSettings,
    smtjMediaFormat,
    smtjOutputEncryptionKMSKeyId,
    smtjOutputKey,
    smtjMediaSampleRateHertz,
    smtjMedicalTranscriptionJobName,
    smtjLanguageCode,
    smtjMedia,
    smtjOutputBucketName,
    smtjSpecialty,
    smtjType,

    -- * Destructuring the response
    StartMedicalTranscriptionJobResponse (..),
    mkStartMedicalTranscriptionJobResponse,

    -- ** Response lenses
    smtjrsMedicalTranscriptionJob,
    smtjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkStartMedicalTranscriptionJob' smart constructor.
data StartMedicalTranscriptionJob = StartMedicalTranscriptionJob'
  { settings ::
      Lude.Maybe
        MedicalTranscriptionSetting,
    mediaFormat ::
      Lude.Maybe MediaFormat,
    outputEncryptionKMSKeyId ::
      Lude.Maybe Lude.Text,
    outputKey :: Lude.Maybe Lude.Text,
    mediaSampleRateHertz ::
      Lude.Maybe Lude.Natural,
    medicalTranscriptionJobName ::
      Lude.Text,
    languageCode :: LanguageCode,
    media :: Media,
    outputBucketName :: Lude.Text,
    specialty :: Specialty,
    type' :: Type
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMedicalTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
-- * 'media' - Undocumented field.
-- * 'mediaFormat' - The audio format of the input media file.
-- * 'mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
-- * 'medicalTranscriptionJobName' - The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
-- * 'outputBucketName' - The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
-- * 'outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the 'StartMedicalTranscriptionJob' operation must have permission to use the specified KMS key.
--
-- You use either of the following to identify a KMS key in the current account:
--
--     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * KMS Key Alias: "alias/ExampleAlias"
--
--
-- You can use either of the following to identify a KMS key in the current account or another account:
--
--     * Amazon Resource Name (ARN) of a KMS key in the current account or another account: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias"
--
--
-- If you don't specify an encryption key, the output of the medical transcription job is encrypted with the default Amazon S3 key (SSE-S3).
-- If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
-- * 'outputKey' - You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job.
--
-- If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
-- * 'settings' - Optional settings for the medical transcription job.
-- * 'specialty' - The medical specialty of any clinician speaking in the input media.
-- * 'type'' - The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
mkStartMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'media'
  Media ->
  -- | 'outputBucketName'
  Lude.Text ->
  -- | 'specialty'
  Specialty ->
  -- | 'type''
  Type ->
  StartMedicalTranscriptionJob
mkStartMedicalTranscriptionJob
  pMedicalTranscriptionJobName_
  pLanguageCode_
  pMedia_
  pOutputBucketName_
  pSpecialty_
  pType_ =
    StartMedicalTranscriptionJob'
      { settings = Lude.Nothing,
        mediaFormat = Lude.Nothing,
        outputEncryptionKMSKeyId = Lude.Nothing,
        outputKey = Lude.Nothing,
        mediaSampleRateHertz = Lude.Nothing,
        medicalTranscriptionJobName = pMedicalTranscriptionJobName_,
        languageCode = pLanguageCode_,
        media = pMedia_,
        outputBucketName = pOutputBucketName_,
        specialty = pSpecialty_,
        type' = pType_
      }

-- | Optional settings for the medical transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjSettings :: Lens.Lens' StartMedicalTranscriptionJob (Lude.Maybe MedicalTranscriptionSetting)
smtjSettings = Lens.lens (settings :: StartMedicalTranscriptionJob -> Lude.Maybe MedicalTranscriptionSetting) (\s a -> s {settings = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The audio format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMediaFormat :: Lens.Lens' StartMedicalTranscriptionJob (Lude.Maybe MediaFormat)
smtjMediaFormat = Lens.lens (mediaFormat :: StartMedicalTranscriptionJob -> Lude.Maybe MediaFormat) (\s a -> s {mediaFormat = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the 'StartMedicalTranscriptionJob' operation must have permission to use the specified KMS key.
--
-- You use either of the following to identify a KMS key in the current account:
--
--     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * KMS Key Alias: "alias/ExampleAlias"
--
--
-- You can use either of the following to identify a KMS key in the current account or another account:
--
--     * Amazon Resource Name (ARN) of a KMS key in the current account or another account: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"
--
--
--     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias"
--
--
-- If you don't specify an encryption key, the output of the medical transcription job is encrypted with the default Amazon S3 key (SSE-S3).
-- If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjOutputEncryptionKMSKeyId :: Lens.Lens' StartMedicalTranscriptionJob (Lude.Maybe Lude.Text)
smtjOutputEncryptionKMSKeyId = Lens.lens (outputEncryptionKMSKeyId :: StartMedicalTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {outputEncryptionKMSKeyId = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjOutputEncryptionKMSKeyId "Use generic-lens or generic-optics with 'outputEncryptionKMSKeyId' instead." #-}

-- | You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job.
--
-- If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjOutputKey :: Lens.Lens' StartMedicalTranscriptionJob (Lude.Maybe Lude.Text)
smtjOutputKey = Lens.lens (outputKey :: StartMedicalTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {outputKey = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMediaSampleRateHertz :: Lens.Lens' StartMedicalTranscriptionJob (Lude.Maybe Lude.Natural)
smtjMediaSampleRateHertz = Lens.lens (mediaSampleRateHertz :: StartMedicalTranscriptionJob -> Lude.Maybe Lude.Natural) (\s a -> s {mediaSampleRateHertz = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

-- | The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMedicalTranscriptionJobName :: Lens.Lens' StartMedicalTranscriptionJob Lude.Text
smtjMedicalTranscriptionJobName = Lens.lens (medicalTranscriptionJobName :: StartMedicalTranscriptionJob -> Lude.Text) (\s a -> s {medicalTranscriptionJobName = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

-- | The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjLanguageCode :: Lens.Lens' StartMedicalTranscriptionJob LanguageCode
smtjLanguageCode = Lens.lens (languageCode :: StartMedicalTranscriptionJob -> LanguageCode) (\s a -> s {languageCode = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMedia :: Lens.Lens' StartMedicalTranscriptionJob Media
smtjMedia = Lens.lens (media :: StartMedicalTranscriptionJob -> Media) (\s a -> s {media = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
--
-- /Note:/ Consider using 'outputBucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjOutputBucketName :: Lens.Lens' StartMedicalTranscriptionJob Lude.Text
smtjOutputBucketName = Lens.lens (outputBucketName :: StartMedicalTranscriptionJob -> Lude.Text) (\s a -> s {outputBucketName = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjOutputBucketName "Use generic-lens or generic-optics with 'outputBucketName' instead." #-}

-- | The medical specialty of any clinician speaking in the input media.
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjSpecialty :: Lens.Lens' StartMedicalTranscriptionJob Specialty
smtjSpecialty = Lens.lens (specialty :: StartMedicalTranscriptionJob -> Specialty) (\s a -> s {specialty = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjSpecialty "Use generic-lens or generic-optics with 'specialty' instead." #-}

-- | The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjType :: Lens.Lens' StartMedicalTranscriptionJob Type
smtjType = Lens.lens (type' :: StartMedicalTranscriptionJob -> Type) (\s a -> s {type' = a} :: StartMedicalTranscriptionJob)
{-# DEPRECATED smtjType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest StartMedicalTranscriptionJob where
  type
    Rs StartMedicalTranscriptionJob =
      StartMedicalTranscriptionJobResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMedicalTranscriptionJobResponse'
            Lude.<$> (x Lude..?> "MedicalTranscriptionJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMedicalTranscriptionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.StartMedicalTranscriptionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMedicalTranscriptionJob where
  toJSON StartMedicalTranscriptionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Settings" Lude..=) Lude.<$> settings,
            ("MediaFormat" Lude..=) Lude.<$> mediaFormat,
            ("OutputEncryptionKMSKeyId" Lude..=)
              Lude.<$> outputEncryptionKMSKeyId,
            ("OutputKey" Lude..=) Lude.<$> outputKey,
            ("MediaSampleRateHertz" Lude..=) Lude.<$> mediaSampleRateHertz,
            Lude.Just
              ( "MedicalTranscriptionJobName"
                  Lude..= medicalTranscriptionJobName
              ),
            Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("Media" Lude..= media),
            Lude.Just ("OutputBucketName" Lude..= outputBucketName),
            Lude.Just ("Specialty" Lude..= specialty),
            Lude.Just ("Type" Lude..= type')
          ]
      )

instance Lude.ToPath StartMedicalTranscriptionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMedicalTranscriptionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMedicalTranscriptionJobResponse' smart constructor.
data StartMedicalTranscriptionJobResponse = StartMedicalTranscriptionJobResponse'
  { medicalTranscriptionJob ::
      Lude.Maybe
        MedicalTranscriptionJob,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMedicalTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- * 'medicalTranscriptionJob' - A batch job submitted to transcribe medical speech to text.
-- * 'responseStatus' - The response status code.
mkStartMedicalTranscriptionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMedicalTranscriptionJobResponse
mkStartMedicalTranscriptionJobResponse pResponseStatus_ =
  StartMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A batch job submitted to transcribe medical speech to text.
--
-- /Note:/ Consider using 'medicalTranscriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjrsMedicalTranscriptionJob :: Lens.Lens' StartMedicalTranscriptionJobResponse (Lude.Maybe MedicalTranscriptionJob)
smtjrsMedicalTranscriptionJob = Lens.lens (medicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> Lude.Maybe MedicalTranscriptionJob) (\s a -> s {medicalTranscriptionJob = a} :: StartMedicalTranscriptionJobResponse)
{-# DEPRECATED smtjrsMedicalTranscriptionJob "Use generic-lens or generic-optics with 'medicalTranscriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjrsResponseStatus :: Lens.Lens' StartMedicalTranscriptionJobResponse Lude.Int
smtjrsResponseStatus = Lens.lens (responseStatus :: StartMedicalTranscriptionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMedicalTranscriptionJobResponse)
{-# DEPRECATED smtjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
