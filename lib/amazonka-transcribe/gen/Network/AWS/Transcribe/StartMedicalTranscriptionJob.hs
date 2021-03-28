{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartMedicalTranscriptionJob (..)
    , mkStartMedicalTranscriptionJob
    -- ** Request lenses
    , smtjMedicalTranscriptionJobName
    , smtjLanguageCode
    , smtjMedia
    , smtjOutputBucketName
    , smtjSpecialty
    , smtjType
    , smtjMediaFormat
    , smtjMediaSampleRateHertz
    , smtjOutputEncryptionKMSKeyId
    , smtjOutputKey
    , smtjSettings

    -- * Destructuring the response
    , StartMedicalTranscriptionJobResponse (..)
    , mkStartMedicalTranscriptionJobResponse
    -- ** Response lenses
    , smtjrrsMedicalTranscriptionJob
    , smtjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkStartMedicalTranscriptionJob' smart constructor.
data StartMedicalTranscriptionJob = StartMedicalTranscriptionJob'
  { medicalTranscriptionJobName :: Types.MedicalTranscriptionJobName
    -- ^ The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
  , languageCode :: Types.LanguageCode
    -- ^ The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
  , media :: Types.Media
  , outputBucketName :: Types.OutputBucketName
    -- ^ The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
  , specialty :: Types.Specialty
    -- ^ The medical specialty of any clinician speaking in the input media.
  , type' :: Types.Type
    -- ^ The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
  , mediaFormat :: Core.Maybe Types.MediaFormat
    -- ^ The audio format of the input media file.
  , mediaSampleRateHertz :: Core.Maybe Core.Natural
    -- ^ The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
  , outputEncryptionKMSKeyId :: Core.Maybe Types.KMSKeyId
    -- ^ The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the 'StartMedicalTranscriptionJob' operation must have permission to use the specified KMS key.
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
  , outputKey :: Core.Maybe Types.OutputKey
    -- ^ You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job.
--
-- If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
  , settings :: Core.Maybe Types.MedicalTranscriptionSetting
    -- ^ Optional settings for the medical transcription job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMedicalTranscriptionJob' value with any optional fields omitted.
mkStartMedicalTranscriptionJob
    :: Types.MedicalTranscriptionJobName -- ^ 'medicalTranscriptionJobName'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> Types.Media -- ^ 'media'
    -> Types.OutputBucketName -- ^ 'outputBucketName'
    -> Types.Specialty -- ^ 'specialty'
    -> Types.Type -- ^ 'type\''
    -> StartMedicalTranscriptionJob
mkStartMedicalTranscriptionJob medicalTranscriptionJobName
  languageCode media outputBucketName specialty type'
  = StartMedicalTranscriptionJob'{medicalTranscriptionJobName,
                                  languageCode, media, outputBucketName, specialty, type',
                                  mediaFormat = Core.Nothing, mediaSampleRateHertz = Core.Nothing,
                                  outputEncryptionKMSKeyId = Core.Nothing, outputKey = Core.Nothing,
                                  settings = Core.Nothing}

-- | The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMedicalTranscriptionJobName :: Lens.Lens' StartMedicalTranscriptionJob Types.MedicalTranscriptionJobName
smtjMedicalTranscriptionJobName = Lens.field @"medicalTranscriptionJobName"
{-# INLINEABLE smtjMedicalTranscriptionJobName #-}
{-# DEPRECATED medicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead"  #-}

-- | The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjLanguageCode :: Lens.Lens' StartMedicalTranscriptionJob Types.LanguageCode
smtjLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE smtjLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMedia :: Lens.Lens' StartMedicalTranscriptionJob Types.Media
smtjMedia = Lens.field @"media"
{-# INLINEABLE smtjMedia #-}
{-# DEPRECATED media "Use generic-lens or generic-optics with 'media' instead"  #-}

-- | The Amazon S3 location where the transcription is stored.
--
-- You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> .
-- You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
--
-- /Note:/ Consider using 'outputBucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjOutputBucketName :: Lens.Lens' StartMedicalTranscriptionJob Types.OutputBucketName
smtjOutputBucketName = Lens.field @"outputBucketName"
{-# INLINEABLE smtjOutputBucketName #-}
{-# DEPRECATED outputBucketName "Use generic-lens or generic-optics with 'outputBucketName' instead"  #-}

-- | The medical specialty of any clinician speaking in the input media.
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjSpecialty :: Lens.Lens' StartMedicalTranscriptionJob Types.Specialty
smtjSpecialty = Lens.field @"specialty"
{-# INLINEABLE smtjSpecialty #-}
{-# DEPRECATED specialty "Use generic-lens or generic-optics with 'specialty' instead"  #-}

-- | The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjType :: Lens.Lens' StartMedicalTranscriptionJob Types.Type
smtjType = Lens.field @"type'"
{-# INLINEABLE smtjType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The audio format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMediaFormat :: Lens.Lens' StartMedicalTranscriptionJob (Core.Maybe Types.MediaFormat)
smtjMediaFormat = Lens.field @"mediaFormat"
{-# INLINEABLE smtjMediaFormat #-}
{-# DEPRECATED mediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead"  #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjMediaSampleRateHertz :: Lens.Lens' StartMedicalTranscriptionJob (Core.Maybe Core.Natural)
smtjMediaSampleRateHertz = Lens.field @"mediaSampleRateHertz"
{-# INLINEABLE smtjMediaSampleRateHertz #-}
{-# DEPRECATED mediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead"  #-}

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
smtjOutputEncryptionKMSKeyId :: Lens.Lens' StartMedicalTranscriptionJob (Core.Maybe Types.KMSKeyId)
smtjOutputEncryptionKMSKeyId = Lens.field @"outputEncryptionKMSKeyId"
{-# INLINEABLE smtjOutputEncryptionKMSKeyId #-}
{-# DEPRECATED outputEncryptionKMSKeyId "Use generic-lens or generic-optics with 'outputEncryptionKMSKeyId' instead"  #-}

-- | You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job.
--
-- If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json".
-- You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json".
-- If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjOutputKey :: Lens.Lens' StartMedicalTranscriptionJob (Core.Maybe Types.OutputKey)
smtjOutputKey = Lens.field @"outputKey"
{-# INLINEABLE smtjOutputKey #-}
{-# DEPRECATED outputKey "Use generic-lens or generic-optics with 'outputKey' instead"  #-}

-- | Optional settings for the medical transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjSettings :: Lens.Lens' StartMedicalTranscriptionJob (Core.Maybe Types.MedicalTranscriptionSetting)
smtjSettings = Lens.field @"settings"
{-# INLINEABLE smtjSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

instance Core.ToQuery StartMedicalTranscriptionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMedicalTranscriptionJob where
        toHeaders StartMedicalTranscriptionJob{..}
          = Core.pure
              ("X-Amz-Target", "Transcribe.StartMedicalTranscriptionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMedicalTranscriptionJob where
        toJSON StartMedicalTranscriptionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MedicalTranscriptionJobName" Core..=
                       medicalTranscriptionJobName),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  Core.Just ("Media" Core..= media),
                  Core.Just ("OutputBucketName" Core..= outputBucketName),
                  Core.Just ("Specialty" Core..= specialty),
                  Core.Just ("Type" Core..= type'),
                  ("MediaFormat" Core..=) Core.<$> mediaFormat,
                  ("MediaSampleRateHertz" Core..=) Core.<$> mediaSampleRateHertz,
                  ("OutputEncryptionKMSKeyId" Core..=) Core.<$>
                    outputEncryptionKMSKeyId,
                  ("OutputKey" Core..=) Core.<$> outputKey,
                  ("Settings" Core..=) Core.<$> settings])

instance Core.AWSRequest StartMedicalTranscriptionJob where
        type Rs StartMedicalTranscriptionJob =
             StartMedicalTranscriptionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMedicalTranscriptionJobResponse' Core.<$>
                   (x Core..:? "MedicalTranscriptionJob") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartMedicalTranscriptionJobResponse' smart constructor.
data StartMedicalTranscriptionJobResponse = StartMedicalTranscriptionJobResponse'
  { medicalTranscriptionJob :: Core.Maybe Types.MedicalTranscriptionJob
    -- ^ A batch job submitted to transcribe medical speech to text.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartMedicalTranscriptionJobResponse' value with any optional fields omitted.
mkStartMedicalTranscriptionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMedicalTranscriptionJobResponse
mkStartMedicalTranscriptionJobResponse responseStatus
  = StartMedicalTranscriptionJobResponse'{medicalTranscriptionJob =
                                            Core.Nothing,
                                          responseStatus}

-- | A batch job submitted to transcribe medical speech to text.
--
-- /Note:/ Consider using 'medicalTranscriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjrrsMedicalTranscriptionJob :: Lens.Lens' StartMedicalTranscriptionJobResponse (Core.Maybe Types.MedicalTranscriptionJob)
smtjrrsMedicalTranscriptionJob = Lens.field @"medicalTranscriptionJob"
{-# INLINEABLE smtjrrsMedicalTranscriptionJob #-}
{-# DEPRECATED medicalTranscriptionJob "Use generic-lens or generic-optics with 'medicalTranscriptionJob' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smtjrrsResponseStatus :: Lens.Lens' StartMedicalTranscriptionJobResponse Core.Int
smtjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smtjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
