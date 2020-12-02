{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    startMedicalTranscriptionJob,
    StartMedicalTranscriptionJob,

    -- * Request Lenses
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

    -- * Destructuring the Response
    startMedicalTranscriptionJobResponse,
    StartMedicalTranscriptionJobResponse,

    -- * Response Lenses
    smtjrsMedicalTranscriptionJob,
    smtjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'startMedicalTranscriptionJob' smart constructor.
data StartMedicalTranscriptionJob = StartMedicalTranscriptionJob'
  { _smtjSettings ::
      !( Maybe
           MedicalTranscriptionSetting
       ),
    _smtjMediaFormat ::
      !(Maybe MediaFormat),
    _smtjOutputEncryptionKMSKeyId ::
      !(Maybe Text),
    _smtjOutputKey :: !(Maybe Text),
    _smtjMediaSampleRateHertz ::
      !(Maybe Nat),
    _smtjMedicalTranscriptionJobName ::
      !Text,
    _smtjLanguageCode ::
      !LanguageCode,
    _smtjMedia :: !Media,
    _smtjOutputBucketName :: !Text,
    _smtjSpecialty :: !Specialty,
    _smtjType :: !Type
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMedicalTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smtjSettings' - Optional settings for the medical transcription job.
--
-- * 'smtjMediaFormat' - The audio format of the input media file.
--
-- * 'smtjOutputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the 'StartMedicalTranscriptionJob' operation must have permission to use the specified KMS key. You use either of the following to identify a KMS key in the current account:     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"     * KMS Key Alias: "alias/ExampleAlias" You can use either of the following to identify a KMS key in the current account or another account:     * Amazon Resource Name (ARN) of a KMS key in the current account or another account: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias" If you don't specify an encryption key, the output of the medical transcription job is encrypted with the default Amazon S3 key (SSE-S3). If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
--
-- * 'smtjOutputKey' - You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job. If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json". You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json". If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
--
-- * 'smtjMediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file. If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
--
-- * 'smtjMedicalTranscriptionJobName' - The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
--
-- * 'smtjLanguageCode' - The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- * 'smtjMedia' - Undocumented member.
--
-- * 'smtjOutputBucketName' - The Amazon S3 location where the transcription is stored. You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> . You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
--
-- * 'smtjSpecialty' - The medical specialty of any clinician speaking in the input media.
--
-- * 'smtjType' - The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
startMedicalTranscriptionJob ::
  -- | 'smtjMedicalTranscriptionJobName'
  Text ->
  -- | 'smtjLanguageCode'
  LanguageCode ->
  -- | 'smtjMedia'
  Media ->
  -- | 'smtjOutputBucketName'
  Text ->
  -- | 'smtjSpecialty'
  Specialty ->
  -- | 'smtjType'
  Type ->
  StartMedicalTranscriptionJob
startMedicalTranscriptionJob
  pMedicalTranscriptionJobName_
  pLanguageCode_
  pMedia_
  pOutputBucketName_
  pSpecialty_
  pType_ =
    StartMedicalTranscriptionJob'
      { _smtjSettings = Nothing,
        _smtjMediaFormat = Nothing,
        _smtjOutputEncryptionKMSKeyId = Nothing,
        _smtjOutputKey = Nothing,
        _smtjMediaSampleRateHertz = Nothing,
        _smtjMedicalTranscriptionJobName = pMedicalTranscriptionJobName_,
        _smtjLanguageCode = pLanguageCode_,
        _smtjMedia = pMedia_,
        _smtjOutputBucketName = pOutputBucketName_,
        _smtjSpecialty = pSpecialty_,
        _smtjType = pType_
      }

-- | Optional settings for the medical transcription job.
smtjSettings :: Lens' StartMedicalTranscriptionJob (Maybe MedicalTranscriptionSetting)
smtjSettings = lens _smtjSettings (\s a -> s {_smtjSettings = a})

-- | The audio format of the input media file.
smtjMediaFormat :: Lens' StartMedicalTranscriptionJob (Maybe MediaFormat)
smtjMediaFormat = lens _smtjMediaFormat (\s a -> s {_smtjMediaFormat = a})

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the 'StartMedicalTranscriptionJob' operation must have permission to use the specified KMS key. You use either of the following to identify a KMS key in the current account:     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"     * KMS Key Alias: "alias/ExampleAlias" You can use either of the following to identify a KMS key in the current account or another account:     * Amazon Resource Name (ARN) of a KMS key in the current account or another account: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias" If you don't specify an encryption key, the output of the medical transcription job is encrypted with the default Amazon S3 key (SSE-S3). If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
smtjOutputEncryptionKMSKeyId :: Lens' StartMedicalTranscriptionJob (Maybe Text)
smtjOutputEncryptionKMSKeyId = lens _smtjOutputEncryptionKMSKeyId (\s a -> s {_smtjOutputEncryptionKMSKeyId = a})

-- | You can specify a location in an Amazon S3 bucket to store the output of your medical transcription job. If you don't specify an output key, Amazon Transcribe Medical stores the output of your transcription job in the Amazon S3 bucket you specified. By default, the object key is "your-transcription-job-name.json". You can use output keys to specify the Amazon S3 prefix and file name of the transcription output. For example, specifying the Amazon S3 prefix, "folder1/folder2/", as an output key would lead to the output being stored as "folder1/folder2/your-transcription-job-name.json". If you specify "my-other-job-name.json" as the output key, the object key is changed to "my-other-job-name.json". You can use an output key to change both the prefix and the file name, for example "folder/my-other-job-name.json". If you specify an output key, you must also specify an S3 bucket in the @OutputBucketName@ parameter.
smtjOutputKey :: Lens' StartMedicalTranscriptionJob (Maybe Text)
smtjOutputKey = lens _smtjOutputKey (\s a -> s {_smtjOutputKey = a})

-- | The sample rate, in Hertz, of the audio track in the input media file. If you do not specify the media sample rate, Amazon Transcribe Medical determines the sample rate. If you specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe Medical determine the sample rate.
smtjMediaSampleRateHertz :: Lens' StartMedicalTranscriptionJob (Maybe Natural)
smtjMediaSampleRateHertz = lens _smtjMediaSampleRateHertz (\s a -> s {_smtjMediaSampleRateHertz = a}) . mapping _Nat

-- | The name of the medical transcription job. You can't use the strings "@.@ " or "@..@ " by themselves as the job name. The name must also be unique within an AWS account. If you try to create a medical transcription job with the same name as a previous medical transcription job, you get a @ConflictException@ error.
smtjMedicalTranscriptionJobName :: Lens' StartMedicalTranscriptionJob Text
smtjMedicalTranscriptionJobName = lens _smtjMedicalTranscriptionJobName (\s a -> s {_smtjMedicalTranscriptionJobName = a})

-- | The language code for the language spoken in the input media file. US English (en-US) is the valid value for medical transcription jobs. Any other value you enter for language code results in a @BadRequestException@ error.
smtjLanguageCode :: Lens' StartMedicalTranscriptionJob LanguageCode
smtjLanguageCode = lens _smtjLanguageCode (\s a -> s {_smtjLanguageCode = a})

-- | Undocumented member.
smtjMedia :: Lens' StartMedicalTranscriptionJob Media
smtjMedia = lens _smtjMedia (\s a -> s {_smtjMedia = a})

-- | The Amazon S3 location where the transcription is stored. You must set @OutputBucketName@ for Amazon Transcribe Medical to store the transcription results. Your transcript appears in the S3 location you specify. When you call the 'GetMedicalTranscriptionJob' , the operation returns this location in the @TranscriptFileUri@ field. The S3 bucket must have permissions that allow Amazon Transcribe Medical to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> . You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe Medical uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket.
smtjOutputBucketName :: Lens' StartMedicalTranscriptionJob Text
smtjOutputBucketName = lens _smtjOutputBucketName (\s a -> s {_smtjOutputBucketName = a})

-- | The medical specialty of any clinician speaking in the input media.
smtjSpecialty :: Lens' StartMedicalTranscriptionJob Specialty
smtjSpecialty = lens _smtjSpecialty (\s a -> s {_smtjSpecialty = a})

-- | The type of speech in the input audio. @CONVERSATION@ refers to conversations between two or more speakers, e.g., a conversations between doctors and patients. @DICTATION@ refers to single-speaker dictated speech, e.g., for clinical notes.
smtjType :: Lens' StartMedicalTranscriptionJob Type
smtjType = lens _smtjType (\s a -> s {_smtjType = a})

instance AWSRequest StartMedicalTranscriptionJob where
  type
    Rs StartMedicalTranscriptionJob =
      StartMedicalTranscriptionJobResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          StartMedicalTranscriptionJobResponse'
            <$> (x .?> "MedicalTranscriptionJob") <*> (pure (fromEnum s))
      )

instance Hashable StartMedicalTranscriptionJob

instance NFData StartMedicalTranscriptionJob

instance ToHeaders StartMedicalTranscriptionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.StartMedicalTranscriptionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMedicalTranscriptionJob where
  toJSON StartMedicalTranscriptionJob' {..} =
    object
      ( catMaybes
          [ ("Settings" .=) <$> _smtjSettings,
            ("MediaFormat" .=) <$> _smtjMediaFormat,
            ("OutputEncryptionKMSKeyId" .=) <$> _smtjOutputEncryptionKMSKeyId,
            ("OutputKey" .=) <$> _smtjOutputKey,
            ("MediaSampleRateHertz" .=) <$> _smtjMediaSampleRateHertz,
            Just
              ( "MedicalTranscriptionJobName"
                  .= _smtjMedicalTranscriptionJobName
              ),
            Just ("LanguageCode" .= _smtjLanguageCode),
            Just ("Media" .= _smtjMedia),
            Just ("OutputBucketName" .= _smtjOutputBucketName),
            Just ("Specialty" .= _smtjSpecialty),
            Just ("Type" .= _smtjType)
          ]
      )

instance ToPath StartMedicalTranscriptionJob where
  toPath = const "/"

instance ToQuery StartMedicalTranscriptionJob where
  toQuery = const mempty

-- | /See:/ 'startMedicalTranscriptionJobResponse' smart constructor.
data StartMedicalTranscriptionJobResponse = StartMedicalTranscriptionJobResponse'
  { _smtjrsMedicalTranscriptionJob ::
      !( Maybe
           MedicalTranscriptionJob
       ),
    _smtjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMedicalTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smtjrsMedicalTranscriptionJob' - A batch job submitted to transcribe medical speech to text.
--
-- * 'smtjrsResponseStatus' - -- | The response status code.
startMedicalTranscriptionJobResponse ::
  -- | 'smtjrsResponseStatus'
  Int ->
  StartMedicalTranscriptionJobResponse
startMedicalTranscriptionJobResponse pResponseStatus_ =
  StartMedicalTranscriptionJobResponse'
    { _smtjrsMedicalTranscriptionJob =
        Nothing,
      _smtjrsResponseStatus = pResponseStatus_
    }

-- | A batch job submitted to transcribe medical speech to text.
smtjrsMedicalTranscriptionJob :: Lens' StartMedicalTranscriptionJobResponse (Maybe MedicalTranscriptionJob)
smtjrsMedicalTranscriptionJob = lens _smtjrsMedicalTranscriptionJob (\s a -> s {_smtjrsMedicalTranscriptionJob = a})

-- | -- | The response status code.
smtjrsResponseStatus :: Lens' StartMedicalTranscriptionJobResponse Int
smtjrsResponseStatus = lens _smtjrsResponseStatus (\s a -> s {_smtjrsResponseStatus = a})

instance NFData StartMedicalTranscriptionJobResponse
