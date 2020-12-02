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
-- Module      : Network.AWS.Transcribe.CreateMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to change how Amazon Transcribe Medical transcribes your audio file.
module Network.AWS.Transcribe.CreateMedicalVocabulary
  ( -- * Creating a Request
    createMedicalVocabulary,
    CreateMedicalVocabulary,

    -- * Request Lenses
    cmvVocabularyName,
    cmvLanguageCode,
    cmvVocabularyFileURI,

    -- * Destructuring the Response
    createMedicalVocabularyResponse,
    CreateMedicalVocabularyResponse,

    -- * Response Lenses
    cmvrsFailureReason,
    cmvrsLanguageCode,
    cmvrsVocabularyName,
    cmvrsLastModifiedTime,
    cmvrsVocabularyState,
    cmvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'createMedicalVocabulary' smart constructor.
data CreateMedicalVocabulary = CreateMedicalVocabulary'
  { _cmvVocabularyName ::
      !Text,
    _cmvLanguageCode :: !LanguageCode,
    _cmvVocabularyFileURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMedicalVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvVocabularyName' - The name of the custom vocabulary. This case-sensitive name must be unique within an AWS account. If you try to create a vocabulary with the same name as a previous vocabulary, you get a @ConflictException@ error.
--
-- * 'cmvLanguageCode' - The language code for the language used for the entries in your custom vocabulary. The language code of your custom vocabulary must match the language code of your transcription job. US English (en-US) is the only language code available for Amazon Transcribe Medical.
--
-- * 'cmvVocabularyFileURI' - The location in Amazon S3 of the text file you use to define your custom vocabulary. The URI must be in the same AWS Region as the resource that you're calling. Enter information about your @VocabularyFileUri@ in the following format: @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  The following is an example URI for a vocabulary file that is stored in Amazon S3: @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@  For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies> .
createMedicalVocabulary ::
  -- | 'cmvVocabularyName'
  Text ->
  -- | 'cmvLanguageCode'
  LanguageCode ->
  -- | 'cmvVocabularyFileURI'
  Text ->
  CreateMedicalVocabulary
createMedicalVocabulary
  pVocabularyName_
  pLanguageCode_
  pVocabularyFileURI_ =
    CreateMedicalVocabulary'
      { _cmvVocabularyName = pVocabularyName_,
        _cmvLanguageCode = pLanguageCode_,
        _cmvVocabularyFileURI = pVocabularyFileURI_
      }

-- | The name of the custom vocabulary. This case-sensitive name must be unique within an AWS account. If you try to create a vocabulary with the same name as a previous vocabulary, you get a @ConflictException@ error.
cmvVocabularyName :: Lens' CreateMedicalVocabulary Text
cmvVocabularyName = lens _cmvVocabularyName (\s a -> s {_cmvVocabularyName = a})

-- | The language code for the language used for the entries in your custom vocabulary. The language code of your custom vocabulary must match the language code of your transcription job. US English (en-US) is the only language code available for Amazon Transcribe Medical.
cmvLanguageCode :: Lens' CreateMedicalVocabulary LanguageCode
cmvLanguageCode = lens _cmvLanguageCode (\s a -> s {_cmvLanguageCode = a})

-- | The location in Amazon S3 of the text file you use to define your custom vocabulary. The URI must be in the same AWS Region as the resource that you're calling. Enter information about your @VocabularyFileUri@ in the following format: @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  The following is an example URI for a vocabulary file that is stored in Amazon S3: @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@  For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies> .
cmvVocabularyFileURI :: Lens' CreateMedicalVocabulary Text
cmvVocabularyFileURI = lens _cmvVocabularyFileURI (\s a -> s {_cmvVocabularyFileURI = a})

instance AWSRequest CreateMedicalVocabulary where
  type Rs CreateMedicalVocabulary = CreateMedicalVocabularyResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          CreateMedicalVocabularyResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "LanguageCode")
            <*> (x .?> "VocabularyName")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyState")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateMedicalVocabulary

instance NFData CreateMedicalVocabulary

instance ToHeaders CreateMedicalVocabulary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.CreateMedicalVocabulary" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateMedicalVocabulary where
  toJSON CreateMedicalVocabulary' {..} =
    object
      ( catMaybes
          [ Just ("VocabularyName" .= _cmvVocabularyName),
            Just ("LanguageCode" .= _cmvLanguageCode),
            Just ("VocabularyFileUri" .= _cmvVocabularyFileURI)
          ]
      )

instance ToPath CreateMedicalVocabulary where
  toPath = const "/"

instance ToQuery CreateMedicalVocabulary where
  toQuery = const mempty

-- | /See:/ 'createMedicalVocabularyResponse' smart constructor.
data CreateMedicalVocabularyResponse = CreateMedicalVocabularyResponse'
  { _cmvrsFailureReason ::
      !(Maybe Text),
    _cmvrsLanguageCode ::
      !(Maybe LanguageCode),
    _cmvrsVocabularyName ::
      !(Maybe Text),
    _cmvrsLastModifiedTime ::
      !(Maybe POSIX),
    _cmvrsVocabularyState ::
      !(Maybe VocabularyState),
    _cmvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvrsFailureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- * 'cmvrsLanguageCode' - The language code for the entries in your custom vocabulary. US English (en-US) is the only valid language code for Amazon Transcribe Medical.
--
-- * 'cmvrsVocabularyName' - The name of the vocabulary. The name must be unique within an AWS account and is case sensitive.
--
-- * 'cmvrsLastModifiedTime' - The date and time that you created the vocabulary.
--
-- * 'cmvrsVocabularyState' - The processing state of your custom vocabulary in Amazon Transcribe Medical. If the state is @READY@ , you can use the vocabulary in a @StartMedicalTranscriptionJob@ request.
--
-- * 'cmvrsResponseStatus' - -- | The response status code.
createMedicalVocabularyResponse ::
  -- | 'cmvrsResponseStatus'
  Int ->
  CreateMedicalVocabularyResponse
createMedicalVocabularyResponse pResponseStatus_ =
  CreateMedicalVocabularyResponse'
    { _cmvrsFailureReason = Nothing,
      _cmvrsLanguageCode = Nothing,
      _cmvrsVocabularyName = Nothing,
      _cmvrsLastModifiedTime = Nothing,
      _cmvrsVocabularyState = Nothing,
      _cmvrsResponseStatus = pResponseStatus_
    }

-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
cmvrsFailureReason :: Lens' CreateMedicalVocabularyResponse (Maybe Text)
cmvrsFailureReason = lens _cmvrsFailureReason (\s a -> s {_cmvrsFailureReason = a})

-- | The language code for the entries in your custom vocabulary. US English (en-US) is the only valid language code for Amazon Transcribe Medical.
cmvrsLanguageCode :: Lens' CreateMedicalVocabularyResponse (Maybe LanguageCode)
cmvrsLanguageCode = lens _cmvrsLanguageCode (\s a -> s {_cmvrsLanguageCode = a})

-- | The name of the vocabulary. The name must be unique within an AWS account and is case sensitive.
cmvrsVocabularyName :: Lens' CreateMedicalVocabularyResponse (Maybe Text)
cmvrsVocabularyName = lens _cmvrsVocabularyName (\s a -> s {_cmvrsVocabularyName = a})

-- | The date and time that you created the vocabulary.
cmvrsLastModifiedTime :: Lens' CreateMedicalVocabularyResponse (Maybe UTCTime)
cmvrsLastModifiedTime = lens _cmvrsLastModifiedTime (\s a -> s {_cmvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of your custom vocabulary in Amazon Transcribe Medical. If the state is @READY@ , you can use the vocabulary in a @StartMedicalTranscriptionJob@ request.
cmvrsVocabularyState :: Lens' CreateMedicalVocabularyResponse (Maybe VocabularyState)
cmvrsVocabularyState = lens _cmvrsVocabularyState (\s a -> s {_cmvrsVocabularyState = a})

-- | -- | The response status code.
cmvrsResponseStatus :: Lens' CreateMedicalVocabularyResponse Int
cmvrsResponseStatus = lens _cmvrsResponseStatus (\s a -> s {_cmvrsResponseStatus = a})

instance NFData CreateMedicalVocabularyResponse
