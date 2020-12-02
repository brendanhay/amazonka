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
-- Module      : Network.AWS.Transcribe.GetMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a medical vocabulary.
module Network.AWS.Transcribe.GetMedicalVocabulary
  ( -- * Creating a Request
    getMedicalVocabulary,
    GetMedicalVocabulary,

    -- * Request Lenses
    gmvVocabularyName,

    -- * Destructuring the Response
    getMedicalVocabularyResponse,
    GetMedicalVocabularyResponse,

    -- * Response Lenses
    gmvrsFailureReason,
    gmvrsLanguageCode,
    gmvrsDownloadURI,
    gmvrsVocabularyName,
    gmvrsLastModifiedTime,
    gmvrsVocabularyState,
    gmvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'getMedicalVocabulary' smart constructor.
newtype GetMedicalVocabulary = GetMedicalVocabulary'
  { _gmvVocabularyName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMedicalVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmvVocabularyName' - The name of the vocabulary that you want information about. The value is case sensitive.
getMedicalVocabulary ::
  -- | 'gmvVocabularyName'
  Text ->
  GetMedicalVocabulary
getMedicalVocabulary pVocabularyName_ =
  GetMedicalVocabulary' {_gmvVocabularyName = pVocabularyName_}

-- | The name of the vocabulary that you want information about. The value is case sensitive.
gmvVocabularyName :: Lens' GetMedicalVocabulary Text
gmvVocabularyName = lens _gmvVocabularyName (\s a -> s {_gmvVocabularyName = a})

instance AWSRequest GetMedicalVocabulary where
  type Rs GetMedicalVocabulary = GetMedicalVocabularyResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          GetMedicalVocabularyResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "LanguageCode")
            <*> (x .?> "DownloadUri")
            <*> (x .?> "VocabularyName")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyState")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMedicalVocabulary

instance NFData GetMedicalVocabulary

instance ToHeaders GetMedicalVocabulary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.GetMedicalVocabulary" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMedicalVocabulary where
  toJSON GetMedicalVocabulary' {..} =
    object
      (catMaybes [Just ("VocabularyName" .= _gmvVocabularyName)])

instance ToPath GetMedicalVocabulary where
  toPath = const "/"

instance ToQuery GetMedicalVocabulary where
  toQuery = const mempty

-- | /See:/ 'getMedicalVocabularyResponse' smart constructor.
data GetMedicalVocabularyResponse = GetMedicalVocabularyResponse'
  { _gmvrsFailureReason ::
      !(Maybe Text),
    _gmvrsLanguageCode ::
      !(Maybe LanguageCode),
    _gmvrsDownloadURI ::
      !(Maybe Text),
    _gmvrsVocabularyName ::
      !(Maybe Text),
    _gmvrsLastModifiedTime ::
      !(Maybe POSIX),
    _gmvrsVocabularyState ::
      !(Maybe VocabularyState),
    _gmvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmvrsFailureReason' - If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
--
-- * 'gmvrsLanguageCode' - The valid language code for your vocabulary entries.
--
-- * 'gmvrsDownloadURI' - The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
--
-- * 'gmvrsVocabularyName' - The name of the vocabulary returned by Amazon Transcribe Medical.
--
-- * 'gmvrsLastModifiedTime' - The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
--
-- * 'gmvrsVocabularyState' - The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
--
-- * 'gmvrsResponseStatus' - -- | The response status code.
getMedicalVocabularyResponse ::
  -- | 'gmvrsResponseStatus'
  Int ->
  GetMedicalVocabularyResponse
getMedicalVocabularyResponse pResponseStatus_ =
  GetMedicalVocabularyResponse'
    { _gmvrsFailureReason = Nothing,
      _gmvrsLanguageCode = Nothing,
      _gmvrsDownloadURI = Nothing,
      _gmvrsVocabularyName = Nothing,
      _gmvrsLastModifiedTime = Nothing,
      _gmvrsVocabularyState = Nothing,
      _gmvrsResponseStatus = pResponseStatus_
    }

-- | If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
gmvrsFailureReason :: Lens' GetMedicalVocabularyResponse (Maybe Text)
gmvrsFailureReason = lens _gmvrsFailureReason (\s a -> s {_gmvrsFailureReason = a})

-- | The valid language code for your vocabulary entries.
gmvrsLanguageCode :: Lens' GetMedicalVocabularyResponse (Maybe LanguageCode)
gmvrsLanguageCode = lens _gmvrsLanguageCode (\s a -> s {_gmvrsLanguageCode = a})

-- | The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
gmvrsDownloadURI :: Lens' GetMedicalVocabularyResponse (Maybe Text)
gmvrsDownloadURI = lens _gmvrsDownloadURI (\s a -> s {_gmvrsDownloadURI = a})

-- | The name of the vocabulary returned by Amazon Transcribe Medical.
gmvrsVocabularyName :: Lens' GetMedicalVocabularyResponse (Maybe Text)
gmvrsVocabularyName = lens _gmvrsVocabularyName (\s a -> s {_gmvrsVocabularyName = a})

-- | The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
gmvrsLastModifiedTime :: Lens' GetMedicalVocabularyResponse (Maybe UTCTime)
gmvrsLastModifiedTime = lens _gmvrsLastModifiedTime (\s a -> s {_gmvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
gmvrsVocabularyState :: Lens' GetMedicalVocabularyResponse (Maybe VocabularyState)
gmvrsVocabularyState = lens _gmvrsVocabularyState (\s a -> s {_gmvrsVocabularyState = a})

-- | -- | The response status code.
gmvrsResponseStatus :: Lens' GetMedicalVocabularyResponse Int
gmvrsResponseStatus = lens _gmvrsResponseStatus (\s a -> s {_gmvrsResponseStatus = a})

instance NFData GetMedicalVocabularyResponse
