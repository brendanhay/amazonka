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
-- Module      : Network.AWS.Transcribe.UpdateVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values. The @UpdateVocabulary@ operation overwrites all of the existing information with the values that you provide in the request.
module Network.AWS.Transcribe.UpdateVocabulary
  ( -- * Creating a Request
    updateVocabulary,
    UpdateVocabulary,

    -- * Request Lenses
    uvVocabularyFileURI,
    uvPhrases,
    uvVocabularyName,
    uvLanguageCode,

    -- * Destructuring the Response
    updateVocabularyResponse,
    UpdateVocabularyResponse,

    -- * Response Lenses
    uvrsLanguageCode,
    uvrsVocabularyName,
    uvrsLastModifiedTime,
    uvrsVocabularyState,
    uvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'updateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { _uvVocabularyFileURI ::
      !(Maybe Text),
    _uvPhrases :: !(Maybe [Text]),
    _uvVocabularyName :: !Text,
    _uvLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvVocabularyFileURI' - The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is  For example: For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
--
-- * 'uvPhrases' - An array of strings containing the vocabulary entries.
--
-- * 'uvVocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
--
-- * 'uvLanguageCode' - The language code of the vocabulary entries.
updateVocabulary ::
  -- | 'uvVocabularyName'
  Text ->
  -- | 'uvLanguageCode'
  LanguageCode ->
  UpdateVocabulary
updateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { _uvVocabularyFileURI = Nothing,
      _uvPhrases = Nothing,
      _uvVocabularyName = pVocabularyName_,
      _uvLanguageCode = pLanguageCode_
    }

-- | The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is  For example: For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
uvVocabularyFileURI :: Lens' UpdateVocabulary (Maybe Text)
uvVocabularyFileURI = lens _uvVocabularyFileURI (\s a -> s {_uvVocabularyFileURI = a})

-- | An array of strings containing the vocabulary entries.
uvPhrases :: Lens' UpdateVocabulary [Text]
uvPhrases = lens _uvPhrases (\s a -> s {_uvPhrases = a}) . _Default . _Coerce

-- | The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
uvVocabularyName :: Lens' UpdateVocabulary Text
uvVocabularyName = lens _uvVocabularyName (\s a -> s {_uvVocabularyName = a})

-- | The language code of the vocabulary entries.
uvLanguageCode :: Lens' UpdateVocabulary LanguageCode
uvLanguageCode = lens _uvLanguageCode (\s a -> s {_uvLanguageCode = a})

instance AWSRequest UpdateVocabulary where
  type Rs UpdateVocabulary = UpdateVocabularyResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          UpdateVocabularyResponse'
            <$> (x .?> "LanguageCode")
            <*> (x .?> "VocabularyName")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyState")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateVocabulary

instance NFData UpdateVocabulary

instance ToHeaders UpdateVocabulary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Transcribe.UpdateVocabulary" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateVocabulary where
  toJSON UpdateVocabulary' {..} =
    object
      ( catMaybes
          [ ("VocabularyFileUri" .=) <$> _uvVocabularyFileURI,
            ("Phrases" .=) <$> _uvPhrases,
            Just ("VocabularyName" .= _uvVocabularyName),
            Just ("LanguageCode" .= _uvLanguageCode)
          ]
      )

instance ToPath UpdateVocabulary where
  toPath = const "/"

instance ToQuery UpdateVocabulary where
  toQuery = const mempty

-- | /See:/ 'updateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { _uvrsLanguageCode ::
      !(Maybe LanguageCode),
    _uvrsVocabularyName :: !(Maybe Text),
    _uvrsLastModifiedTime :: !(Maybe POSIX),
    _uvrsVocabularyState ::
      !(Maybe VocabularyState),
    _uvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvrsLanguageCode' - The language code of the vocabulary entries.
--
-- * 'uvrsVocabularyName' - The name of the vocabulary that was updated.
--
-- * 'uvrsLastModifiedTime' - The date and time that the vocabulary was updated.
--
-- * 'uvrsVocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
--
-- * 'uvrsResponseStatus' - -- | The response status code.
updateVocabularyResponse ::
  -- | 'uvrsResponseStatus'
  Int ->
  UpdateVocabularyResponse
updateVocabularyResponse pResponseStatus_ =
  UpdateVocabularyResponse'
    { _uvrsLanguageCode = Nothing,
      _uvrsVocabularyName = Nothing,
      _uvrsLastModifiedTime = Nothing,
      _uvrsVocabularyState = Nothing,
      _uvrsResponseStatus = pResponseStatus_
    }

-- | The language code of the vocabulary entries.
uvrsLanguageCode :: Lens' UpdateVocabularyResponse (Maybe LanguageCode)
uvrsLanguageCode = lens _uvrsLanguageCode (\s a -> s {_uvrsLanguageCode = a})

-- | The name of the vocabulary that was updated.
uvrsVocabularyName :: Lens' UpdateVocabularyResponse (Maybe Text)
uvrsVocabularyName = lens _uvrsVocabularyName (\s a -> s {_uvrsVocabularyName = a})

-- | The date and time that the vocabulary was updated.
uvrsLastModifiedTime :: Lens' UpdateVocabularyResponse (Maybe UTCTime)
uvrsLastModifiedTime = lens _uvrsLastModifiedTime (\s a -> s {_uvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
uvrsVocabularyState :: Lens' UpdateVocabularyResponse (Maybe VocabularyState)
uvrsVocabularyState = lens _uvrsVocabularyState (\s a -> s {_uvrsVocabularyState = a})

-- | -- | The response status code.
uvrsResponseStatus :: Lens' UpdateVocabularyResponse Int
uvrsResponseStatus = lens _uvrsResponseStatus (\s a -> s {_uvrsResponseStatus = a})

instance NFData UpdateVocabularyResponse
