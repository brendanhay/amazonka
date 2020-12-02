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
-- Module      : Network.AWS.Transcribe.CreateVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new vocabulary filter that you can use to filter words, such as profane words, from the output of a transcription job.
module Network.AWS.Transcribe.CreateVocabularyFilter
  ( -- * Creating a Request
    createVocabularyFilter,
    CreateVocabularyFilter,

    -- * Request Lenses
    cvfVocabularyFilterFileURI,
    cvfWords,
    cvfVocabularyFilterName,
    cvfLanguageCode,

    -- * Destructuring the Response
    createVocabularyFilterResponse,
    CreateVocabularyFilterResponse,

    -- * Response Lenses
    cvfrsLanguageCode,
    cvfrsLastModifiedTime,
    cvfrsVocabularyFilterName,
    cvfrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'createVocabularyFilter' smart constructor.
data CreateVocabularyFilter = CreateVocabularyFilter'
  { _cvfVocabularyFilterFileURI ::
      !(Maybe Text),
    _cvfWords :: !(Maybe (List1 Text)),
    _cvfVocabularyFilterName :: !Text,
    _cvfLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVocabularyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvfVocabularyFilterFileURI' - The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . The specified file must be less than 50 KB of UTF-8 characters. If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- * 'cvfWords' - The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- * 'cvfVocabularyFilterName' - The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
--
-- * 'cvfLanguageCode' - The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
createVocabularyFilter ::
  -- | 'cvfVocabularyFilterName'
  Text ->
  -- | 'cvfLanguageCode'
  LanguageCode ->
  CreateVocabularyFilter
createVocabularyFilter pVocabularyFilterName_ pLanguageCode_ =
  CreateVocabularyFilter'
    { _cvfVocabularyFilterFileURI = Nothing,
      _cvfWords = Nothing,
      _cvfVocabularyFilterName = pVocabularyFilterName_,
      _cvfLanguageCode = pLanguageCode_
    }

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . The specified file must be less than 50 KB of UTF-8 characters. If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
cvfVocabularyFilterFileURI :: Lens' CreateVocabularyFilter (Maybe Text)
cvfVocabularyFilterFileURI = lens _cvfVocabularyFilterFileURI (\s a -> s {_cvfVocabularyFilterFileURI = a})

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
cvfWords :: Lens' CreateVocabularyFilter (Maybe (NonEmpty Text))
cvfWords = lens _cvfWords (\s a -> s {_cvfWords = a}) . mapping _List1

-- | The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
cvfVocabularyFilterName :: Lens' CreateVocabularyFilter Text
cvfVocabularyFilterName = lens _cvfVocabularyFilterName (\s a -> s {_cvfVocabularyFilterName = a})

-- | The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
cvfLanguageCode :: Lens' CreateVocabularyFilter LanguageCode
cvfLanguageCode = lens _cvfLanguageCode (\s a -> s {_cvfLanguageCode = a})

instance AWSRequest CreateVocabularyFilter where
  type Rs CreateVocabularyFilter = CreateVocabularyFilterResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          CreateVocabularyFilterResponse'
            <$> (x .?> "LanguageCode")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyFilterName")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateVocabularyFilter

instance NFData CreateVocabularyFilter

instance ToHeaders CreateVocabularyFilter where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.CreateVocabularyFilter" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter' {..} =
    object
      ( catMaybes
          [ ("VocabularyFilterFileUri" .=) <$> _cvfVocabularyFilterFileURI,
            ("Words" .=) <$> _cvfWords,
            Just ("VocabularyFilterName" .= _cvfVocabularyFilterName),
            Just ("LanguageCode" .= _cvfLanguageCode)
          ]
      )

instance ToPath CreateVocabularyFilter where
  toPath = const "/"

instance ToQuery CreateVocabularyFilter where
  toQuery = const mempty

-- | /See:/ 'createVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { _cvfrsLanguageCode ::
      !(Maybe LanguageCode),
    _cvfrsLastModifiedTime ::
      !(Maybe POSIX),
    _cvfrsVocabularyFilterName ::
      !(Maybe Text),
    _cvfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvfrsLanguageCode' - The language code of the words in the collection.
--
-- * 'cvfrsLastModifiedTime' - The date and time that the vocabulary filter was modified.
--
-- * 'cvfrsVocabularyFilterName' - The name of the vocabulary filter.
--
-- * 'cvfrsResponseStatus' - -- | The response status code.
createVocabularyFilterResponse ::
  -- | 'cvfrsResponseStatus'
  Int ->
  CreateVocabularyFilterResponse
createVocabularyFilterResponse pResponseStatus_ =
  CreateVocabularyFilterResponse'
    { _cvfrsLanguageCode = Nothing,
      _cvfrsLastModifiedTime = Nothing,
      _cvfrsVocabularyFilterName = Nothing,
      _cvfrsResponseStatus = pResponseStatus_
    }

-- | The language code of the words in the collection.
cvfrsLanguageCode :: Lens' CreateVocabularyFilterResponse (Maybe LanguageCode)
cvfrsLanguageCode = lens _cvfrsLanguageCode (\s a -> s {_cvfrsLanguageCode = a})

-- | The date and time that the vocabulary filter was modified.
cvfrsLastModifiedTime :: Lens' CreateVocabularyFilterResponse (Maybe UTCTime)
cvfrsLastModifiedTime = lens _cvfrsLastModifiedTime (\s a -> s {_cvfrsLastModifiedTime = a}) . mapping _Time

-- | The name of the vocabulary filter.
cvfrsVocabularyFilterName :: Lens' CreateVocabularyFilterResponse (Maybe Text)
cvfrsVocabularyFilterName = lens _cvfrsVocabularyFilterName (\s a -> s {_cvfrsVocabularyFilterName = a})

-- | -- | The response status code.
cvfrsResponseStatus :: Lens' CreateVocabularyFilterResponse Int
cvfrsResponseStatus = lens _cvfrsResponseStatus (\s a -> s {_cvfrsResponseStatus = a})

instance NFData CreateVocabularyFilterResponse
