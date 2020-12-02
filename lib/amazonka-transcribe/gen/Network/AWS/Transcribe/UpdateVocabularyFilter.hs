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
-- Module      : Network.AWS.Transcribe.UpdateVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vocabulary filter with a new list of filtered words.
module Network.AWS.Transcribe.UpdateVocabularyFilter
  ( -- * Creating a Request
    updateVocabularyFilter,
    UpdateVocabularyFilter,

    -- * Request Lenses
    uvfVocabularyFilterFileURI,
    uvfWords,
    uvfVocabularyFilterName,

    -- * Destructuring the Response
    updateVocabularyFilterResponse,
    UpdateVocabularyFilterResponse,

    -- * Response Lenses
    uvfrsLanguageCode,
    uvfrsLastModifiedTime,
    uvfrsVocabularyFilterName,
    uvfrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'updateVocabularyFilter' smart constructor.
data UpdateVocabularyFilter = UpdateVocabularyFilter'
  { _uvfVocabularyFilterFileURI ::
      !(Maybe Text),
    _uvfWords :: !(Maybe (List1 Text)),
    _uvfVocabularyFilterName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVocabularyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvfVocabularyFilterFileURI' - The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . The specified file must be less than 50 KB of UTF-8 characters. If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- * 'uvfWords' - The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- * 'uvfVocabularyFilterName' - The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
updateVocabularyFilter ::
  -- | 'uvfVocabularyFilterName'
  Text ->
  UpdateVocabularyFilter
updateVocabularyFilter pVocabularyFilterName_ =
  UpdateVocabularyFilter'
    { _uvfVocabularyFilterFileURI = Nothing,
      _uvfWords = Nothing,
      _uvfVocabularyFilterName = pVocabularyFilterName_
    }

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . The specified file must be less than 50 KB of UTF-8 characters. If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
uvfVocabularyFilterFileURI :: Lens' UpdateVocabularyFilter (Maybe Text)
uvfVocabularyFilterFileURI = lens _uvfVocabularyFilterFileURI (\s a -> s {_uvfVocabularyFilterFileURI = a})

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> . If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
uvfWords :: Lens' UpdateVocabularyFilter (Maybe (NonEmpty Text))
uvfWords = lens _uvfWords (\s a -> s {_uvfWords = a}) . mapping _List1

-- | The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
uvfVocabularyFilterName :: Lens' UpdateVocabularyFilter Text
uvfVocabularyFilterName = lens _uvfVocabularyFilterName (\s a -> s {_uvfVocabularyFilterName = a})

instance AWSRequest UpdateVocabularyFilter where
  type Rs UpdateVocabularyFilter = UpdateVocabularyFilterResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          UpdateVocabularyFilterResponse'
            <$> (x .?> "LanguageCode")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyFilterName")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateVocabularyFilter

instance NFData UpdateVocabularyFilter

instance ToHeaders UpdateVocabularyFilter where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.UpdateVocabularyFilter" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateVocabularyFilter where
  toJSON UpdateVocabularyFilter' {..} =
    object
      ( catMaybes
          [ ("VocabularyFilterFileUri" .=) <$> _uvfVocabularyFilterFileURI,
            ("Words" .=) <$> _uvfWords,
            Just ("VocabularyFilterName" .= _uvfVocabularyFilterName)
          ]
      )

instance ToPath UpdateVocabularyFilter where
  toPath = const "/"

instance ToQuery UpdateVocabularyFilter where
  toQuery = const mempty

-- | /See:/ 'updateVocabularyFilterResponse' smart constructor.
data UpdateVocabularyFilterResponse = UpdateVocabularyFilterResponse'
  { _uvfrsLanguageCode ::
      !(Maybe LanguageCode),
    _uvfrsLastModifiedTime ::
      !(Maybe POSIX),
    _uvfrsVocabularyFilterName ::
      !(Maybe Text),
    _uvfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvfrsLanguageCode' - The language code of the words in the vocabulary filter.
--
-- * 'uvfrsLastModifiedTime' - The date and time that the vocabulary filter was updated.
--
-- * 'uvfrsVocabularyFilterName' - The name of the updated vocabulary filter.
--
-- * 'uvfrsResponseStatus' - -- | The response status code.
updateVocabularyFilterResponse ::
  -- | 'uvfrsResponseStatus'
  Int ->
  UpdateVocabularyFilterResponse
updateVocabularyFilterResponse pResponseStatus_ =
  UpdateVocabularyFilterResponse'
    { _uvfrsLanguageCode = Nothing,
      _uvfrsLastModifiedTime = Nothing,
      _uvfrsVocabularyFilterName = Nothing,
      _uvfrsResponseStatus = pResponseStatus_
    }

-- | The language code of the words in the vocabulary filter.
uvfrsLanguageCode :: Lens' UpdateVocabularyFilterResponse (Maybe LanguageCode)
uvfrsLanguageCode = lens _uvfrsLanguageCode (\s a -> s {_uvfrsLanguageCode = a})

-- | The date and time that the vocabulary filter was updated.
uvfrsLastModifiedTime :: Lens' UpdateVocabularyFilterResponse (Maybe UTCTime)
uvfrsLastModifiedTime = lens _uvfrsLastModifiedTime (\s a -> s {_uvfrsLastModifiedTime = a}) . mapping _Time

-- | The name of the updated vocabulary filter.
uvfrsVocabularyFilterName :: Lens' UpdateVocabularyFilterResponse (Maybe Text)
uvfrsVocabularyFilterName = lens _uvfrsVocabularyFilterName (\s a -> s {_uvfrsVocabularyFilterName = a})

-- | -- | The response status code.
uvfrsResponseStatus :: Lens' UpdateVocabularyFilterResponse Int
uvfrsResponseStatus = lens _uvfrsResponseStatus (\s a -> s {_uvfrsResponseStatus = a})

instance NFData UpdateVocabularyFilterResponse
