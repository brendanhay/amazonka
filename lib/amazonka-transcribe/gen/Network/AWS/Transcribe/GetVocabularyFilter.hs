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
-- Module      : Network.AWS.Transcribe.GetVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a vocabulary filter.
module Network.AWS.Transcribe.GetVocabularyFilter
  ( -- * Creating a Request
    getVocabularyFilter,
    GetVocabularyFilter,

    -- * Request Lenses
    gvfVocabularyFilterName,

    -- * Destructuring the Response
    getVocabularyFilterResponse,
    GetVocabularyFilterResponse,

    -- * Response Lenses
    gvfrsLanguageCode,
    gvfrsDownloadURI,
    gvfrsLastModifiedTime,
    gvfrsVocabularyFilterName,
    gvfrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'getVocabularyFilter' smart constructor.
newtype GetVocabularyFilter = GetVocabularyFilter'
  { _gvfVocabularyFilterName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVocabularyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvfVocabularyFilterName' - The name of the vocabulary filter for which to return information.
getVocabularyFilter ::
  -- | 'gvfVocabularyFilterName'
  Text ->
  GetVocabularyFilter
getVocabularyFilter pVocabularyFilterName_ =
  GetVocabularyFilter'
    { _gvfVocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter for which to return information.
gvfVocabularyFilterName :: Lens' GetVocabularyFilter Text
gvfVocabularyFilterName = lens _gvfVocabularyFilterName (\s a -> s {_gvfVocabularyFilterName = a})

instance AWSRequest GetVocabularyFilter where
  type Rs GetVocabularyFilter = GetVocabularyFilterResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          GetVocabularyFilterResponse'
            <$> (x .?> "LanguageCode")
            <*> (x .?> "DownloadUri")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "VocabularyFilterName")
            <*> (pure (fromEnum s))
      )

instance Hashable GetVocabularyFilter

instance NFData GetVocabularyFilter

instance ToHeaders GetVocabularyFilter where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.GetVocabularyFilter" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetVocabularyFilter where
  toJSON GetVocabularyFilter' {..} =
    object
      ( catMaybes
          [Just ("VocabularyFilterName" .= _gvfVocabularyFilterName)]
      )

instance ToPath GetVocabularyFilter where
  toPath = const "/"

instance ToQuery GetVocabularyFilter where
  toQuery = const mempty

-- | /See:/ 'getVocabularyFilterResponse' smart constructor.
data GetVocabularyFilterResponse = GetVocabularyFilterResponse'
  { _gvfrsLanguageCode ::
      !(Maybe LanguageCode),
    _gvfrsDownloadURI :: !(Maybe Text),
    _gvfrsLastModifiedTime ::
      !(Maybe POSIX),
    _gvfrsVocabularyFilterName ::
      !(Maybe Text),
    _gvfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvfrsLanguageCode' - The language code of the words in the vocabulary filter.
--
-- * 'gvfrsDownloadURI' - The URI of the list of words in the vocabulary filter. You can use this URI to get the list of words.
--
-- * 'gvfrsLastModifiedTime' - The date and time that the contents of the vocabulary filter were updated.
--
-- * 'gvfrsVocabularyFilterName' - The name of the vocabulary filter.
--
-- * 'gvfrsResponseStatus' - -- | The response status code.
getVocabularyFilterResponse ::
  -- | 'gvfrsResponseStatus'
  Int ->
  GetVocabularyFilterResponse
getVocabularyFilterResponse pResponseStatus_ =
  GetVocabularyFilterResponse'
    { _gvfrsLanguageCode = Nothing,
      _gvfrsDownloadURI = Nothing,
      _gvfrsLastModifiedTime = Nothing,
      _gvfrsVocabularyFilterName = Nothing,
      _gvfrsResponseStatus = pResponseStatus_
    }

-- | The language code of the words in the vocabulary filter.
gvfrsLanguageCode :: Lens' GetVocabularyFilterResponse (Maybe LanguageCode)
gvfrsLanguageCode = lens _gvfrsLanguageCode (\s a -> s {_gvfrsLanguageCode = a})

-- | The URI of the list of words in the vocabulary filter. You can use this URI to get the list of words.
gvfrsDownloadURI :: Lens' GetVocabularyFilterResponse (Maybe Text)
gvfrsDownloadURI = lens _gvfrsDownloadURI (\s a -> s {_gvfrsDownloadURI = a})

-- | The date and time that the contents of the vocabulary filter were updated.
gvfrsLastModifiedTime :: Lens' GetVocabularyFilterResponse (Maybe UTCTime)
gvfrsLastModifiedTime = lens _gvfrsLastModifiedTime (\s a -> s {_gvfrsLastModifiedTime = a}) . mapping _Time

-- | The name of the vocabulary filter.
gvfrsVocabularyFilterName :: Lens' GetVocabularyFilterResponse (Maybe Text)
gvfrsVocabularyFilterName = lens _gvfrsVocabularyFilterName (\s a -> s {_gvfrsVocabularyFilterName = a})

-- | -- | The response status code.
gvfrsResponseStatus :: Lens' GetVocabularyFilterResponse Int
gvfrsResponseStatus = lens _gvfrsResponseStatus (\s a -> s {_gvfrsResponseStatus = a})

instance NFData GetVocabularyFilterResponse
