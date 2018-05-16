{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetVocabulary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a vocabulary.
--
--
module Network.AWS.Transcribe.GetVocabulary
    (
    -- * Creating a Request
      getVocabulary
    , GetVocabulary
    -- * Request Lenses
    , gvVocabularyName

    -- * Destructuring the Response
    , getVocabularyResponse
    , GetVocabularyResponse
    -- * Response Lenses
    , gvrsFailureReason
    , gvrsLanguageCode
    , gvrsDownloadURI
    , gvrsVocabularyName
    , gvrsLastModifiedTime
    , gvrsVocabularyState
    , gvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'getVocabulary' smart constructor.
newtype GetVocabulary = GetVocabulary'
  { _gvVocabularyName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvVocabularyName' - The name of the vocabulary to return information about. The name is case-sensitive.
getVocabulary
    :: Text -- ^ 'gvVocabularyName'
    -> GetVocabulary
getVocabulary pVocabularyName_ =
  GetVocabulary' {_gvVocabularyName = pVocabularyName_}


-- | The name of the vocabulary to return information about. The name is case-sensitive.
gvVocabularyName :: Lens' GetVocabulary Text
gvVocabularyName = lens _gvVocabularyName (\ s a -> s{_gvVocabularyName = a})

instance AWSRequest GetVocabulary where
        type Rs GetVocabulary = GetVocabularyResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 GetVocabularyResponse' <$>
                   (x .?> "FailureReason") <*> (x .?> "LanguageCode")
                     <*> (x .?> "DownloadUri")
                     <*> (x .?> "VocabularyName")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "VocabularyState")
                     <*> (pure (fromEnum s)))

instance Hashable GetVocabulary where

instance NFData GetVocabulary where

instance ToHeaders GetVocabulary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.GetVocabulary" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetVocabulary where
        toJSON GetVocabulary'{..}
          = object
              (catMaybes
                 [Just ("VocabularyName" .= _gvVocabularyName)])

instance ToPath GetVocabulary where
        toPath = const "/"

instance ToQuery GetVocabulary where
        toQuery = const mempty

-- | /See:/ 'getVocabularyResponse' smart constructor.
data GetVocabularyResponse = GetVocabularyResponse'
  { _gvrsFailureReason    :: !(Maybe Text)
  , _gvrsLanguageCode     :: !(Maybe LanguageCode)
  , _gvrsDownloadURI      :: !(Maybe Text)
  , _gvrsVocabularyName   :: !(Maybe Text)
  , _gvrsLastModifiedTime :: !(Maybe POSIX)
  , _gvrsVocabularyState  :: !(Maybe VocabularyState)
  , _gvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvrsFailureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- * 'gvrsLanguageCode' - The language code of the vocabulary entries.
--
-- * 'gvrsDownloadURI' - The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
--
-- * 'gvrsVocabularyName' - The name of the vocabulary to return.
--
-- * 'gvrsLastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- * 'gvrsVocabularyState' - The processing state of the vocabulary.
--
-- * 'gvrsResponseStatus' - -- | The response status code.
getVocabularyResponse
    :: Int -- ^ 'gvrsResponseStatus'
    -> GetVocabularyResponse
getVocabularyResponse pResponseStatus_ =
  GetVocabularyResponse'
    { _gvrsFailureReason = Nothing
    , _gvrsLanguageCode = Nothing
    , _gvrsDownloadURI = Nothing
    , _gvrsVocabularyName = Nothing
    , _gvrsLastModifiedTime = Nothing
    , _gvrsVocabularyState = Nothing
    , _gvrsResponseStatus = pResponseStatus_
    }


-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
gvrsFailureReason :: Lens' GetVocabularyResponse (Maybe Text)
gvrsFailureReason = lens _gvrsFailureReason (\ s a -> s{_gvrsFailureReason = a})

-- | The language code of the vocabulary entries.
gvrsLanguageCode :: Lens' GetVocabularyResponse (Maybe LanguageCode)
gvrsLanguageCode = lens _gvrsLanguageCode (\ s a -> s{_gvrsLanguageCode = a})

-- | The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
gvrsDownloadURI :: Lens' GetVocabularyResponse (Maybe Text)
gvrsDownloadURI = lens _gvrsDownloadURI (\ s a -> s{_gvrsDownloadURI = a})

-- | The name of the vocabulary to return.
gvrsVocabularyName :: Lens' GetVocabularyResponse (Maybe Text)
gvrsVocabularyName = lens _gvrsVocabularyName (\ s a -> s{_gvrsVocabularyName = a})

-- | The date and time that the vocabulary was last modified.
gvrsLastModifiedTime :: Lens' GetVocabularyResponse (Maybe UTCTime)
gvrsLastModifiedTime = lens _gvrsLastModifiedTime (\ s a -> s{_gvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary.
gvrsVocabularyState :: Lens' GetVocabularyResponse (Maybe VocabularyState)
gvrsVocabularyState = lens _gvrsVocabularyState (\ s a -> s{_gvrsVocabularyState = a})

-- | -- | The response status code.
gvrsResponseStatus :: Lens' GetVocabularyResponse Int
gvrsResponseStatus = lens _gvrsResponseStatus (\ s a -> s{_gvrsResponseStatus = a})

instance NFData GetVocabularyResponse where
