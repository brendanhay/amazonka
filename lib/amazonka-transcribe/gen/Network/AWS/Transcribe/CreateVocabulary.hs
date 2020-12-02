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
-- Module      : Network.AWS.Transcribe.CreateVocabulary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to change the way Amazon Transcribe handles transcription of an audio file.
--
--
module Network.AWS.Transcribe.CreateVocabulary
    (
    -- * Creating a Request
      createVocabulary
    , CreateVocabulary
    -- * Request Lenses
    , cvVocabularyName
    , cvLanguageCode
    , cvPhrases

    -- * Destructuring the Response
    , createVocabularyResponse
    , CreateVocabularyResponse
    -- * Response Lenses
    , cvrsFailureReason
    , cvrsLanguageCode
    , cvrsVocabularyName
    , cvrsLastModifiedTime
    , cvrsVocabularyState
    , cvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'createVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { _cvVocabularyName :: !Text
  , _cvLanguageCode   :: !LanguageCode
  , _cvPhrases        :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvVocabularyName' - The name of the vocabulary. The name must be unique within an AWS account. The name is case-sensitive.
--
-- * 'cvLanguageCode' - The language code of the vocabulary entries.
--
-- * 'cvPhrases' - An array of strings that contains the vocabulary entries.
createVocabulary
    :: Text -- ^ 'cvVocabularyName'
    -> LanguageCode -- ^ 'cvLanguageCode'
    -> CreateVocabulary
createVocabulary pVocabularyName_ pLanguageCode_ =
  CreateVocabulary'
    { _cvVocabularyName = pVocabularyName_
    , _cvLanguageCode = pLanguageCode_
    , _cvPhrases = mempty
    }


-- | The name of the vocabulary. The name must be unique within an AWS account. The name is case-sensitive.
cvVocabularyName :: Lens' CreateVocabulary Text
cvVocabularyName = lens _cvVocabularyName (\ s a -> s{_cvVocabularyName = a})

-- | The language code of the vocabulary entries.
cvLanguageCode :: Lens' CreateVocabulary LanguageCode
cvLanguageCode = lens _cvLanguageCode (\ s a -> s{_cvLanguageCode = a})

-- | An array of strings that contains the vocabulary entries.
cvPhrases :: Lens' CreateVocabulary [Text]
cvPhrases = lens _cvPhrases (\ s a -> s{_cvPhrases = a}) . _Coerce

instance AWSRequest CreateVocabulary where
        type Rs CreateVocabulary = CreateVocabularyResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 CreateVocabularyResponse' <$>
                   (x .?> "FailureReason") <*> (x .?> "LanguageCode")
                     <*> (x .?> "VocabularyName")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "VocabularyState")
                     <*> (pure (fromEnum s)))

instance Hashable CreateVocabulary where

instance NFData CreateVocabulary where

instance ToHeaders CreateVocabulary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.CreateVocabulary" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateVocabulary where
        toJSON CreateVocabulary'{..}
          = object
              (catMaybes
                 [Just ("VocabularyName" .= _cvVocabularyName),
                  Just ("LanguageCode" .= _cvLanguageCode),
                  Just ("Phrases" .= _cvPhrases)])

instance ToPath CreateVocabulary where
        toPath = const "/"

instance ToQuery CreateVocabulary where
        toQuery = const mempty

-- | /See:/ 'createVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { _cvrsFailureReason    :: !(Maybe Text)
  , _cvrsLanguageCode     :: !(Maybe LanguageCode)
  , _cvrsVocabularyName   :: !(Maybe Text)
  , _cvrsLastModifiedTime :: !(Maybe POSIX)
  , _cvrsVocabularyState  :: !(Maybe VocabularyState)
  , _cvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrsFailureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- * 'cvrsLanguageCode' - The language code of the vocabulary entries.
--
-- * 'cvrsVocabularyName' - The name of the vocabulary.
--
-- * 'cvrsLastModifiedTime' - The date and time that the vocabulary was created.
--
-- * 'cvrsVocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
--
-- * 'cvrsResponseStatus' - -- | The response status code.
createVocabularyResponse
    :: Int -- ^ 'cvrsResponseStatus'
    -> CreateVocabularyResponse
createVocabularyResponse pResponseStatus_ =
  CreateVocabularyResponse'
    { _cvrsFailureReason = Nothing
    , _cvrsLanguageCode = Nothing
    , _cvrsVocabularyName = Nothing
    , _cvrsLastModifiedTime = Nothing
    , _cvrsVocabularyState = Nothing
    , _cvrsResponseStatus = pResponseStatus_
    }


-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
cvrsFailureReason :: Lens' CreateVocabularyResponse (Maybe Text)
cvrsFailureReason = lens _cvrsFailureReason (\ s a -> s{_cvrsFailureReason = a})

-- | The language code of the vocabulary entries.
cvrsLanguageCode :: Lens' CreateVocabularyResponse (Maybe LanguageCode)
cvrsLanguageCode = lens _cvrsLanguageCode (\ s a -> s{_cvrsLanguageCode = a})

-- | The name of the vocabulary.
cvrsVocabularyName :: Lens' CreateVocabularyResponse (Maybe Text)
cvrsVocabularyName = lens _cvrsVocabularyName (\ s a -> s{_cvrsVocabularyName = a})

-- | The date and time that the vocabulary was created.
cvrsLastModifiedTime :: Lens' CreateVocabularyResponse (Maybe UTCTime)
cvrsLastModifiedTime = lens _cvrsLastModifiedTime (\ s a -> s{_cvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
cvrsVocabularyState :: Lens' CreateVocabularyResponse (Maybe VocabularyState)
cvrsVocabularyState = lens _cvrsVocabularyState (\ s a -> s{_cvrsVocabularyState = a})

-- | -- | The response status code.
cvrsResponseStatus :: Lens' CreateVocabularyResponse Int
cvrsResponseStatus = lens _cvrsResponseStatus (\ s a -> s{_cvrsResponseStatus = a})

instance NFData CreateVocabularyResponse where
