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
-- Module      : Network.AWS.Transcribe.UpdateVocabulary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values.
--
--
module Network.AWS.Transcribe.UpdateVocabulary
    (
    -- * Creating a Request
      updateVocabulary
    , UpdateVocabulary
    -- * Request Lenses
    , uvVocabularyName
    , uvLanguageCode
    , uvPhrases

    -- * Destructuring the Response
    , updateVocabularyResponse
    , UpdateVocabularyResponse
    -- * Response Lenses
    , uvrsLanguageCode
    , uvrsVocabularyName
    , uvrsLastModifiedTime
    , uvrsVocabularyState
    , uvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'updateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { _uvVocabularyName :: !Text
  , _uvLanguageCode   :: !LanguageCode
  , _uvPhrases        :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvVocabularyName' - The name of the vocabulary to update. The name is case-sensitive.
--
-- * 'uvLanguageCode' - The language code of the vocabulary entries.
--
-- * 'uvPhrases' - An array of strings containing the vocabulary entries.
updateVocabulary
    :: Text -- ^ 'uvVocabularyName'
    -> LanguageCode -- ^ 'uvLanguageCode'
    -> UpdateVocabulary
updateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { _uvVocabularyName = pVocabularyName_
    , _uvLanguageCode = pLanguageCode_
    , _uvPhrases = mempty
    }


-- | The name of the vocabulary to update. The name is case-sensitive.
uvVocabularyName :: Lens' UpdateVocabulary Text
uvVocabularyName = lens _uvVocabularyName (\ s a -> s{_uvVocabularyName = a})

-- | The language code of the vocabulary entries.
uvLanguageCode :: Lens' UpdateVocabulary LanguageCode
uvLanguageCode = lens _uvLanguageCode (\ s a -> s{_uvLanguageCode = a})

-- | An array of strings containing the vocabulary entries.
uvPhrases :: Lens' UpdateVocabulary [Text]
uvPhrases = lens _uvPhrases (\ s a -> s{_uvPhrases = a}) . _Coerce

instance AWSRequest UpdateVocabulary where
        type Rs UpdateVocabulary = UpdateVocabularyResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVocabularyResponse' <$>
                   (x .?> "LanguageCode") <*> (x .?> "VocabularyName")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "VocabularyState")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateVocabulary where

instance NFData UpdateVocabulary where

instance ToHeaders UpdateVocabulary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.UpdateVocabulary" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVocabulary where
        toJSON UpdateVocabulary'{..}
          = object
              (catMaybes
                 [Just ("VocabularyName" .= _uvVocabularyName),
                  Just ("LanguageCode" .= _uvLanguageCode),
                  Just ("Phrases" .= _uvPhrases)])

instance ToPath UpdateVocabulary where
        toPath = const "/"

instance ToQuery UpdateVocabulary where
        toQuery = const mempty

-- | /See:/ 'updateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { _uvrsLanguageCode     :: !(Maybe LanguageCode)
  , _uvrsVocabularyName   :: !(Maybe Text)
  , _uvrsLastModifiedTime :: !(Maybe POSIX)
  , _uvrsVocabularyState  :: !(Maybe VocabularyState)
  , _uvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
updateVocabularyResponse
    :: Int -- ^ 'uvrsResponseStatus'
    -> UpdateVocabularyResponse
updateVocabularyResponse pResponseStatus_ =
  UpdateVocabularyResponse'
    { _uvrsLanguageCode = Nothing
    , _uvrsVocabularyName = Nothing
    , _uvrsLastModifiedTime = Nothing
    , _uvrsVocabularyState = Nothing
    , _uvrsResponseStatus = pResponseStatus_
    }


-- | The language code of the vocabulary entries.
uvrsLanguageCode :: Lens' UpdateVocabularyResponse (Maybe LanguageCode)
uvrsLanguageCode = lens _uvrsLanguageCode (\ s a -> s{_uvrsLanguageCode = a})

-- | The name of the vocabulary that was updated.
uvrsVocabularyName :: Lens' UpdateVocabularyResponse (Maybe Text)
uvrsVocabularyName = lens _uvrsVocabularyName (\ s a -> s{_uvrsVocabularyName = a})

-- | The date and time that the vocabulary was updated.
uvrsLastModifiedTime :: Lens' UpdateVocabularyResponse (Maybe UTCTime)
uvrsLastModifiedTime = lens _uvrsLastModifiedTime (\ s a -> s{_uvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
uvrsVocabularyState :: Lens' UpdateVocabularyResponse (Maybe VocabularyState)
uvrsVocabularyState = lens _uvrsVocabularyState (\ s a -> s{_uvrsVocabularyState = a})

-- | -- | The response status code.
uvrsResponseStatus :: Lens' UpdateVocabularyResponse Int
uvrsResponseStatus = lens _uvrsResponseStatus (\ s a -> s{_uvrsResponseStatus = a})

instance NFData UpdateVocabularyResponse where
