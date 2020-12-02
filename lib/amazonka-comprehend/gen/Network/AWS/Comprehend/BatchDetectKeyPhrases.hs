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
-- Module      : Network.AWS.Comprehend.BatchDetectKeyPhrases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in a batch of documents.
--
--
module Network.AWS.Comprehend.BatchDetectKeyPhrases
    (
    -- * Creating a Request
      batchDetectKeyPhrases
    , BatchDetectKeyPhrases
    -- * Request Lenses
    , bdkpTextList
    , bdkpLanguageCode

    -- * Destructuring the Response
    , batchDetectKeyPhrasesResponse
    , BatchDetectKeyPhrasesResponse
    -- * Response Lenses
    , bdkprsResponseStatus
    , bdkprsResultList
    , bdkprsErrorList
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDetectKeyPhrases' smart constructor.
data BatchDetectKeyPhrases = BatchDetectKeyPhrases'
  { _bdkpTextList     :: ![Text]
  , _bdkpLanguageCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectKeyPhrases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdkpTextList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'bdkpLanguageCode' - The language of the input documents. All documents must be in the same language.
batchDetectKeyPhrases
    :: Text -- ^ 'bdkpLanguageCode'
    -> BatchDetectKeyPhrases
batchDetectKeyPhrases pLanguageCode_ =
  BatchDetectKeyPhrases'
    {_bdkpTextList = mempty, _bdkpLanguageCode = pLanguageCode_}


-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
bdkpTextList :: Lens' BatchDetectKeyPhrases [Text]
bdkpTextList = lens _bdkpTextList (\ s a -> s{_bdkpTextList = a}) . _Coerce

-- | The language of the input documents. All documents must be in the same language.
bdkpLanguageCode :: Lens' BatchDetectKeyPhrases Text
bdkpLanguageCode = lens _bdkpLanguageCode (\ s a -> s{_bdkpLanguageCode = a})

instance AWSRequest BatchDetectKeyPhrases where
        type Rs BatchDetectKeyPhrases =
             BatchDetectKeyPhrasesResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 BatchDetectKeyPhrasesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "ResultList" .!@ mempty)
                     <*> (x .?> "ErrorList" .!@ mempty))

instance Hashable BatchDetectKeyPhrases where

instance NFData BatchDetectKeyPhrases where

instance ToHeaders BatchDetectKeyPhrases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.BatchDetectKeyPhrases" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDetectKeyPhrases where
        toJSON BatchDetectKeyPhrases'{..}
          = object
              (catMaybes
                 [Just ("TextList" .= _bdkpTextList),
                  Just ("LanguageCode" .= _bdkpLanguageCode)])

instance ToPath BatchDetectKeyPhrases where
        toPath = const "/"

instance ToQuery BatchDetectKeyPhrases where
        toQuery = const mempty

-- | /See:/ 'batchDetectKeyPhrasesResponse' smart constructor.
data BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse'
  { _bdkprsResponseStatus :: !Int
  , _bdkprsResultList     :: ![BatchDetectKeyPhrasesItemResult]
  , _bdkprsErrorList      :: ![BatchItemError]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectKeyPhrasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdkprsResponseStatus' - -- | The response status code.
--
-- * 'bdkprsResultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- * 'bdkprsErrorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
batchDetectKeyPhrasesResponse
    :: Int -- ^ 'bdkprsResponseStatus'
    -> BatchDetectKeyPhrasesResponse
batchDetectKeyPhrasesResponse pResponseStatus_ =
  BatchDetectKeyPhrasesResponse'
    { _bdkprsResponseStatus = pResponseStatus_
    , _bdkprsResultList = mempty
    , _bdkprsErrorList = mempty
    }


-- | -- | The response status code.
bdkprsResponseStatus :: Lens' BatchDetectKeyPhrasesResponse Int
bdkprsResponseStatus = lens _bdkprsResponseStatus (\ s a -> s{_bdkprsResponseStatus = a})

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
bdkprsResultList :: Lens' BatchDetectKeyPhrasesResponse [BatchDetectKeyPhrasesItemResult]
bdkprsResultList = lens _bdkprsResultList (\ s a -> s{_bdkprsResultList = a}) . _Coerce

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
bdkprsErrorList :: Lens' BatchDetectKeyPhrasesResponse [BatchItemError]
bdkprsErrorList = lens _bdkprsErrorList (\ s a -> s{_bdkprsErrorList = a}) . _Coerce

instance NFData BatchDetectKeyPhrasesResponse where
