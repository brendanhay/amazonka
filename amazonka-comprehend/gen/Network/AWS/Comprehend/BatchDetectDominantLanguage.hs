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
-- Module      : Network.AWS.Comprehend.BatchDetectDominantLanguage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text for a batch of documents. For a list of languages that Amazon Comprehend can detect, see <http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages> .
--
--
module Network.AWS.Comprehend.BatchDetectDominantLanguage
    (
    -- * Creating a Request
      batchDetectDominantLanguage
    , BatchDetectDominantLanguage
    -- * Request Lenses
    , bddlTextList

    -- * Destructuring the Response
    , batchDetectDominantLanguageResponse
    , BatchDetectDominantLanguageResponse
    -- * Response Lenses
    , bddlrsResponseStatus
    , bddlrsResultList
    , bddlrsErrorList
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDetectDominantLanguage' smart constructor.
newtype BatchDetectDominantLanguage = BatchDetectDominantLanguage'
  { _bddlTextList :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectDominantLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bddlTextList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
batchDetectDominantLanguage
    :: BatchDetectDominantLanguage
batchDetectDominantLanguage =
  BatchDetectDominantLanguage' {_bddlTextList = mempty}


-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
bddlTextList :: Lens' BatchDetectDominantLanguage [Text]
bddlTextList = lens _bddlTextList (\ s a -> s{_bddlTextList = a}) . _Coerce

instance AWSRequest BatchDetectDominantLanguage where
        type Rs BatchDetectDominantLanguage =
             BatchDetectDominantLanguageResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 BatchDetectDominantLanguageResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "ResultList" .!@ mempty)
                     <*> (x .?> "ErrorList" .!@ mempty))

instance Hashable BatchDetectDominantLanguage where

instance NFData BatchDetectDominantLanguage where

instance ToHeaders BatchDetectDominantLanguage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.BatchDetectDominantLanguage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDetectDominantLanguage where
        toJSON BatchDetectDominantLanguage'{..}
          = object
              (catMaybes [Just ("TextList" .= _bddlTextList)])

instance ToPath BatchDetectDominantLanguage where
        toPath = const "/"

instance ToQuery BatchDetectDominantLanguage where
        toQuery = const mempty

-- | /See:/ 'batchDetectDominantLanguageResponse' smart constructor.
data BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse'
  { _bddlrsResponseStatus :: !Int
  , _bddlrsResultList     :: ![BatchDetectDominantLanguageItemResult]
  , _bddlrsErrorList      :: ![BatchItemError]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectDominantLanguageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bddlrsResponseStatus' - -- | The response status code.
--
-- * 'bddlrsResultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- * 'bddlrsErrorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
batchDetectDominantLanguageResponse
    :: Int -- ^ 'bddlrsResponseStatus'
    -> BatchDetectDominantLanguageResponse
batchDetectDominantLanguageResponse pResponseStatus_ =
  BatchDetectDominantLanguageResponse'
    { _bddlrsResponseStatus = pResponseStatus_
    , _bddlrsResultList = mempty
    , _bddlrsErrorList = mempty
    }


-- | -- | The response status code.
bddlrsResponseStatus :: Lens' BatchDetectDominantLanguageResponse Int
bddlrsResponseStatus = lens _bddlrsResponseStatus (\ s a -> s{_bddlrsResponseStatus = a})

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
bddlrsResultList :: Lens' BatchDetectDominantLanguageResponse [BatchDetectDominantLanguageItemResult]
bddlrsResultList = lens _bddlrsResultList (\ s a -> s{_bddlrsResultList = a}) . _Coerce

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
bddlrsErrorList :: Lens' BatchDetectDominantLanguageResponse [BatchItemError]
bddlrsErrorList = lens _bddlrsErrorList (\ s a -> s{_bddlrsErrorList = a}) . _Coerce

instance NFData BatchDetectDominantLanguageResponse
         where
