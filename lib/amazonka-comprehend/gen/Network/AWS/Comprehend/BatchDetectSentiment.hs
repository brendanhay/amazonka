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
-- Module      : Network.AWS.Comprehend.BatchDetectSentiment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects a batch of documents and returns an inference of the prevailing sentiment, @POSITIVE@ , @NEUTRAL@ , @MIXED@ , or @NEGATIVE@ , in each one.
--
--
module Network.AWS.Comprehend.BatchDetectSentiment
    (
    -- * Creating a Request
      batchDetectSentiment
    , BatchDetectSentiment
    -- * Request Lenses
    , bdsTextList
    , bdsLanguageCode

    -- * Destructuring the Response
    , batchDetectSentimentResponse
    , BatchDetectSentimentResponse
    -- * Response Lenses
    , bdsrsResponseStatus
    , bdsrsResultList
    , bdsrsErrorList
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDetectSentiment' smart constructor.
data BatchDetectSentiment = BatchDetectSentiment'
  { _bdsTextList     :: ![Text]
  , _bdsLanguageCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectSentiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsTextList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'bdsLanguageCode' - The language of the input documents. All documents must be in the same language.
batchDetectSentiment
    :: Text -- ^ 'bdsLanguageCode'
    -> BatchDetectSentiment
batchDetectSentiment pLanguageCode_ =
  BatchDetectSentiment'
    {_bdsTextList = mempty, _bdsLanguageCode = pLanguageCode_}


-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
bdsTextList :: Lens' BatchDetectSentiment [Text]
bdsTextList = lens _bdsTextList (\ s a -> s{_bdsTextList = a}) . _Coerce

-- | The language of the input documents. All documents must be in the same language.
bdsLanguageCode :: Lens' BatchDetectSentiment Text
bdsLanguageCode = lens _bdsLanguageCode (\ s a -> s{_bdsLanguageCode = a})

instance AWSRequest BatchDetectSentiment where
        type Rs BatchDetectSentiment =
             BatchDetectSentimentResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 BatchDetectSentimentResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "ResultList" .!@ mempty)
                     <*> (x .?> "ErrorList" .!@ mempty))

instance Hashable BatchDetectSentiment where

instance NFData BatchDetectSentiment where

instance ToHeaders BatchDetectSentiment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.BatchDetectSentiment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDetectSentiment where
        toJSON BatchDetectSentiment'{..}
          = object
              (catMaybes
                 [Just ("TextList" .= _bdsTextList),
                  Just ("LanguageCode" .= _bdsLanguageCode)])

instance ToPath BatchDetectSentiment where
        toPath = const "/"

instance ToQuery BatchDetectSentiment where
        toQuery = const mempty

-- | /See:/ 'batchDetectSentimentResponse' smart constructor.
data BatchDetectSentimentResponse = BatchDetectSentimentResponse'
  { _bdsrsResponseStatus :: !Int
  , _bdsrsResultList     :: ![BatchDetectSentimentItemResult]
  , _bdsrsErrorList      :: ![BatchItemError]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectSentimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsrsResponseStatus' - -- | The response status code.
--
-- * 'bdsrsResultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- * 'bdsrsErrorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
batchDetectSentimentResponse
    :: Int -- ^ 'bdsrsResponseStatus'
    -> BatchDetectSentimentResponse
batchDetectSentimentResponse pResponseStatus_ =
  BatchDetectSentimentResponse'
    { _bdsrsResponseStatus = pResponseStatus_
    , _bdsrsResultList = mempty
    , _bdsrsErrorList = mempty
    }


-- | -- | The response status code.
bdsrsResponseStatus :: Lens' BatchDetectSentimentResponse Int
bdsrsResponseStatus = lens _bdsrsResponseStatus (\ s a -> s{_bdsrsResponseStatus = a})

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
bdsrsResultList :: Lens' BatchDetectSentimentResponse [BatchDetectSentimentItemResult]
bdsrsResultList = lens _bdsrsResultList (\ s a -> s{_bdsrsResultList = a}) . _Coerce

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
bdsrsErrorList :: Lens' BatchDetectSentimentResponse [BatchItemError]
bdsrsErrorList = lens _bdsrsErrorList (\ s a -> s{_bdsrsErrorList = a}) . _Coerce

instance NFData BatchDetectSentimentResponse where
