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
-- Module      : Network.AWS.Comprehend.BatchDetectSyntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for the syntax and part of speech of the words in the document and returns information about them. For more information, see 'how-syntax' .
module Network.AWS.Comprehend.BatchDetectSyntax
  ( -- * Creating a Request
    batchDetectSyntax,
    BatchDetectSyntax,

    -- * Request Lenses
    bTextList,
    bLanguageCode,

    -- * Destructuring the Response
    batchDetectSyntaxResponse,
    BatchDetectSyntaxResponse,

    -- * Response Lenses
    brsResponseStatus,
    brsResultList,
    brsErrorList,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDetectSyntax' smart constructor.
data BatchDetectSyntax = BatchDetectSyntax'
  { _bTextList ::
      !(Sensitive [Sensitive Text]),
    _bLanguageCode :: !SyntaxLanguageCode
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectSyntax' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bTextList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'bLanguageCode' - The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
batchDetectSyntax ::
  -- | 'bLanguageCode'
  SyntaxLanguageCode ->
  BatchDetectSyntax
batchDetectSyntax pLanguageCode_ =
  BatchDetectSyntax'
    { _bTextList = mempty,
      _bLanguageCode = pLanguageCode_
    }

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
bTextList :: Lens' BatchDetectSyntax [Text]
bTextList = lens _bTextList (\s a -> s {_bTextList = a}) . _Sensitive . _Coerce

-- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
bLanguageCode :: Lens' BatchDetectSyntax SyntaxLanguageCode
bLanguageCode = lens _bLanguageCode (\s a -> s {_bLanguageCode = a})

instance AWSRequest BatchDetectSyntax where
  type Rs BatchDetectSyntax = BatchDetectSyntaxResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          BatchDetectSyntaxResponse'
            <$> (pure (fromEnum s))
            <*> (x .?> "ResultList" .!@ mempty)
            <*> (x .?> "ErrorList" .!@ mempty)
      )

instance Hashable BatchDetectSyntax

instance NFData BatchDetectSyntax

instance ToHeaders BatchDetectSyntax where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.BatchDetectSyntax" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchDetectSyntax where
  toJSON BatchDetectSyntax' {..} =
    object
      ( catMaybes
          [ Just ("TextList" .= _bTextList),
            Just ("LanguageCode" .= _bLanguageCode)
          ]
      )

instance ToPath BatchDetectSyntax where
  toPath = const "/"

instance ToQuery BatchDetectSyntax where
  toQuery = const mempty

-- | /See:/ 'batchDetectSyntaxResponse' smart constructor.
data BatchDetectSyntaxResponse = BatchDetectSyntaxResponse'
  { _brsResponseStatus ::
      !Int,
    _brsResultList ::
      ![BatchDetectSyntaxItemResult],
    _brsErrorList :: ![BatchItemError]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectSyntaxResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsResponseStatus' - -- | The response status code.
--
-- * 'brsResultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- * 'brsErrorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
batchDetectSyntaxResponse ::
  -- | 'brsResponseStatus'
  Int ->
  BatchDetectSyntaxResponse
batchDetectSyntaxResponse pResponseStatus_ =
  BatchDetectSyntaxResponse'
    { _brsResponseStatus = pResponseStatus_,
      _brsResultList = mempty,
      _brsErrorList = mempty
    }

-- | -- | The response status code.
brsResponseStatus :: Lens' BatchDetectSyntaxResponse Int
brsResponseStatus = lens _brsResponseStatus (\s a -> s {_brsResponseStatus = a})

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
brsResultList :: Lens' BatchDetectSyntaxResponse [BatchDetectSyntaxItemResult]
brsResultList = lens _brsResultList (\s a -> s {_brsResultList = a}) . _Coerce

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
brsErrorList :: Lens' BatchDetectSyntaxResponse [BatchItemError]
brsErrorList = lens _brsErrorList (\s a -> s {_brsErrorList = a}) . _Coerce

instance NFData BatchDetectSyntaxResponse
