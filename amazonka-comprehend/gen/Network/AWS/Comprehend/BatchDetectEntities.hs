{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for named entities and returns
-- information about them. For more information about named entities, see
-- how-entities
module Network.AWS.Comprehend.BatchDetectEntities
  ( -- * Creating a Request
    BatchDetectEntities (..),
    newBatchDetectEntities,

    -- * Request Lenses
    batchDetectEntities_textList,
    batchDetectEntities_languageCode,

    -- * Destructuring the Response
    BatchDetectEntitiesResponse (..),
    newBatchDetectEntitiesResponse,

    -- * Response Lenses
    batchDetectEntitiesResponse_httpStatus,
    batchDetectEntitiesResponse_resultList,
    batchDetectEntitiesResponse_errorList,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDetectEntities' smart constructor.
data BatchDetectEntities = BatchDetectEntities'
  { -- | A list containing the text of the input documents. The list can contain
    -- a maximum of 25 documents. Each document must contain fewer than 5,000
    -- bytes of UTF-8 encoded characters.
    textList :: Prelude.Sensitive [Prelude.Sensitive Prelude.Text],
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectEntities_textList' - A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer than 5,000
-- bytes of UTF-8 encoded characters.
--
-- 'languageCode', 'batchDetectEntities_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newBatchDetectEntities ::
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectEntities
newBatchDetectEntities pLanguageCode_ =
  BatchDetectEntities'
    { textList = Prelude.mempty,
      languageCode = pLanguageCode_
    }

-- | A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer than 5,000
-- bytes of UTF-8 encoded characters.
batchDetectEntities_textList :: Lens.Lens' BatchDetectEntities [Prelude.Text]
batchDetectEntities_textList = Lens.lens (\BatchDetectEntities' {textList} -> textList) (\s@BatchDetectEntities' {} a -> s {textList = a} :: BatchDetectEntities) Prelude.. Prelude._Sensitive Prelude.. Prelude._Coerce

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
batchDetectEntities_languageCode :: Lens.Lens' BatchDetectEntities LanguageCode
batchDetectEntities_languageCode = Lens.lens (\BatchDetectEntities' {languageCode} -> languageCode) (\s@BatchDetectEntities' {} a -> s {languageCode = a} :: BatchDetectEntities)

instance Prelude.AWSRequest BatchDetectEntities where
  type
    Rs BatchDetectEntities =
      BatchDetectEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectEntitiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "ResultList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "ErrorList"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchDetectEntities

instance Prelude.NFData BatchDetectEntities

instance Prelude.ToHeaders BatchDetectEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.BatchDetectEntities" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchDetectEntities where
  toJSON BatchDetectEntities' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Prelude..= textList),
            Prelude.Just
              ("LanguageCode" Prelude..= languageCode)
          ]
      )

instance Prelude.ToPath BatchDetectEntities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchDetectEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectEntitiesResponse' smart constructor.
data BatchDetectEntitiesResponse = BatchDetectEntitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectEntitiesItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectEntitiesResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectEntitiesResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectEntitiesResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDetectEntitiesResponse
newBatchDetectEntitiesResponse pHttpStatus_ =
  BatchDetectEntitiesResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectEntitiesResponse_httpStatus :: Lens.Lens' BatchDetectEntitiesResponse Prelude.Int
batchDetectEntitiesResponse_httpStatus = Lens.lens (\BatchDetectEntitiesResponse' {httpStatus} -> httpStatus) (\s@BatchDetectEntitiesResponse' {} a -> s {httpStatus = a} :: BatchDetectEntitiesResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectEntitiesResponse_resultList :: Lens.Lens' BatchDetectEntitiesResponse [BatchDetectEntitiesItemResult]
batchDetectEntitiesResponse_resultList = Lens.lens (\BatchDetectEntitiesResponse' {resultList} -> resultList) (\s@BatchDetectEntitiesResponse' {} a -> s {resultList = a} :: BatchDetectEntitiesResponse) Prelude.. Prelude._Coerce

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectEntitiesResponse_errorList :: Lens.Lens' BatchDetectEntitiesResponse [BatchItemError]
batchDetectEntitiesResponse_errorList = Lens.lens (\BatchDetectEntitiesResponse' {errorList} -> errorList) (\s@BatchDetectEntitiesResponse' {} a -> s {errorList = a} :: BatchDetectEntitiesResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData BatchDetectEntitiesResponse
