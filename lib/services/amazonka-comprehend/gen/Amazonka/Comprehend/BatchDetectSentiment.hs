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
-- Module      : Amazonka.Comprehend.BatchDetectSentiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects a batch of documents and returns an inference of the prevailing
-- sentiment, @POSITIVE@, @NEUTRAL@, @MIXED@, or @NEGATIVE@, in each one.
module Amazonka.Comprehend.BatchDetectSentiment
  ( -- * Creating a Request
    BatchDetectSentiment (..),
    newBatchDetectSentiment,

    -- * Request Lenses
    batchDetectSentiment_textList,
    batchDetectSentiment_languageCode,

    -- * Destructuring the Response
    BatchDetectSentimentResponse (..),
    newBatchDetectSentimentResponse,

    -- * Response Lenses
    batchDetectSentimentResponse_httpStatus,
    batchDetectSentimentResponse_resultList,
    batchDetectSentimentResponse_errorList,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDetectSentiment' smart constructor.
data BatchDetectSentiment = BatchDetectSentiment'
  { -- | A list containing the UTF-8 encoded text of the input documents. The
    -- list can contain a maximum of 25 documents. The maximum size of each
    -- document is 5 KB.
    --
    -- Amazon Comprehend performs real-time sentiment analysis on the first 500
    -- characters of the input text and ignores any additional text in the
    -- input.
    textList :: Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)),
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectSentiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectSentiment_textList' - A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
--
-- Amazon Comprehend performs real-time sentiment analysis on the first 500
-- characters of the input text and ignores any additional text in the
-- input.
--
-- 'languageCode', 'batchDetectSentiment_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newBatchDetectSentiment ::
  -- | 'textList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectSentiment
newBatchDetectSentiment pTextList_ pLanguageCode_ =
  BatchDetectSentiment'
    { textList =
        Data._Sensitive
          Prelude.. Lens.coerced
          Lens.# pTextList_,
      languageCode = pLanguageCode_
    }

-- | A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
--
-- Amazon Comprehend performs real-time sentiment analysis on the first 500
-- characters of the input text and ignores any additional text in the
-- input.
batchDetectSentiment_textList :: Lens.Lens' BatchDetectSentiment (Prelude.NonEmpty Prelude.Text)
batchDetectSentiment_textList = Lens.lens (\BatchDetectSentiment' {textList} -> textList) (\s@BatchDetectSentiment' {} a -> s {textList = a} :: BatchDetectSentiment) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
batchDetectSentiment_languageCode :: Lens.Lens' BatchDetectSentiment LanguageCode
batchDetectSentiment_languageCode = Lens.lens (\BatchDetectSentiment' {languageCode} -> languageCode) (\s@BatchDetectSentiment' {} a -> s {languageCode = a} :: BatchDetectSentiment)

instance Core.AWSRequest BatchDetectSentiment where
  type
    AWSResponse BatchDetectSentiment =
      BatchDetectSentimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectSentimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResultList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDetectSentiment where
  hashWithSalt _salt BatchDetectSentiment' {..} =
    _salt
      `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData BatchDetectSentiment where
  rnf BatchDetectSentiment' {..} =
    Prelude.rnf textList
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders BatchDetectSentiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.BatchDetectSentiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDetectSentiment where
  toJSON BatchDetectSentiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Data..= textList),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath BatchDetectSentiment where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDetectSentiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectSentimentResponse' smart constructor.
data BatchDetectSentimentResponse = BatchDetectSentimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectSentimentItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectSentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectSentimentResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectSentimentResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectSentimentResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectSentimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDetectSentimentResponse
newBatchDetectSentimentResponse pHttpStatus_ =
  BatchDetectSentimentResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectSentimentResponse_httpStatus :: Lens.Lens' BatchDetectSentimentResponse Prelude.Int
batchDetectSentimentResponse_httpStatus = Lens.lens (\BatchDetectSentimentResponse' {httpStatus} -> httpStatus) (\s@BatchDetectSentimentResponse' {} a -> s {httpStatus = a} :: BatchDetectSentimentResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectSentimentResponse_resultList :: Lens.Lens' BatchDetectSentimentResponse [BatchDetectSentimentItemResult]
batchDetectSentimentResponse_resultList = Lens.lens (\BatchDetectSentimentResponse' {resultList} -> resultList) (\s@BatchDetectSentimentResponse' {} a -> s {resultList = a} :: BatchDetectSentimentResponse) Prelude.. Lens.coerced

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectSentimentResponse_errorList :: Lens.Lens' BatchDetectSentimentResponse [BatchItemError]
batchDetectSentimentResponse_errorList = Lens.lens (\BatchDetectSentimentResponse' {errorList} -> errorList) (\s@BatchDetectSentimentResponse' {} a -> s {errorList = a} :: BatchDetectSentimentResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchDetectSentimentResponse where
  rnf BatchDetectSentimentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resultList
      `Prelude.seq` Prelude.rnf errorList
