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
-- Module      : Amazonka.Comprehend.BatchDetectTargetedSentiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects a batch of documents and returns a sentiment analysis for each
-- entity identified in the documents.
--
-- For more information about targeted sentiment, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html Targeted sentiment>.
module Amazonka.Comprehend.BatchDetectTargetedSentiment
  ( -- * Creating a Request
    BatchDetectTargetedSentiment (..),
    newBatchDetectTargetedSentiment,

    -- * Request Lenses
    batchDetectTargetedSentiment_textList,
    batchDetectTargetedSentiment_languageCode,

    -- * Destructuring the Response
    BatchDetectTargetedSentimentResponse (..),
    newBatchDetectTargetedSentimentResponse,

    -- * Response Lenses
    batchDetectTargetedSentimentResponse_httpStatus,
    batchDetectTargetedSentimentResponse_resultList,
    batchDetectTargetedSentimentResponse_errorList,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDetectTargetedSentiment' smart constructor.
data BatchDetectTargetedSentiment = BatchDetectTargetedSentiment'
  { -- | A list containing the UTF-8 encoded text of the input documents. The
    -- list can contain a maximum of 25 documents. The maximum size of each
    -- document is 5 KB.
    textList :: Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)),
    -- | The language of the input documents. Currently, English is the only
    -- supported language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectTargetedSentiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectTargetedSentiment_textList' - A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
--
-- 'languageCode', 'batchDetectTargetedSentiment_languageCode' - The language of the input documents. Currently, English is the only
-- supported language.
newBatchDetectTargetedSentiment ::
  -- | 'textList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectTargetedSentiment
newBatchDetectTargetedSentiment
  pTextList_
  pLanguageCode_ =
    BatchDetectTargetedSentiment'
      { textList =
          Data._Sensitive
            Prelude.. Lens.coerced
            Lens.# pTextList_,
        languageCode = pLanguageCode_
      }

-- | A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
batchDetectTargetedSentiment_textList :: Lens.Lens' BatchDetectTargetedSentiment (Prelude.NonEmpty Prelude.Text)
batchDetectTargetedSentiment_textList = Lens.lens (\BatchDetectTargetedSentiment' {textList} -> textList) (\s@BatchDetectTargetedSentiment' {} a -> s {textList = a} :: BatchDetectTargetedSentiment) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The language of the input documents. Currently, English is the only
-- supported language.
batchDetectTargetedSentiment_languageCode :: Lens.Lens' BatchDetectTargetedSentiment LanguageCode
batchDetectTargetedSentiment_languageCode = Lens.lens (\BatchDetectTargetedSentiment' {languageCode} -> languageCode) (\s@BatchDetectTargetedSentiment' {} a -> s {languageCode = a} :: BatchDetectTargetedSentiment)

instance Core.AWSRequest BatchDetectTargetedSentiment where
  type
    AWSResponse BatchDetectTargetedSentiment =
      BatchDetectTargetedSentimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectTargetedSentimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResultList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorList" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDetectTargetedSentiment
  where
  hashWithSalt _salt BatchDetectTargetedSentiment' {..} =
    _salt
      `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData BatchDetectTargetedSentiment where
  rnf BatchDetectTargetedSentiment' {..} =
    Prelude.rnf textList `Prelude.seq`
      Prelude.rnf languageCode

instance Data.ToHeaders BatchDetectTargetedSentiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.BatchDetectTargetedSentiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDetectTargetedSentiment where
  toJSON BatchDetectTargetedSentiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Data..= textList),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath BatchDetectTargetedSentiment where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDetectTargetedSentiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectTargetedSentimentResponse' smart constructor.
data BatchDetectTargetedSentimentResponse = BatchDetectTargetedSentimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectTargetedSentimentItemResult],
    -- | List of errors that the operation can return.
    errorList :: [BatchItemError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectTargetedSentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectTargetedSentimentResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectTargetedSentimentResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectTargetedSentimentResponse_errorList' - List of errors that the operation can return.
newBatchDetectTargetedSentimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDetectTargetedSentimentResponse
newBatchDetectTargetedSentimentResponse pHttpStatus_ =
  BatchDetectTargetedSentimentResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectTargetedSentimentResponse_httpStatus :: Lens.Lens' BatchDetectTargetedSentimentResponse Prelude.Int
batchDetectTargetedSentimentResponse_httpStatus = Lens.lens (\BatchDetectTargetedSentimentResponse' {httpStatus} -> httpStatus) (\s@BatchDetectTargetedSentimentResponse' {} a -> s {httpStatus = a} :: BatchDetectTargetedSentimentResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectTargetedSentimentResponse_resultList :: Lens.Lens' BatchDetectTargetedSentimentResponse [BatchDetectTargetedSentimentItemResult]
batchDetectTargetedSentimentResponse_resultList = Lens.lens (\BatchDetectTargetedSentimentResponse' {resultList} -> resultList) (\s@BatchDetectTargetedSentimentResponse' {} a -> s {resultList = a} :: BatchDetectTargetedSentimentResponse) Prelude.. Lens.coerced

-- | List of errors that the operation can return.
batchDetectTargetedSentimentResponse_errorList :: Lens.Lens' BatchDetectTargetedSentimentResponse [BatchItemError]
batchDetectTargetedSentimentResponse_errorList = Lens.lens (\BatchDetectTargetedSentimentResponse' {errorList} -> errorList) (\s@BatchDetectTargetedSentimentResponse' {} a -> s {errorList = a} :: BatchDetectTargetedSentimentResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchDetectTargetedSentimentResponse
  where
  rnf BatchDetectTargetedSentimentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf resultList `Prelude.seq`
        Prelude.rnf errorList
