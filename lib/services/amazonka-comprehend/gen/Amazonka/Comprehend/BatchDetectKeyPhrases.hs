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
-- Module      : Amazonka.Comprehend.BatchDetectKeyPhrases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in a batch of documents.
module Amazonka.Comprehend.BatchDetectKeyPhrases
  ( -- * Creating a Request
    BatchDetectKeyPhrases (..),
    newBatchDetectKeyPhrases,

    -- * Request Lenses
    batchDetectKeyPhrases_textList,
    batchDetectKeyPhrases_languageCode,

    -- * Destructuring the Response
    BatchDetectKeyPhrasesResponse (..),
    newBatchDetectKeyPhrasesResponse,

    -- * Response Lenses
    batchDetectKeyPhrasesResponse_httpStatus,
    batchDetectKeyPhrasesResponse_resultList,
    batchDetectKeyPhrasesResponse_errorList,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDetectKeyPhrases' smart constructor.
data BatchDetectKeyPhrases = BatchDetectKeyPhrases'
  { -- | A list containing the UTF-8 encoded text of the input documents. The
    -- list can contain a maximum of 25 documents. The maximum size of each
    -- document is 5 KB.
    textList :: Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)),
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectKeyPhrases_textList' - A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
--
-- 'languageCode', 'batchDetectKeyPhrases_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newBatchDetectKeyPhrases ::
  -- | 'textList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectKeyPhrases
newBatchDetectKeyPhrases pTextList_ pLanguageCode_ =
  BatchDetectKeyPhrases'
    { textList =
        Data._Sensitive
          Prelude.. Lens.coerced
          Lens.# pTextList_,
      languageCode = pLanguageCode_
    }

-- | A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
batchDetectKeyPhrases_textList :: Lens.Lens' BatchDetectKeyPhrases (Prelude.NonEmpty Prelude.Text)
batchDetectKeyPhrases_textList = Lens.lens (\BatchDetectKeyPhrases' {textList} -> textList) (\s@BatchDetectKeyPhrases' {} a -> s {textList = a} :: BatchDetectKeyPhrases) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
batchDetectKeyPhrases_languageCode :: Lens.Lens' BatchDetectKeyPhrases LanguageCode
batchDetectKeyPhrases_languageCode = Lens.lens (\BatchDetectKeyPhrases' {languageCode} -> languageCode) (\s@BatchDetectKeyPhrases' {} a -> s {languageCode = a} :: BatchDetectKeyPhrases)

instance Core.AWSRequest BatchDetectKeyPhrases where
  type
    AWSResponse BatchDetectKeyPhrases =
      BatchDetectKeyPhrasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectKeyPhrasesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResultList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDetectKeyPhrases where
  hashWithSalt _salt BatchDetectKeyPhrases' {..} =
    _salt
      `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData BatchDetectKeyPhrases where
  rnf BatchDetectKeyPhrases' {..} =
    Prelude.rnf textList `Prelude.seq`
      Prelude.rnf languageCode

instance Data.ToHeaders BatchDetectKeyPhrases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.BatchDetectKeyPhrases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDetectKeyPhrases where
  toJSON BatchDetectKeyPhrases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Data..= textList),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath BatchDetectKeyPhrases where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDetectKeyPhrases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectKeyPhrasesResponse' smart constructor.
data BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectKeyPhrasesItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectKeyPhrasesResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectKeyPhrasesResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectKeyPhrasesResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectKeyPhrasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDetectKeyPhrasesResponse
newBatchDetectKeyPhrasesResponse pHttpStatus_ =
  BatchDetectKeyPhrasesResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectKeyPhrasesResponse_httpStatus :: Lens.Lens' BatchDetectKeyPhrasesResponse Prelude.Int
batchDetectKeyPhrasesResponse_httpStatus = Lens.lens (\BatchDetectKeyPhrasesResponse' {httpStatus} -> httpStatus) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {httpStatus = a} :: BatchDetectKeyPhrasesResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectKeyPhrasesResponse_resultList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchDetectKeyPhrasesItemResult]
batchDetectKeyPhrasesResponse_resultList = Lens.lens (\BatchDetectKeyPhrasesResponse' {resultList} -> resultList) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {resultList = a} :: BatchDetectKeyPhrasesResponse) Prelude.. Lens.coerced

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectKeyPhrasesResponse_errorList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchItemError]
batchDetectKeyPhrasesResponse_errorList = Lens.lens (\BatchDetectKeyPhrasesResponse' {errorList} -> errorList) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {errorList = a} :: BatchDetectKeyPhrasesResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchDetectKeyPhrasesResponse where
  rnf BatchDetectKeyPhrasesResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf resultList `Prelude.seq`
        Prelude.rnf errorList
