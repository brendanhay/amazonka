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
-- Module      : Amazonka.Kendra.ListFaqs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of FAQ lists associated with an index.
module Amazonka.Kendra.ListFaqs
  ( -- * Creating a Request
    ListFaqs (..),
    newListFaqs,

    -- * Request Lenses
    listFaqs_maxResults,
    listFaqs_nextToken,
    listFaqs_indexId,

    -- * Destructuring the Response
    ListFaqsResponse (..),
    newListFaqsResponse,

    -- * Response Lenses
    listFaqsResponse_faqSummaryItems,
    listFaqsResponse_nextToken,
    listFaqsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFaqs' smart constructor.
data ListFaqs = ListFaqs'
  { -- | The maximum number of FAQs to return in the response. If there are fewer
    -- results in the list, this response contains only the actual results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of FAQs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The index that contains the FAQ lists.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFaqs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFaqs_maxResults' - The maximum number of FAQs to return in the response. If there are fewer
-- results in the list, this response contains only the actual results.
--
-- 'nextToken', 'listFaqs_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of FAQs.
--
-- 'indexId', 'listFaqs_indexId' - The index that contains the FAQ lists.
newListFaqs ::
  -- | 'indexId'
  Prelude.Text ->
  ListFaqs
newListFaqs pIndexId_ =
  ListFaqs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | The maximum number of FAQs to return in the response. If there are fewer
-- results in the list, this response contains only the actual results.
listFaqs_maxResults :: Lens.Lens' ListFaqs (Prelude.Maybe Prelude.Natural)
listFaqs_maxResults = Lens.lens (\ListFaqs' {maxResults} -> maxResults) (\s@ListFaqs' {} a -> s {maxResults = a} :: ListFaqs)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of FAQs.
listFaqs_nextToken :: Lens.Lens' ListFaqs (Prelude.Maybe Prelude.Text)
listFaqs_nextToken = Lens.lens (\ListFaqs' {nextToken} -> nextToken) (\s@ListFaqs' {} a -> s {nextToken = a} :: ListFaqs)

-- | The index that contains the FAQ lists.
listFaqs_indexId :: Lens.Lens' ListFaqs Prelude.Text
listFaqs_indexId = Lens.lens (\ListFaqs' {indexId} -> indexId) (\s@ListFaqs' {} a -> s {indexId = a} :: ListFaqs)

instance Core.AWSRequest ListFaqs where
  type AWSResponse ListFaqs = ListFaqsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFaqsResponse'
            Prelude.<$> ( x
                            Data..?> "FaqSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFaqs where
  hashWithSalt _salt ListFaqs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListFaqs where
  rnf ListFaqs' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf indexId

instance Data.ToHeaders ListFaqs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListFaqs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFaqs where
  toJSON ListFaqs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListFaqs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFaqs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFaqsResponse' smart constructor.
data ListFaqsResponse = ListFaqsResponse'
  { -- | information about the FAQs associated with the specified index.
    faqSummaryItems :: Prelude.Maybe [FaqSummary],
    -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of FAQs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFaqsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faqSummaryItems', 'listFaqsResponse_faqSummaryItems' - information about the FAQs associated with the specified index.
--
-- 'nextToken', 'listFaqsResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of FAQs.
--
-- 'httpStatus', 'listFaqsResponse_httpStatus' - The response's http status code.
newListFaqsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFaqsResponse
newListFaqsResponse pHttpStatus_ =
  ListFaqsResponse'
    { faqSummaryItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | information about the FAQs associated with the specified index.
listFaqsResponse_faqSummaryItems :: Lens.Lens' ListFaqsResponse (Prelude.Maybe [FaqSummary])
listFaqsResponse_faqSummaryItems = Lens.lens (\ListFaqsResponse' {faqSummaryItems} -> faqSummaryItems) (\s@ListFaqsResponse' {} a -> s {faqSummaryItems = a} :: ListFaqsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of FAQs.
listFaqsResponse_nextToken :: Lens.Lens' ListFaqsResponse (Prelude.Maybe Prelude.Text)
listFaqsResponse_nextToken = Lens.lens (\ListFaqsResponse' {nextToken} -> nextToken) (\s@ListFaqsResponse' {} a -> s {nextToken = a} :: ListFaqsResponse)

-- | The response's http status code.
listFaqsResponse_httpStatus :: Lens.Lens' ListFaqsResponse Prelude.Int
listFaqsResponse_httpStatus = Lens.lens (\ListFaqsResponse' {httpStatus} -> httpStatus) (\s@ListFaqsResponse' {} a -> s {httpStatus = a} :: ListFaqsResponse)

instance Prelude.NFData ListFaqsResponse where
  rnf ListFaqsResponse' {..} =
    Prelude.rnf faqSummaryItems `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
