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
-- Module      : Amazonka.Comprehend.ListDocumentClassifierSummaries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of summaries of the document classifiers that you have
-- created
module Amazonka.Comprehend.ListDocumentClassifierSummaries
  ( -- * Creating a Request
    ListDocumentClassifierSummaries (..),
    newListDocumentClassifierSummaries,

    -- * Request Lenses
    listDocumentClassifierSummaries_maxResults,
    listDocumentClassifierSummaries_nextToken,

    -- * Destructuring the Response
    ListDocumentClassifierSummariesResponse (..),
    newListDocumentClassifierSummariesResponse,

    -- * Response Lenses
    listDocumentClassifierSummariesResponse_documentClassifierSummariesList,
    listDocumentClassifierSummariesResponse_nextToken,
    listDocumentClassifierSummariesResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDocumentClassifierSummaries' smart constructor.
data ListDocumentClassifierSummaries = ListDocumentClassifierSummaries'
  { -- | The maximum number of results to return on each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassifierSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDocumentClassifierSummaries_maxResults' - The maximum number of results to return on each page. The default is
-- 100.
--
-- 'nextToken', 'listDocumentClassifierSummaries_nextToken' - Identifies the next page of results to return.
newListDocumentClassifierSummaries ::
  ListDocumentClassifierSummaries
newListDocumentClassifierSummaries =
  ListDocumentClassifierSummaries'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return on each page. The default is
-- 100.
listDocumentClassifierSummaries_maxResults :: Lens.Lens' ListDocumentClassifierSummaries (Prelude.Maybe Prelude.Natural)
listDocumentClassifierSummaries_maxResults = Lens.lens (\ListDocumentClassifierSummaries' {maxResults} -> maxResults) (\s@ListDocumentClassifierSummaries' {} a -> s {maxResults = a} :: ListDocumentClassifierSummaries)

-- | Identifies the next page of results to return.
listDocumentClassifierSummaries_nextToken :: Lens.Lens' ListDocumentClassifierSummaries (Prelude.Maybe Prelude.Text)
listDocumentClassifierSummaries_nextToken = Lens.lens (\ListDocumentClassifierSummaries' {nextToken} -> nextToken) (\s@ListDocumentClassifierSummaries' {} a -> s {nextToken = a} :: ListDocumentClassifierSummaries)

instance
  Core.AWSRequest
    ListDocumentClassifierSummaries
  where
  type
    AWSResponse ListDocumentClassifierSummaries =
      ListDocumentClassifierSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassifierSummariesResponse'
            Prelude.<$> ( x
                            Data..?> "DocumentClassifierSummariesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDocumentClassifierSummaries
  where
  hashWithSalt
    _salt
    ListDocumentClassifierSummaries' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListDocumentClassifierSummaries
  where
  rnf ListDocumentClassifierSummaries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListDocumentClassifierSummaries
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListDocumentClassifierSummaries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDocumentClassifierSummaries where
  toJSON ListDocumentClassifierSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDocumentClassifierSummaries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDocumentClassifierSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentClassifierSummariesResponse' smart constructor.
data ListDocumentClassifierSummariesResponse = ListDocumentClassifierSummariesResponse'
  { -- | The list of summaries of document classifiers.
    documentClassifierSummariesList :: Prelude.Maybe [DocumentClassifierSummary],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassifierSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierSummariesList', 'listDocumentClassifierSummariesResponse_documentClassifierSummariesList' - The list of summaries of document classifiers.
--
-- 'nextToken', 'listDocumentClassifierSummariesResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listDocumentClassifierSummariesResponse_httpStatus' - The response's http status code.
newListDocumentClassifierSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentClassifierSummariesResponse
newListDocumentClassifierSummariesResponse
  pHttpStatus_ =
    ListDocumentClassifierSummariesResponse'
      { documentClassifierSummariesList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of summaries of document classifiers.
listDocumentClassifierSummariesResponse_documentClassifierSummariesList :: Lens.Lens' ListDocumentClassifierSummariesResponse (Prelude.Maybe [DocumentClassifierSummary])
listDocumentClassifierSummariesResponse_documentClassifierSummariesList = Lens.lens (\ListDocumentClassifierSummariesResponse' {documentClassifierSummariesList} -> documentClassifierSummariesList) (\s@ListDocumentClassifierSummariesResponse' {} a -> s {documentClassifierSummariesList = a} :: ListDocumentClassifierSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listDocumentClassifierSummariesResponse_nextToken :: Lens.Lens' ListDocumentClassifierSummariesResponse (Prelude.Maybe Prelude.Text)
listDocumentClassifierSummariesResponse_nextToken = Lens.lens (\ListDocumentClassifierSummariesResponse' {nextToken} -> nextToken) (\s@ListDocumentClassifierSummariesResponse' {} a -> s {nextToken = a} :: ListDocumentClassifierSummariesResponse)

-- | The response's http status code.
listDocumentClassifierSummariesResponse_httpStatus :: Lens.Lens' ListDocumentClassifierSummariesResponse Prelude.Int
listDocumentClassifierSummariesResponse_httpStatus = Lens.lens (\ListDocumentClassifierSummariesResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassifierSummariesResponse' {} a -> s {httpStatus = a} :: ListDocumentClassifierSummariesResponse)

instance
  Prelude.NFData
    ListDocumentClassifierSummariesResponse
  where
  rnf ListDocumentClassifierSummariesResponse' {..} =
    Prelude.rnf documentClassifierSummariesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
