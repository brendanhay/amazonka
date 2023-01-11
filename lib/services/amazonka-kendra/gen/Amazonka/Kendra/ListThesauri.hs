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
-- Module      : Amazonka.Kendra.ListThesauri
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the thesauri for an index.
module Amazonka.Kendra.ListThesauri
  ( -- * Creating a Request
    ListThesauri (..),
    newListThesauri,

    -- * Request Lenses
    listThesauri_maxResults,
    listThesauri_nextToken,
    listThesauri_indexId,

    -- * Destructuring the Response
    ListThesauriResponse (..),
    newListThesauriResponse,

    -- * Response Lenses
    listThesauriResponse_nextToken,
    listThesauriResponse_thesaurusSummaryItems,
    listThesauriResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThesauri' smart constructor.
data ListThesauri = ListThesauri'
  { -- | The maximum number of thesauri to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of thesauri
    -- (@ThesaurusSummaryItems@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index with one or more thesauri.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThesauri' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThesauri_maxResults' - The maximum number of thesauri to return.
--
-- 'nextToken', 'listThesauri_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of thesauri
-- (@ThesaurusSummaryItems@).
--
-- 'indexId', 'listThesauri_indexId' - The identifier of the index with one or more thesauri.
newListThesauri ::
  -- | 'indexId'
  Prelude.Text ->
  ListThesauri
newListThesauri pIndexId_ =
  ListThesauri'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | The maximum number of thesauri to return.
listThesauri_maxResults :: Lens.Lens' ListThesauri (Prelude.Maybe Prelude.Natural)
listThesauri_maxResults = Lens.lens (\ListThesauri' {maxResults} -> maxResults) (\s@ListThesauri' {} a -> s {maxResults = a} :: ListThesauri)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of thesauri
-- (@ThesaurusSummaryItems@).
listThesauri_nextToken :: Lens.Lens' ListThesauri (Prelude.Maybe Prelude.Text)
listThesauri_nextToken = Lens.lens (\ListThesauri' {nextToken} -> nextToken) (\s@ListThesauri' {} a -> s {nextToken = a} :: ListThesauri)

-- | The identifier of the index with one or more thesauri.
listThesauri_indexId :: Lens.Lens' ListThesauri Prelude.Text
listThesauri_indexId = Lens.lens (\ListThesauri' {indexId} -> indexId) (\s@ListThesauri' {} a -> s {indexId = a} :: ListThesauri)

instance Core.AWSRequest ListThesauri where
  type AWSResponse ListThesauri = ListThesauriResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThesauriResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ThesaurusSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThesauri where
  hashWithSalt _salt ListThesauri' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListThesauri where
  rnf ListThesauri' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders ListThesauri where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListThesauri" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListThesauri where
  toJSON ListThesauri' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListThesauri where
  toPath = Prelude.const "/"

instance Data.ToQuery ListThesauri where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListThesauriResponse' smart constructor.
data ListThesauriResponse = ListThesauriResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of thesauri.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for a thesaurus or multiple thesauri.
    thesaurusSummaryItems :: Prelude.Maybe [ThesaurusSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThesauriResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThesauriResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of thesauri.
--
-- 'thesaurusSummaryItems', 'listThesauriResponse_thesaurusSummaryItems' - An array of summary information for a thesaurus or multiple thesauri.
--
-- 'httpStatus', 'listThesauriResponse_httpStatus' - The response's http status code.
newListThesauriResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThesauriResponse
newListThesauriResponse pHttpStatus_ =
  ListThesauriResponse'
    { nextToken = Prelude.Nothing,
      thesaurusSummaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of thesauri.
listThesauriResponse_nextToken :: Lens.Lens' ListThesauriResponse (Prelude.Maybe Prelude.Text)
listThesauriResponse_nextToken = Lens.lens (\ListThesauriResponse' {nextToken} -> nextToken) (\s@ListThesauriResponse' {} a -> s {nextToken = a} :: ListThesauriResponse)

-- | An array of summary information for a thesaurus or multiple thesauri.
listThesauriResponse_thesaurusSummaryItems :: Lens.Lens' ListThesauriResponse (Prelude.Maybe [ThesaurusSummary])
listThesauriResponse_thesaurusSummaryItems = Lens.lens (\ListThesauriResponse' {thesaurusSummaryItems} -> thesaurusSummaryItems) (\s@ListThesauriResponse' {} a -> s {thesaurusSummaryItems = a} :: ListThesauriResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThesauriResponse_httpStatus :: Lens.Lens' ListThesauriResponse Prelude.Int
listThesauriResponse_httpStatus = Lens.lens (\ListThesauriResponse' {httpStatus} -> httpStatus) (\s@ListThesauriResponse' {} a -> s {httpStatus = a} :: ListThesauriResponse)

instance Prelude.NFData ListThesauriResponse where
  rnf ListThesauriResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thesaurusSummaryItems
      `Prelude.seq` Prelude.rnf httpStatus
