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
-- Module      : Amazonka.Comprehend.ListDocumentClassifiers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the document classifiers that you have created.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListDocumentClassifiers
  ( -- * Creating a Request
    ListDocumentClassifiers (..),
    newListDocumentClassifiers,

    -- * Request Lenses
    listDocumentClassifiers_filter,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiers_nextToken,

    -- * Destructuring the Response
    ListDocumentClassifiersResponse (..),
    newListDocumentClassifiersResponse,

    -- * Response Lenses
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDocumentClassifiers' smart constructor.
data ListDocumentClassifiers = ListDocumentClassifiers'
  { -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe DocumentClassifierFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listDocumentClassifiers_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listDocumentClassifiers_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listDocumentClassifiers_nextToken' - Identifies the next page of results to return.
newListDocumentClassifiers ::
  ListDocumentClassifiers
newListDocumentClassifiers =
  ListDocumentClassifiers'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDocumentClassifiers_filter :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe DocumentClassifierFilter)
listDocumentClassifiers_filter = Lens.lens (\ListDocumentClassifiers' {filter'} -> filter') (\s@ListDocumentClassifiers' {} a -> s {filter' = a} :: ListDocumentClassifiers)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDocumentClassifiers_maxResults :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe Prelude.Natural)
listDocumentClassifiers_maxResults = Lens.lens (\ListDocumentClassifiers' {maxResults} -> maxResults) (\s@ListDocumentClassifiers' {} a -> s {maxResults = a} :: ListDocumentClassifiers)

-- | Identifies the next page of results to return.
listDocumentClassifiers_nextToken :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe Prelude.Text)
listDocumentClassifiers_nextToken = Lens.lens (\ListDocumentClassifiers' {nextToken} -> nextToken) (\s@ListDocumentClassifiers' {} a -> s {nextToken = a} :: ListDocumentClassifiers)

instance Core.AWSPager ListDocumentClassifiers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentClassifiersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentClassifiersResponse_documentClassifierPropertiesList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDocumentClassifiers_nextToken
              Lens..~ rs
              Lens.^? listDocumentClassifiersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDocumentClassifiers where
  type
    AWSResponse ListDocumentClassifiers =
      ListDocumentClassifiersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassifiersResponse'
            Prelude.<$> ( x
                            Data..?> "DocumentClassifierPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDocumentClassifiers where
  hashWithSalt _salt ListDocumentClassifiers' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDocumentClassifiers where
  rnf ListDocumentClassifiers' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListDocumentClassifiers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListDocumentClassifiers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDocumentClassifiers where
  toJSON ListDocumentClassifiers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDocumentClassifiers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDocumentClassifiers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentClassifiersResponse' smart constructor.
data ListDocumentClassifiersResponse = ListDocumentClassifiersResponse'
  { -- | A list containing the properties of each job returned.
    documentClassifierPropertiesList :: Prelude.Maybe [DocumentClassifierProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentClassifiersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierPropertiesList', 'listDocumentClassifiersResponse_documentClassifierPropertiesList' - A list containing the properties of each job returned.
--
-- 'nextToken', 'listDocumentClassifiersResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listDocumentClassifiersResponse_httpStatus' - The response's http status code.
newListDocumentClassifiersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentClassifiersResponse
newListDocumentClassifiersResponse pHttpStatus_ =
  ListDocumentClassifiersResponse'
    { documentClassifierPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job returned.
listDocumentClassifiersResponse_documentClassifierPropertiesList :: Lens.Lens' ListDocumentClassifiersResponse (Prelude.Maybe [DocumentClassifierProperties])
listDocumentClassifiersResponse_documentClassifierPropertiesList = Lens.lens (\ListDocumentClassifiersResponse' {documentClassifierPropertiesList} -> documentClassifierPropertiesList) (\s@ListDocumentClassifiersResponse' {} a -> s {documentClassifierPropertiesList = a} :: ListDocumentClassifiersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listDocumentClassifiersResponse_nextToken :: Lens.Lens' ListDocumentClassifiersResponse (Prelude.Maybe Prelude.Text)
listDocumentClassifiersResponse_nextToken = Lens.lens (\ListDocumentClassifiersResponse' {nextToken} -> nextToken) (\s@ListDocumentClassifiersResponse' {} a -> s {nextToken = a} :: ListDocumentClassifiersResponse)

-- | The response's http status code.
listDocumentClassifiersResponse_httpStatus :: Lens.Lens' ListDocumentClassifiersResponse Prelude.Int
listDocumentClassifiersResponse_httpStatus = Lens.lens (\ListDocumentClassifiersResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassifiersResponse' {} a -> s {httpStatus = a} :: ListDocumentClassifiersResponse)

instance
  Prelude.NFData
    ListDocumentClassifiersResponse
  where
  rnf ListDocumentClassifiersResponse' {..} =
    Prelude.rnf documentClassifierPropertiesList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
