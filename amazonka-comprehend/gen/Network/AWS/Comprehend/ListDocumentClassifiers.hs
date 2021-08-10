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
-- Module      : Network.AWS.Comprehend.ListDocumentClassifiers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the document classifiers that you have created.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassifiers
  ( -- * Creating a Request
    ListDocumentClassifiers (..),
    newListDocumentClassifiers,

    -- * Request Lenses
    listDocumentClassifiers_nextToken,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiers_filter,

    -- * Destructuring the Response
    ListDocumentClassifiersResponse (..),
    newListDocumentClassifiersResponse,

    -- * Response Lenses
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDocumentClassifiers' smart constructor.
data ListDocumentClassifiers = ListDocumentClassifiers'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe DocumentClassifierFilter
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
-- 'nextToken', 'listDocumentClassifiers_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listDocumentClassifiers_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listDocumentClassifiers_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListDocumentClassifiers ::
  ListDocumentClassifiers
newListDocumentClassifiers =
  ListDocumentClassifiers'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listDocumentClassifiers_nextToken :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe Prelude.Text)
listDocumentClassifiers_nextToken = Lens.lens (\ListDocumentClassifiers' {nextToken} -> nextToken) (\s@ListDocumentClassifiers' {} a -> s {nextToken = a} :: ListDocumentClassifiers)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDocumentClassifiers_maxResults :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe Prelude.Natural)
listDocumentClassifiers_maxResults = Lens.lens (\ListDocumentClassifiers' {maxResults} -> maxResults) (\s@ListDocumentClassifiers' {} a -> s {maxResults = a} :: ListDocumentClassifiers)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDocumentClassifiers_filter :: Lens.Lens' ListDocumentClassifiers (Prelude.Maybe DocumentClassifierFilter)
listDocumentClassifiers_filter = Lens.lens (\ListDocumentClassifiers' {filter'} -> filter') (\s@ListDocumentClassifiers' {} a -> s {filter' = a} :: ListDocumentClassifiers)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassifiersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DocumentClassifierPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDocumentClassifiers

instance Prelude.NFData ListDocumentClassifiers

instance Core.ToHeaders ListDocumentClassifiers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListDocumentClassifiers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDocumentClassifiers where
  toJSON ListDocumentClassifiers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filter" Core..=) Prelude.<$> filter'
          ]
      )

instance Core.ToPath ListDocumentClassifiers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDocumentClassifiers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentClassifiersResponse' smart constructor.
data ListDocumentClassifiersResponse = ListDocumentClassifiersResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job returned.
    documentClassifierPropertiesList :: Prelude.Maybe [DocumentClassifierProperties],
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
-- 'nextToken', 'listDocumentClassifiersResponse_nextToken' - Identifies the next page of results to return.
--
-- 'documentClassifierPropertiesList', 'listDocumentClassifiersResponse_documentClassifierPropertiesList' - A list containing the properties of each job returned.
--
-- 'httpStatus', 'listDocumentClassifiersResponse_httpStatus' - The response's http status code.
newListDocumentClassifiersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentClassifiersResponse
newListDocumentClassifiersResponse pHttpStatus_ =
  ListDocumentClassifiersResponse'
    { nextToken =
        Prelude.Nothing,
      documentClassifierPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listDocumentClassifiersResponse_nextToken :: Lens.Lens' ListDocumentClassifiersResponse (Prelude.Maybe Prelude.Text)
listDocumentClassifiersResponse_nextToken = Lens.lens (\ListDocumentClassifiersResponse' {nextToken} -> nextToken) (\s@ListDocumentClassifiersResponse' {} a -> s {nextToken = a} :: ListDocumentClassifiersResponse)

-- | A list containing the properties of each job returned.
listDocumentClassifiersResponse_documentClassifierPropertiesList :: Lens.Lens' ListDocumentClassifiersResponse (Prelude.Maybe [DocumentClassifierProperties])
listDocumentClassifiersResponse_documentClassifierPropertiesList = Lens.lens (\ListDocumentClassifiersResponse' {documentClassifierPropertiesList} -> documentClassifierPropertiesList) (\s@ListDocumentClassifiersResponse' {} a -> s {documentClassifierPropertiesList = a} :: ListDocumentClassifiersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDocumentClassifiersResponse_httpStatus :: Lens.Lens' ListDocumentClassifiersResponse Prelude.Int
listDocumentClassifiersResponse_httpStatus = Lens.lens (\ListDocumentClassifiersResponse' {httpStatus} -> httpStatus) (\s@ListDocumentClassifiersResponse' {} a -> s {httpStatus = a} :: ListDocumentClassifiersResponse)

instance
  Prelude.NFData
    ListDocumentClassifiersResponse
