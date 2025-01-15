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
-- Module      : Amazonka.SSMContacts.ListPagesByEngagement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the engagements to contact channels that occurred by engaging a
-- contact.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListPagesByEngagement
  ( -- * Creating a Request
    ListPagesByEngagement (..),
    newListPagesByEngagement,

    -- * Request Lenses
    listPagesByEngagement_maxResults,
    listPagesByEngagement_nextToken,
    listPagesByEngagement_engagementId,

    -- * Destructuring the Response
    ListPagesByEngagementResponse (..),
    newListPagesByEngagementResponse,

    -- * Response Lenses
    listPagesByEngagementResponse_nextToken,
    listPagesByEngagementResponse_httpStatus,
    listPagesByEngagementResponse_pages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListPagesByEngagement' smart constructor.
data ListPagesByEngagement = ListPagesByEngagement'
  { -- | The maximum number of engagements to contact channels to list per page
    -- of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the engagement.
    engagementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPagesByEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPagesByEngagement_maxResults' - The maximum number of engagements to contact channels to list per page
-- of results.
--
-- 'nextToken', 'listPagesByEngagement_nextToken' - The pagination token to continue to the next page of results.
--
-- 'engagementId', 'listPagesByEngagement_engagementId' - The Amazon Resource Name (ARN) of the engagement.
newListPagesByEngagement ::
  -- | 'engagementId'
  Prelude.Text ->
  ListPagesByEngagement
newListPagesByEngagement pEngagementId_ =
  ListPagesByEngagement'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      engagementId = pEngagementId_
    }

-- | The maximum number of engagements to contact channels to list per page
-- of results.
listPagesByEngagement_maxResults :: Lens.Lens' ListPagesByEngagement (Prelude.Maybe Prelude.Natural)
listPagesByEngagement_maxResults = Lens.lens (\ListPagesByEngagement' {maxResults} -> maxResults) (\s@ListPagesByEngagement' {} a -> s {maxResults = a} :: ListPagesByEngagement)

-- | The pagination token to continue to the next page of results.
listPagesByEngagement_nextToken :: Lens.Lens' ListPagesByEngagement (Prelude.Maybe Prelude.Text)
listPagesByEngagement_nextToken = Lens.lens (\ListPagesByEngagement' {nextToken} -> nextToken) (\s@ListPagesByEngagement' {} a -> s {nextToken = a} :: ListPagesByEngagement)

-- | The Amazon Resource Name (ARN) of the engagement.
listPagesByEngagement_engagementId :: Lens.Lens' ListPagesByEngagement Prelude.Text
listPagesByEngagement_engagementId = Lens.lens (\ListPagesByEngagement' {engagementId} -> engagementId) (\s@ListPagesByEngagement' {} a -> s {engagementId = a} :: ListPagesByEngagement)

instance Core.AWSPager ListPagesByEngagement where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPagesByEngagementResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPagesByEngagementResponse_pages) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPagesByEngagement_nextToken
              Lens..~ rs
              Lens.^? listPagesByEngagementResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPagesByEngagement where
  type
    AWSResponse ListPagesByEngagement =
      ListPagesByEngagementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPagesByEngagementResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Pages" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPagesByEngagement where
  hashWithSalt _salt ListPagesByEngagement' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` engagementId

instance Prelude.NFData ListPagesByEngagement where
  rnf ListPagesByEngagement' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf engagementId

instance Data.ToHeaders ListPagesByEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListPagesByEngagement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPagesByEngagement where
  toJSON ListPagesByEngagement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("EngagementId" Data..= engagementId)
          ]
      )

instance Data.ToPath ListPagesByEngagement where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPagesByEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPagesByEngagementResponse' smart constructor.
data ListPagesByEngagementResponse = ListPagesByEngagementResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of engagements to contact channels.
    pages :: [Page]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPagesByEngagementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPagesByEngagementResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listPagesByEngagementResponse_httpStatus' - The response's http status code.
--
-- 'pages', 'listPagesByEngagementResponse_pages' - The list of engagements to contact channels.
newListPagesByEngagementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPagesByEngagementResponse
newListPagesByEngagementResponse pHttpStatus_ =
  ListPagesByEngagementResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pages = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listPagesByEngagementResponse_nextToken :: Lens.Lens' ListPagesByEngagementResponse (Prelude.Maybe Prelude.Text)
listPagesByEngagementResponse_nextToken = Lens.lens (\ListPagesByEngagementResponse' {nextToken} -> nextToken) (\s@ListPagesByEngagementResponse' {} a -> s {nextToken = a} :: ListPagesByEngagementResponse)

-- | The response's http status code.
listPagesByEngagementResponse_httpStatus :: Lens.Lens' ListPagesByEngagementResponse Prelude.Int
listPagesByEngagementResponse_httpStatus = Lens.lens (\ListPagesByEngagementResponse' {httpStatus} -> httpStatus) (\s@ListPagesByEngagementResponse' {} a -> s {httpStatus = a} :: ListPagesByEngagementResponse)

-- | The list of engagements to contact channels.
listPagesByEngagementResponse_pages :: Lens.Lens' ListPagesByEngagementResponse [Page]
listPagesByEngagementResponse_pages = Lens.lens (\ListPagesByEngagementResponse' {pages} -> pages) (\s@ListPagesByEngagementResponse' {} a -> s {pages = a} :: ListPagesByEngagementResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPagesByEngagementResponse where
  rnf ListPagesByEngagementResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf pages
