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
-- Module      : Amazonka.Backup.ListLegalHolds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns metadata about active and previous legal holds.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListLegalHolds
  ( -- * Creating a Request
    ListLegalHolds (..),
    newListLegalHolds,

    -- * Request Lenses
    listLegalHolds_maxResults,
    listLegalHolds_nextToken,

    -- * Destructuring the Response
    ListLegalHoldsResponse (..),
    newListLegalHoldsResponse,

    -- * Response Lenses
    listLegalHoldsResponse_legalHolds,
    listLegalHoldsResponse_nextToken,
    listLegalHoldsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLegalHolds' smart constructor.
data ListLegalHolds = ListLegalHolds'
  { -- | The maximum number of resource list items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLegalHolds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLegalHolds_maxResults' - The maximum number of resource list items to be returned.
--
-- 'nextToken', 'listLegalHolds_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
newListLegalHolds ::
  ListLegalHolds
newListLegalHolds =
  ListLegalHolds'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of resource list items to be returned.
listLegalHolds_maxResults :: Lens.Lens' ListLegalHolds (Prelude.Maybe Prelude.Natural)
listLegalHolds_maxResults = Lens.lens (\ListLegalHolds' {maxResults} -> maxResults) (\s@ListLegalHolds' {} a -> s {maxResults = a} :: ListLegalHolds)

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listLegalHolds_nextToken :: Lens.Lens' ListLegalHolds (Prelude.Maybe Prelude.Text)
listLegalHolds_nextToken = Lens.lens (\ListLegalHolds' {nextToken} -> nextToken) (\s@ListLegalHolds' {} a -> s {nextToken = a} :: ListLegalHolds)

instance Core.AWSPager ListLegalHolds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLegalHoldsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLegalHoldsResponse_legalHolds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLegalHolds_nextToken
          Lens..~ rs
          Lens.^? listLegalHoldsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListLegalHolds where
  type
    AWSResponse ListLegalHolds =
      ListLegalHoldsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLegalHoldsResponse'
            Prelude.<$> (x Data..?> "LegalHolds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLegalHolds where
  hashWithSalt _salt ListLegalHolds' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLegalHolds where
  rnf ListLegalHolds' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLegalHolds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLegalHolds where
  toPath = Prelude.const "/legal-holds/"

instance Data.ToQuery ListLegalHolds where
  toQuery ListLegalHolds' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListLegalHoldsResponse' smart constructor.
data ListLegalHoldsResponse = ListLegalHoldsResponse'
  { -- | This is an array of returned legal holds, both active and previous.
    legalHolds :: Prelude.Maybe [LegalHold],
    -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLegalHoldsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'legalHolds', 'listLegalHoldsResponse_legalHolds' - This is an array of returned legal holds, both active and previous.
--
-- 'nextToken', 'listLegalHoldsResponse_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'httpStatus', 'listLegalHoldsResponse_httpStatus' - The response's http status code.
newListLegalHoldsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLegalHoldsResponse
newListLegalHoldsResponse pHttpStatus_ =
  ListLegalHoldsResponse'
    { legalHolds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is an array of returned legal holds, both active and previous.
listLegalHoldsResponse_legalHolds :: Lens.Lens' ListLegalHoldsResponse (Prelude.Maybe [LegalHold])
listLegalHoldsResponse_legalHolds = Lens.lens (\ListLegalHoldsResponse' {legalHolds} -> legalHolds) (\s@ListLegalHoldsResponse' {} a -> s {legalHolds = a} :: ListLegalHoldsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listLegalHoldsResponse_nextToken :: Lens.Lens' ListLegalHoldsResponse (Prelude.Maybe Prelude.Text)
listLegalHoldsResponse_nextToken = Lens.lens (\ListLegalHoldsResponse' {nextToken} -> nextToken) (\s@ListLegalHoldsResponse' {} a -> s {nextToken = a} :: ListLegalHoldsResponse)

-- | The response's http status code.
listLegalHoldsResponse_httpStatus :: Lens.Lens' ListLegalHoldsResponse Prelude.Int
listLegalHoldsResponse_httpStatus = Lens.lens (\ListLegalHoldsResponse' {httpStatus} -> httpStatus) (\s@ListLegalHoldsResponse' {} a -> s {httpStatus = a} :: ListLegalHoldsResponse)

instance Prelude.NFData ListLegalHoldsResponse where
  rnf ListLegalHoldsResponse' {..} =
    Prelude.rnf legalHolds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
