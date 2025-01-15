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
-- Module      : Amazonka.MacieV2.ListClassificationScopes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about the classification scope for an
-- account.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListClassificationScopes
  ( -- * Creating a Request
    ListClassificationScopes (..),
    newListClassificationScopes,

    -- * Request Lenses
    listClassificationScopes_name,
    listClassificationScopes_nextToken,

    -- * Destructuring the Response
    ListClassificationScopesResponse (..),
    newListClassificationScopesResponse,

    -- * Response Lenses
    listClassificationScopesResponse_classificationScopes,
    listClassificationScopesResponse_nextToken,
    listClassificationScopesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClassificationScopes' smart constructor.
data ListClassificationScopes = ListClassificationScopes'
  { -- | The name of the classification scope to retrieve the unique identifier
    -- for.
    name :: Prelude.Maybe Prelude.Text,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClassificationScopes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listClassificationScopes_name' - The name of the classification scope to retrieve the unique identifier
-- for.
--
-- 'nextToken', 'listClassificationScopes_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
newListClassificationScopes ::
  ListClassificationScopes
newListClassificationScopes =
  ListClassificationScopes'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the classification scope to retrieve the unique identifier
-- for.
listClassificationScopes_name :: Lens.Lens' ListClassificationScopes (Prelude.Maybe Prelude.Text)
listClassificationScopes_name = Lens.lens (\ListClassificationScopes' {name} -> name) (\s@ListClassificationScopes' {} a -> s {name = a} :: ListClassificationScopes)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listClassificationScopes_nextToken :: Lens.Lens' ListClassificationScopes (Prelude.Maybe Prelude.Text)
listClassificationScopes_nextToken = Lens.lens (\ListClassificationScopes' {nextToken} -> nextToken) (\s@ListClassificationScopes' {} a -> s {nextToken = a} :: ListClassificationScopes)

instance Core.AWSPager ListClassificationScopes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClassificationScopesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClassificationScopesResponse_classificationScopes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listClassificationScopes_nextToken
              Lens..~ rs
              Lens.^? listClassificationScopesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListClassificationScopes where
  type
    AWSResponse ListClassificationScopes =
      ListClassificationScopesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClassificationScopesResponse'
            Prelude.<$> ( x
                            Data..?> "classificationScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClassificationScopes where
  hashWithSalt _salt ListClassificationScopes' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListClassificationScopes where
  rnf ListClassificationScopes' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListClassificationScopes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClassificationScopes where
  toPath = Prelude.const "/classification-scopes"

instance Data.ToQuery ListClassificationScopes where
  toQuery ListClassificationScopes' {..} =
    Prelude.mconcat
      ["name" Data.=: name, "nextToken" Data.=: nextToken]

-- | /See:/ 'newListClassificationScopesResponse' smart constructor.
data ListClassificationScopesResponse = ListClassificationScopesResponse'
  { -- | An array that specifies the unique identifier and name of the
    -- classification scope for the account.
    classificationScopes :: Prelude.Maybe [ClassificationScopeSummary],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClassificationScopesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classificationScopes', 'listClassificationScopesResponse_classificationScopes' - An array that specifies the unique identifier and name of the
-- classification scope for the account.
--
-- 'nextToken', 'listClassificationScopesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listClassificationScopesResponse_httpStatus' - The response's http status code.
newListClassificationScopesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClassificationScopesResponse
newListClassificationScopesResponse pHttpStatus_ =
  ListClassificationScopesResponse'
    { classificationScopes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that specifies the unique identifier and name of the
-- classification scope for the account.
listClassificationScopesResponse_classificationScopes :: Lens.Lens' ListClassificationScopesResponse (Prelude.Maybe [ClassificationScopeSummary])
listClassificationScopesResponse_classificationScopes = Lens.lens (\ListClassificationScopesResponse' {classificationScopes} -> classificationScopes) (\s@ListClassificationScopesResponse' {} a -> s {classificationScopes = a} :: ListClassificationScopesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listClassificationScopesResponse_nextToken :: Lens.Lens' ListClassificationScopesResponse (Prelude.Maybe Prelude.Text)
listClassificationScopesResponse_nextToken = Lens.lens (\ListClassificationScopesResponse' {nextToken} -> nextToken) (\s@ListClassificationScopesResponse' {} a -> s {nextToken = a} :: ListClassificationScopesResponse)

-- | The response's http status code.
listClassificationScopesResponse_httpStatus :: Lens.Lens' ListClassificationScopesResponse Prelude.Int
listClassificationScopesResponse_httpStatus = Lens.lens (\ListClassificationScopesResponse' {httpStatus} -> httpStatus) (\s@ListClassificationScopesResponse' {} a -> s {httpStatus = a} :: ListClassificationScopesResponse)

instance
  Prelude.NFData
    ListClassificationScopesResponse
  where
  rnf ListClassificationScopesResponse' {..} =
    Prelude.rnf classificationScopes `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
