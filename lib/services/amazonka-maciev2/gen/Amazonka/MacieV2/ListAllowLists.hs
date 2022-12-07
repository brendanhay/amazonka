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
-- Module      : Amazonka.MacieV2.ListAllowLists
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about all the allow lists for an
-- account.
module Amazonka.MacieV2.ListAllowLists
  ( -- * Creating a Request
    ListAllowLists (..),
    newListAllowLists,

    -- * Request Lenses
    listAllowLists_nextToken,
    listAllowLists_maxResults,

    -- * Destructuring the Response
    ListAllowListsResponse (..),
    newListAllowListsResponse,

    -- * Response Lenses
    listAllowListsResponse_nextToken,
    listAllowListsResponse_allowLists,
    listAllowListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAllowLists' smart constructor.
data ListAllowLists = ListAllowLists'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAllowLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAllowLists_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'maxResults', 'listAllowLists_maxResults' - The maximum number of items to include in each page of a paginated
-- response.
newListAllowLists ::
  ListAllowLists
newListAllowLists =
  ListAllowLists'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listAllowLists_nextToken :: Lens.Lens' ListAllowLists (Prelude.Maybe Prelude.Text)
listAllowLists_nextToken = Lens.lens (\ListAllowLists' {nextToken} -> nextToken) (\s@ListAllowLists' {} a -> s {nextToken = a} :: ListAllowLists)

-- | The maximum number of items to include in each page of a paginated
-- response.
listAllowLists_maxResults :: Lens.Lens' ListAllowLists (Prelude.Maybe Prelude.Natural)
listAllowLists_maxResults = Lens.lens (\ListAllowLists' {maxResults} -> maxResults) (\s@ListAllowLists' {} a -> s {maxResults = a} :: ListAllowLists)

instance Core.AWSRequest ListAllowLists where
  type
    AWSResponse ListAllowLists =
      ListAllowListsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAllowListsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "allowLists" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAllowLists where
  hashWithSalt _salt ListAllowLists' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAllowLists where
  rnf ListAllowLists' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListAllowLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAllowLists where
  toPath = Prelude.const "/allow-lists"

instance Data.ToQuery ListAllowLists where
  toQuery ListAllowLists' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListAllowListsResponse' smart constructor.
data ListAllowListsResponse = ListAllowListsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects, one for each allow list.
    allowLists :: Prelude.Maybe [AllowListSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAllowListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAllowListsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'allowLists', 'listAllowListsResponse_allowLists' - An array of objects, one for each allow list.
--
-- 'httpStatus', 'listAllowListsResponse_httpStatus' - The response's http status code.
newListAllowListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAllowListsResponse
newListAllowListsResponse pHttpStatus_ =
  ListAllowListsResponse'
    { nextToken =
        Prelude.Nothing,
      allowLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listAllowListsResponse_nextToken :: Lens.Lens' ListAllowListsResponse (Prelude.Maybe Prelude.Text)
listAllowListsResponse_nextToken = Lens.lens (\ListAllowListsResponse' {nextToken} -> nextToken) (\s@ListAllowListsResponse' {} a -> s {nextToken = a} :: ListAllowListsResponse)

-- | An array of objects, one for each allow list.
listAllowListsResponse_allowLists :: Lens.Lens' ListAllowListsResponse (Prelude.Maybe [AllowListSummary])
listAllowListsResponse_allowLists = Lens.lens (\ListAllowListsResponse' {allowLists} -> allowLists) (\s@ListAllowListsResponse' {} a -> s {allowLists = a} :: ListAllowListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAllowListsResponse_httpStatus :: Lens.Lens' ListAllowListsResponse Prelude.Int
listAllowListsResponse_httpStatus = Lens.lens (\ListAllowListsResponse' {httpStatus} -> httpStatus) (\s@ListAllowListsResponse' {} a -> s {httpStatus = a} :: ListAllowListsResponse)

instance Prelude.NFData ListAllowListsResponse where
  rnf ListAllowListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf allowLists
      `Prelude.seq` Prelude.rnf httpStatus
