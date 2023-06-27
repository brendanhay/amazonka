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
-- Module      : Amazonka.ManagedBlockChain.ListAccessors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the accessors and their properties. Accessor objects
-- are containers that have the information required for token based access
-- to your Ethereum nodes.
--
-- This operation returns paginated results.
module Amazonka.ManagedBlockChain.ListAccessors
  ( -- * Creating a Request
    ListAccessors (..),
    newListAccessors,

    -- * Request Lenses
    listAccessors_maxResults,
    listAccessors_nextToken,

    -- * Destructuring the Response
    ListAccessorsResponse (..),
    newListAccessorsResponse,

    -- * Response Lenses
    listAccessorsResponse_accessors,
    listAccessorsResponse_nextToken,
    listAccessorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccessors' smart constructor.
data ListAccessors = ListAccessors'
  { -- | The maximum number of accessors to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccessors_maxResults' - The maximum number of accessors to list.
--
-- 'nextToken', 'listAccessors_nextToken' - The pagination token that indicates the next set of results to retrieve.
newListAccessors ::
  ListAccessors
newListAccessors =
  ListAccessors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of accessors to list.
listAccessors_maxResults :: Lens.Lens' ListAccessors (Prelude.Maybe Prelude.Natural)
listAccessors_maxResults = Lens.lens (\ListAccessors' {maxResults} -> maxResults) (\s@ListAccessors' {} a -> s {maxResults = a} :: ListAccessors)

-- | The pagination token that indicates the next set of results to retrieve.
listAccessors_nextToken :: Lens.Lens' ListAccessors (Prelude.Maybe Prelude.Text)
listAccessors_nextToken = Lens.lens (\ListAccessors' {nextToken} -> nextToken) (\s@ListAccessors' {} a -> s {nextToken = a} :: ListAccessors)

instance Core.AWSPager ListAccessors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccessorsResponse_accessors
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAccessors_nextToken
          Lens..~ rs
          Lens.^? listAccessorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAccessors where
  type
    AWSResponse ListAccessors =
      ListAccessorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessorsResponse'
            Prelude.<$> (x Data..?> "Accessors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccessors where
  hashWithSalt _salt ListAccessors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAccessors where
  rnf ListAccessors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAccessors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAccessors where
  toPath = Prelude.const "/accessors"

instance Data.ToQuery ListAccessors where
  toQuery ListAccessors' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAccessorsResponse' smart constructor.
data ListAccessorsResponse = ListAccessorsResponse'
  { -- | An array of AccessorSummary objects that contain configuration
    -- properties for each accessor.
    accessors :: Prelude.Maybe [AccessorSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessors', 'listAccessorsResponse_accessors' - An array of AccessorSummary objects that contain configuration
-- properties for each accessor.
--
-- 'nextToken', 'listAccessorsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listAccessorsResponse_httpStatus' - The response's http status code.
newListAccessorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccessorsResponse
newListAccessorsResponse pHttpStatus_ =
  ListAccessorsResponse'
    { accessors = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of AccessorSummary objects that contain configuration
-- properties for each accessor.
listAccessorsResponse_accessors :: Lens.Lens' ListAccessorsResponse (Prelude.Maybe [AccessorSummary])
listAccessorsResponse_accessors = Lens.lens (\ListAccessorsResponse' {accessors} -> accessors) (\s@ListAccessorsResponse' {} a -> s {accessors = a} :: ListAccessorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
listAccessorsResponse_nextToken :: Lens.Lens' ListAccessorsResponse (Prelude.Maybe Prelude.Text)
listAccessorsResponse_nextToken = Lens.lens (\ListAccessorsResponse' {nextToken} -> nextToken) (\s@ListAccessorsResponse' {} a -> s {nextToken = a} :: ListAccessorsResponse)

-- | The response's http status code.
listAccessorsResponse_httpStatus :: Lens.Lens' ListAccessorsResponse Prelude.Int
listAccessorsResponse_httpStatus = Lens.lens (\ListAccessorsResponse' {httpStatus} -> httpStatus) (\s@ListAccessorsResponse' {} a -> s {httpStatus = a} :: ListAccessorsResponse)

instance Prelude.NFData ListAccessorsResponse where
  rnf ListAccessorsResponse' {..} =
    Prelude.rnf accessors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
