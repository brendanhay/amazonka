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
-- Module      : Amazonka.Route53RecoveryReadiness.ListResourceSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource sets in an account.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListResourceSets
  ( -- * Creating a Request
    ListResourceSets (..),
    newListResourceSets,

    -- * Request Lenses
    listResourceSets_nextToken,
    listResourceSets_maxResults,

    -- * Destructuring the Response
    ListResourceSetsResponse (..),
    newListResourceSetsResponse,

    -- * Response Lenses
    listResourceSetsResponse_nextToken,
    listResourceSetsResponse_resourceSets,
    listResourceSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListResourceSets' smart constructor.
data ListResourceSets = ListResourceSets'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSets_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'maxResults', 'listResourceSets_maxResults' - The number of objects that you want to return with this call.
newListResourceSets ::
  ListResourceSets
newListResourceSets =
  ListResourceSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that identifies which batch of results you want to see.
listResourceSets_nextToken :: Lens.Lens' ListResourceSets (Prelude.Maybe Prelude.Text)
listResourceSets_nextToken = Lens.lens (\ListResourceSets' {nextToken} -> nextToken) (\s@ListResourceSets' {} a -> s {nextToken = a} :: ListResourceSets)

-- | The number of objects that you want to return with this call.
listResourceSets_maxResults :: Lens.Lens' ListResourceSets (Prelude.Maybe Prelude.Natural)
listResourceSets_maxResults = Lens.lens (\ListResourceSets' {maxResults} -> maxResults) (\s@ListResourceSets' {} a -> s {maxResults = a} :: ListResourceSets)

instance Core.AWSPager ListResourceSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceSetsResponse_resourceSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceSets_nextToken
          Lens..~ rs
          Lens.^? listResourceSetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceSets where
  type
    AWSResponse ListResourceSets =
      ListResourceSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceSetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resourceSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceSets where
  hashWithSalt _salt ListResourceSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListResourceSets where
  rnf ListResourceSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListResourceSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListResourceSets where
  toPath = Prelude.const "/resourcesets"

instance Data.ToQuery ListResourceSets where
  toQuery ListResourceSets' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListResourceSetsResponse' smart constructor.
data ListResourceSetsResponse = ListResourceSetsResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of resource sets associated with the account.
    resourceSets :: Prelude.Maybe [ResourceSetOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSetsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'resourceSets', 'listResourceSetsResponse_resourceSets' - A list of resource sets associated with the account.
--
-- 'httpStatus', 'listResourceSetsResponse_httpStatus' - The response's http status code.
newListResourceSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceSetsResponse
newListResourceSetsResponse pHttpStatus_ =
  ListResourceSetsResponse'
    { nextToken =
        Prelude.Nothing,
      resourceSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
listResourceSetsResponse_nextToken :: Lens.Lens' ListResourceSetsResponse (Prelude.Maybe Prelude.Text)
listResourceSetsResponse_nextToken = Lens.lens (\ListResourceSetsResponse' {nextToken} -> nextToken) (\s@ListResourceSetsResponse' {} a -> s {nextToken = a} :: ListResourceSetsResponse)

-- | A list of resource sets associated with the account.
listResourceSetsResponse_resourceSets :: Lens.Lens' ListResourceSetsResponse (Prelude.Maybe [ResourceSetOutput])
listResourceSetsResponse_resourceSets = Lens.lens (\ListResourceSetsResponse' {resourceSets} -> resourceSets) (\s@ListResourceSetsResponse' {} a -> s {resourceSets = a} :: ListResourceSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceSetsResponse_httpStatus :: Lens.Lens' ListResourceSetsResponse Prelude.Int
listResourceSetsResponse_httpStatus = Lens.lens (\ListResourceSetsResponse' {httpStatus} -> httpStatus) (\s@ListResourceSetsResponse' {} a -> s {httpStatus = a} :: ListResourceSetsResponse)

instance Prelude.NFData ListResourceSetsResponse where
  rnf ListResourceSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceSets
      `Prelude.seq` Prelude.rnf httpStatus
