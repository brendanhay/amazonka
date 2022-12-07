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
-- Module      : Amazonka.AppConfig.ListDeploymentStrategies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists deployment strategies.
module Amazonka.AppConfig.ListDeploymentStrategies
  ( -- * Creating a Request
    ListDeploymentStrategies (..),
    newListDeploymentStrategies,

    -- * Request Lenses
    listDeploymentStrategies_nextToken,
    listDeploymentStrategies_maxResults,

    -- * Destructuring the Response
    ListDeploymentStrategiesResponse (..),
    newListDeploymentStrategiesResponse,

    -- * Response Lenses
    listDeploymentStrategiesResponse_items,
    listDeploymentStrategiesResponse_nextToken,
    listDeploymentStrategiesResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeploymentStrategies' smart constructor.
data ListDeploymentStrategies = ListDeploymentStrategies'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentStrategies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentStrategies_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listDeploymentStrategies_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newListDeploymentStrategies ::
  ListDeploymentStrategies
newListDeploymentStrategies =
  ListDeploymentStrategies'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listDeploymentStrategies_nextToken :: Lens.Lens' ListDeploymentStrategies (Prelude.Maybe Prelude.Text)
listDeploymentStrategies_nextToken = Lens.lens (\ListDeploymentStrategies' {nextToken} -> nextToken) (\s@ListDeploymentStrategies' {} a -> s {nextToken = a} :: ListDeploymentStrategies)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDeploymentStrategies_maxResults :: Lens.Lens' ListDeploymentStrategies (Prelude.Maybe Prelude.Natural)
listDeploymentStrategies_maxResults = Lens.lens (\ListDeploymentStrategies' {maxResults} -> maxResults) (\s@ListDeploymentStrategies' {} a -> s {maxResults = a} :: ListDeploymentStrategies)

instance Core.AWSRequest ListDeploymentStrategies where
  type
    AWSResponse ListDeploymentStrategies =
      ListDeploymentStrategiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentStrategiesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentStrategies where
  hashWithSalt _salt ListDeploymentStrategies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDeploymentStrategies where
  rnf ListDeploymentStrategies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDeploymentStrategies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeploymentStrategies where
  toPath = Prelude.const "/deploymentstrategies"

instance Data.ToQuery ListDeploymentStrategies where
  toQuery ListDeploymentStrategies' {..} =
    Prelude.mconcat
      [ "next_token" Data.=: nextToken,
        "max_results" Data.=: maxResults
      ]

-- | /See:/ 'newListDeploymentStrategiesResponse' smart constructor.
data ListDeploymentStrategiesResponse = ListDeploymentStrategiesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [DeploymentStrategy],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentStrategiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listDeploymentStrategiesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'listDeploymentStrategiesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listDeploymentStrategiesResponse_httpStatus' - The response's http status code.
newListDeploymentStrategiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentStrategiesResponse
newListDeploymentStrategiesResponse pHttpStatus_ =
  ListDeploymentStrategiesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
listDeploymentStrategiesResponse_items :: Lens.Lens' ListDeploymentStrategiesResponse (Prelude.Maybe [DeploymentStrategy])
listDeploymentStrategiesResponse_items = Lens.lens (\ListDeploymentStrategiesResponse' {items} -> items) (\s@ListDeploymentStrategiesResponse' {} a -> s {items = a} :: ListDeploymentStrategiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listDeploymentStrategiesResponse_nextToken :: Lens.Lens' ListDeploymentStrategiesResponse (Prelude.Maybe Prelude.Text)
listDeploymentStrategiesResponse_nextToken = Lens.lens (\ListDeploymentStrategiesResponse' {nextToken} -> nextToken) (\s@ListDeploymentStrategiesResponse' {} a -> s {nextToken = a} :: ListDeploymentStrategiesResponse)

-- | The response's http status code.
listDeploymentStrategiesResponse_httpStatus :: Lens.Lens' ListDeploymentStrategiesResponse Prelude.Int
listDeploymentStrategiesResponse_httpStatus = Lens.lens (\ListDeploymentStrategiesResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentStrategiesResponse' {} a -> s {httpStatus = a} :: ListDeploymentStrategiesResponse)

instance
  Prelude.NFData
    ListDeploymentStrategiesResponse
  where
  rnf ListDeploymentStrategiesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
