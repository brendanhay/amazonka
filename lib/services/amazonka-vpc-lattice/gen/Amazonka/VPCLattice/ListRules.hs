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
-- Module      : Amazonka.VPCLattice.ListRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the listener.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_maxResults,
    listRules_nextToken,
    listRules_listenerIdentifier,
    listRules_serviceIdentifier,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_httpStatus,
    listRulesResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRules_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listRules_nextToken' - A pagination token for the next page of results.
--
-- 'listenerIdentifier', 'listRules_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'serviceIdentifier', 'listRules_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newListRules ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  ListRules
newListRules pListenerIdentifier_ pServiceIdentifier_ =
  ListRules'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      listenerIdentifier = pListenerIdentifier_,
      serviceIdentifier = pServiceIdentifier_
    }

-- | The maximum number of results to return.
listRules_maxResults :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_maxResults = Lens.lens (\ListRules' {maxResults} -> maxResults) (\s@ListRules' {} a -> s {maxResults = a} :: ListRules)

-- | A pagination token for the next page of results.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | The ID or Amazon Resource Name (ARN) of the listener.
listRules_listenerIdentifier :: Lens.Lens' ListRules Prelude.Text
listRules_listenerIdentifier = Lens.lens (\ListRules' {listenerIdentifier} -> listenerIdentifier) (\s@ListRules' {} a -> s {listenerIdentifier = a} :: ListRules)

-- | The ID or Amazon Resource Name (ARN) of the service.
listRules_serviceIdentifier :: Lens.Lens' ListRules Prelude.Text
listRules_serviceIdentifier = Lens.lens (\ListRules' {serviceIdentifier} -> serviceIdentifier) (\s@ListRules' {} a -> s {serviceIdentifier = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listRulesResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRules_nextToken
          Lens..~ rs
          Lens.^? listRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRules where
  toPath ListRules' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules"
      ]

instance Data.ToQuery ListRules where
  toQuery ListRules' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the rules.
    items :: [RuleSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listRulesResponse_items' - Information about the rules.
newListRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | If there are additional results, a pagination token for the next page of
-- results.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

-- | Information about the rules.
listRulesResponse_items :: Lens.Lens' ListRulesResponse [RuleSummary]
listRulesResponse_items = Lens.lens (\ListRulesResponse' {items} -> items) (\s@ListRulesResponse' {} a -> s {items = a} :: ListRulesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
