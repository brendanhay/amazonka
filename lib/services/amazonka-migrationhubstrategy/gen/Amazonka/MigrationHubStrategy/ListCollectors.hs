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
-- Module      : Amazonka.MigrationHubStrategy.ListCollectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all the installed collectors.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubStrategy.ListCollectors
  ( -- * Creating a Request
    ListCollectors (..),
    newListCollectors,

    -- * Request Lenses
    listCollectors_nextToken,
    listCollectors_maxResults,

    -- * Destructuring the Response
    ListCollectorsResponse (..),
    newListCollectorsResponse,

    -- * Response Lenses
    listCollectorsResponse_nextToken,
    listCollectorsResponse_collectors,
    listCollectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCollectors' smart constructor.
data ListCollectors = ListCollectors'
  { -- | The token from a previous call that you use to retrieve the next set of
    -- results. For example, if a previous call to this action returned 100
    -- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
    -- results along with a token. You then use the returned token to retrieve
    -- the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in the response. The maximum
    -- value is 100.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCollectors_nextToken' - The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
--
-- 'maxResults', 'listCollectors_maxResults' - The maximum number of items to include in the response. The maximum
-- value is 100.
newListCollectors ::
  ListCollectors
newListCollectors =
  ListCollectors'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
listCollectors_nextToken :: Lens.Lens' ListCollectors (Prelude.Maybe Prelude.Text)
listCollectors_nextToken = Lens.lens (\ListCollectors' {nextToken} -> nextToken) (\s@ListCollectors' {} a -> s {nextToken = a} :: ListCollectors)

-- | The maximum number of items to include in the response. The maximum
-- value is 100.
listCollectors_maxResults :: Lens.Lens' ListCollectors (Prelude.Maybe Prelude.Int)
listCollectors_maxResults = Lens.lens (\ListCollectors' {maxResults} -> maxResults) (\s@ListCollectors' {} a -> s {maxResults = a} :: ListCollectors)

instance Core.AWSPager ListCollectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCollectorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCollectorsResponse_collectors
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCollectors_nextToken
          Lens..~ rs
          Lens.^? listCollectorsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListCollectors where
  type
    AWSResponse ListCollectors =
      ListCollectorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectorsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "Collectors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCollectors where
  hashWithSalt _salt ListCollectors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCollectors where
  rnf ListCollectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCollectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListCollectors where
  toPath = Prelude.const "/list-collectors"

instance Core.ToQuery ListCollectors where
  toQuery ListCollectors' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCollectorsResponse' smart constructor.
data ListCollectorsResponse = ListCollectorsResponse'
  { -- | The token you use to retrieve the next set of results, or null if there
    -- are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of all the installed collectors.
    collectors :: Prelude.Maybe [Collector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCollectorsResponse_nextToken' - The token you use to retrieve the next set of results, or null if there
-- are no more results.
--
-- 'collectors', 'listCollectorsResponse_collectors' - The list of all the installed collectors.
--
-- 'httpStatus', 'listCollectorsResponse_httpStatus' - The response's http status code.
newListCollectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCollectorsResponse
newListCollectorsResponse pHttpStatus_ =
  ListCollectorsResponse'
    { nextToken =
        Prelude.Nothing,
      collectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token you use to retrieve the next set of results, or null if there
-- are no more results.
listCollectorsResponse_nextToken :: Lens.Lens' ListCollectorsResponse (Prelude.Maybe Prelude.Text)
listCollectorsResponse_nextToken = Lens.lens (\ListCollectorsResponse' {nextToken} -> nextToken) (\s@ListCollectorsResponse' {} a -> s {nextToken = a} :: ListCollectorsResponse)

-- | The list of all the installed collectors.
listCollectorsResponse_collectors :: Lens.Lens' ListCollectorsResponse (Prelude.Maybe [Collector])
listCollectorsResponse_collectors = Lens.lens (\ListCollectorsResponse' {collectors} -> collectors) (\s@ListCollectorsResponse' {} a -> s {collectors = a} :: ListCollectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCollectorsResponse_httpStatus :: Lens.Lens' ListCollectorsResponse Prelude.Int
listCollectorsResponse_httpStatus = Lens.lens (\ListCollectorsResponse' {httpStatus} -> httpStatus) (\s@ListCollectorsResponse' {} a -> s {httpStatus = a} :: ListCollectorsResponse)

instance Prelude.NFData ListCollectorsResponse where
  rnf ListCollectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf collectors
      `Prelude.seq` Prelude.rnf httpStatus
