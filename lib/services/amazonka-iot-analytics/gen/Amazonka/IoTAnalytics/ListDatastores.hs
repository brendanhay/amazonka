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
-- Module      : Amazonka.IoTAnalytics.ListDatastores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data stores.
--
-- This operation returns paginated results.
module Amazonka.IoTAnalytics.ListDatastores
  ( -- * Creating a Request
    ListDatastores (..),
    newListDatastores,

    -- * Request Lenses
    listDatastores_maxResults,
    listDatastores_nextToken,

    -- * Destructuring the Response
    ListDatastoresResponse (..),
    newListDatastoresResponse,

    -- * Response Lenses
    listDatastoresResponse_datastoreSummaries,
    listDatastoresResponse_nextToken,
    listDatastoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatastores' smart constructor.
data ListDatastores = ListDatastores'
  { -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatastores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDatastores_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- 'nextToken', 'listDatastores_nextToken' - The token for the next set of results.
newListDatastores ::
  ListDatastores
newListDatastores =
  ListDatastores'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listDatastores_maxResults :: Lens.Lens' ListDatastores (Prelude.Maybe Prelude.Natural)
listDatastores_maxResults = Lens.lens (\ListDatastores' {maxResults} -> maxResults) (\s@ListDatastores' {} a -> s {maxResults = a} :: ListDatastores)

-- | The token for the next set of results.
listDatastores_nextToken :: Lens.Lens' ListDatastores (Prelude.Maybe Prelude.Text)
listDatastores_nextToken = Lens.lens (\ListDatastores' {nextToken} -> nextToken) (\s@ListDatastores' {} a -> s {nextToken = a} :: ListDatastores)

instance Core.AWSPager ListDatastores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatastoresResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatastoresResponse_datastoreSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatastores_nextToken
          Lens..~ rs
          Lens.^? listDatastoresResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDatastores where
  type
    AWSResponse ListDatastores =
      ListDatastoresResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatastoresResponse'
            Prelude.<$> ( x Data..?> "datastoreSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatastores where
  hashWithSalt _salt ListDatastores' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatastores where
  rnf ListDatastores' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDatastores where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDatastores where
  toPath = Prelude.const "/datastores"

instance Data.ToQuery ListDatastores where
  toQuery ListDatastores' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDatastoresResponse' smart constructor.
data ListDatastoresResponse = ListDatastoresResponse'
  { -- | A list of @DatastoreSummary@ objects.
    datastoreSummaries :: Prelude.Maybe [DatastoreSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatastoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreSummaries', 'listDatastoresResponse_datastoreSummaries' - A list of @DatastoreSummary@ objects.
--
-- 'nextToken', 'listDatastoresResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'httpStatus', 'listDatastoresResponse_httpStatus' - The response's http status code.
newListDatastoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatastoresResponse
newListDatastoresResponse pHttpStatus_ =
  ListDatastoresResponse'
    { datastoreSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DatastoreSummary@ objects.
listDatastoresResponse_datastoreSummaries :: Lens.Lens' ListDatastoresResponse (Prelude.Maybe [DatastoreSummary])
listDatastoresResponse_datastoreSummaries = Lens.lens (\ListDatastoresResponse' {datastoreSummaries} -> datastoreSummaries) (\s@ListDatastoresResponse' {} a -> s {datastoreSummaries = a} :: ListDatastoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatastoresResponse_nextToken :: Lens.Lens' ListDatastoresResponse (Prelude.Maybe Prelude.Text)
listDatastoresResponse_nextToken = Lens.lens (\ListDatastoresResponse' {nextToken} -> nextToken) (\s@ListDatastoresResponse' {} a -> s {nextToken = a} :: ListDatastoresResponse)

-- | The response's http status code.
listDatastoresResponse_httpStatus :: Lens.Lens' ListDatastoresResponse Prelude.Int
listDatastoresResponse_httpStatus = Lens.lens (\ListDatastoresResponse' {httpStatus} -> httpStatus) (\s@ListDatastoresResponse' {} a -> s {httpStatus = a} :: ListDatastoresResponse)

instance Prelude.NFData ListDatastoresResponse where
  rnf ListDatastoresResponse' {..} =
    Prelude.rnf datastoreSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
