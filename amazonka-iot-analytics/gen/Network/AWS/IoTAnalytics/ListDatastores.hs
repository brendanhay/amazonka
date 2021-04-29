{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.ListDatastores
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data stores.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatastores
  ( -- * Creating a Request
    ListDatastores (..),
    newListDatastores,

    -- * Request Lenses
    listDatastores_nextToken,
    listDatastores_maxResults,

    -- * Destructuring the Response
    ListDatastoresResponse (..),
    newListDatastoresResponse,

    -- * Response Lenses
    listDatastoresResponse_nextToken,
    listDatastoresResponse_datastoreSummaries,
    listDatastoresResponse_httpStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDatastores' smart constructor.
data ListDatastores = ListDatastores'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDatastores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatastores_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listDatastores_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
newListDatastores ::
  ListDatastores
newListDatastores =
  ListDatastores'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
listDatastores_nextToken :: Lens.Lens' ListDatastores (Prelude.Maybe Prelude.Text)
listDatastores_nextToken = Lens.lens (\ListDatastores' {nextToken} -> nextToken) (\s@ListDatastores' {} a -> s {nextToken = a} :: ListDatastores)

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listDatastores_maxResults :: Lens.Lens' ListDatastores (Prelude.Maybe Prelude.Natural)
listDatastores_maxResults = Lens.lens (\ListDatastores' {maxResults} -> maxResults) (\s@ListDatastores' {} a -> s {maxResults = a} :: ListDatastores)

instance Pager.AWSPager ListDatastores where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDatastoresResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDatastoresResponse_datastoreSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDatastores_nextToken
          Lens..~ rs
          Lens.^? listDatastoresResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListDatastores where
  type Rs ListDatastores = ListDatastoresResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatastoresResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "datastoreSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatastores

instance Prelude.NFData ListDatastores

instance Prelude.ToHeaders ListDatastores where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListDatastores where
  toPath = Prelude.const "/datastores"

instance Prelude.ToQuery ListDatastores where
  toQuery ListDatastores' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListDatastoresResponse' smart constructor.
data ListDatastoresResponse = ListDatastoresResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @DatastoreSummary@ objects.
    datastoreSummaries :: Prelude.Maybe [DatastoreSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDatastoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatastoresResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'datastoreSummaries', 'listDatastoresResponse_datastoreSummaries' - A list of @DatastoreSummary@ objects.
--
-- 'httpStatus', 'listDatastoresResponse_httpStatus' - The response's http status code.
newListDatastoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatastoresResponse
newListDatastoresResponse pHttpStatus_ =
  ListDatastoresResponse'
    { nextToken =
        Prelude.Nothing,
      datastoreSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatastoresResponse_nextToken :: Lens.Lens' ListDatastoresResponse (Prelude.Maybe Prelude.Text)
listDatastoresResponse_nextToken = Lens.lens (\ListDatastoresResponse' {nextToken} -> nextToken) (\s@ListDatastoresResponse' {} a -> s {nextToken = a} :: ListDatastoresResponse)

-- | A list of @DatastoreSummary@ objects.
listDatastoresResponse_datastoreSummaries :: Lens.Lens' ListDatastoresResponse (Prelude.Maybe [DatastoreSummary])
listDatastoresResponse_datastoreSummaries = Lens.lens (\ListDatastoresResponse' {datastoreSummaries} -> datastoreSummaries) (\s@ListDatastoresResponse' {} a -> s {datastoreSummaries = a} :: ListDatastoresResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDatastoresResponse_httpStatus :: Lens.Lens' ListDatastoresResponse Prelude.Int
listDatastoresResponse_httpStatus = Lens.lens (\ListDatastoresResponse' {httpStatus} -> httpStatus) (\s@ListDatastoresResponse' {} a -> s {httpStatus = a} :: ListDatastoresResponse)

instance Prelude.NFData ListDatastoresResponse
