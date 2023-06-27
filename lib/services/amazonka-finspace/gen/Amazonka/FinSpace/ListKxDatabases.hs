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
-- Module      : Amazonka.FinSpace.ListKxDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the databases in the kdb environment.
module Amazonka.FinSpace.ListKxDatabases
  ( -- * Creating a Request
    ListKxDatabases (..),
    newListKxDatabases,

    -- * Request Lenses
    listKxDatabases_maxResults,
    listKxDatabases_nextToken,
    listKxDatabases_environmentId,

    -- * Destructuring the Response
    ListKxDatabasesResponse (..),
    newListKxDatabasesResponse,

    -- * Response Lenses
    listKxDatabasesResponse_kxDatabases,
    listKxDatabasesResponse_nextToken,
    listKxDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxDatabases' smart constructor.
data ListKxDatabases = ListKxDatabases'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKxDatabases_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxDatabases_nextToken' - A token that indicates where a results page should begin.
--
-- 'environmentId', 'listKxDatabases_environmentId' - A unique identifier for the kdb environment.
newListKxDatabases ::
  -- | 'environmentId'
  Prelude.Text ->
  ListKxDatabases
newListKxDatabases pEnvironmentId_ =
  ListKxDatabases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | The maximum number of results to return in this request.
listKxDatabases_maxResults :: Lens.Lens' ListKxDatabases (Prelude.Maybe Prelude.Natural)
listKxDatabases_maxResults = Lens.lens (\ListKxDatabases' {maxResults} -> maxResults) (\s@ListKxDatabases' {} a -> s {maxResults = a} :: ListKxDatabases)

-- | A token that indicates where a results page should begin.
listKxDatabases_nextToken :: Lens.Lens' ListKxDatabases (Prelude.Maybe Prelude.Text)
listKxDatabases_nextToken = Lens.lens (\ListKxDatabases' {nextToken} -> nextToken) (\s@ListKxDatabases' {} a -> s {nextToken = a} :: ListKxDatabases)

-- | A unique identifier for the kdb environment.
listKxDatabases_environmentId :: Lens.Lens' ListKxDatabases Prelude.Text
listKxDatabases_environmentId = Lens.lens (\ListKxDatabases' {environmentId} -> environmentId) (\s@ListKxDatabases' {} a -> s {environmentId = a} :: ListKxDatabases)

instance Core.AWSRequest ListKxDatabases where
  type
    AWSResponse ListKxDatabases =
      ListKxDatabasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxDatabasesResponse'
            Prelude.<$> (x Data..?> "kxDatabases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxDatabases where
  hashWithSalt _salt ListKxDatabases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ListKxDatabases where
  rnf ListKxDatabases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders ListKxDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxDatabases where
  toPath ListKxDatabases' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases"
      ]

instance Data.ToQuery ListKxDatabases where
  toQuery ListKxDatabases' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxDatabasesResponse' smart constructor.
data ListKxDatabasesResponse = ListKxDatabasesResponse'
  { -- | A list of databases in the kdb environment.
    kxDatabases :: Prelude.Maybe [KxDatabaseListEntry],
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kxDatabases', 'listKxDatabasesResponse_kxDatabases' - A list of databases in the kdb environment.
--
-- 'nextToken', 'listKxDatabasesResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'httpStatus', 'listKxDatabasesResponse_httpStatus' - The response's http status code.
newListKxDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxDatabasesResponse
newListKxDatabasesResponse pHttpStatus_ =
  ListKxDatabasesResponse'
    { kxDatabases =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of databases in the kdb environment.
listKxDatabasesResponse_kxDatabases :: Lens.Lens' ListKxDatabasesResponse (Prelude.Maybe [KxDatabaseListEntry])
listKxDatabasesResponse_kxDatabases = Lens.lens (\ListKxDatabasesResponse' {kxDatabases} -> kxDatabases) (\s@ListKxDatabasesResponse' {} a -> s {kxDatabases = a} :: ListKxDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where a results page should begin.
listKxDatabasesResponse_nextToken :: Lens.Lens' ListKxDatabasesResponse (Prelude.Maybe Prelude.Text)
listKxDatabasesResponse_nextToken = Lens.lens (\ListKxDatabasesResponse' {nextToken} -> nextToken) (\s@ListKxDatabasesResponse' {} a -> s {nextToken = a} :: ListKxDatabasesResponse)

-- | The response's http status code.
listKxDatabasesResponse_httpStatus :: Lens.Lens' ListKxDatabasesResponse Prelude.Int
listKxDatabasesResponse_httpStatus = Lens.lens (\ListKxDatabasesResponse' {httpStatus} -> httpStatus) (\s@ListKxDatabasesResponse' {} a -> s {httpStatus = a} :: ListKxDatabasesResponse)

instance Prelude.NFData ListKxDatabasesResponse where
  rnf ListKxDatabasesResponse' {..} =
    Prelude.rnf kxDatabases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
