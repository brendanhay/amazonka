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
-- Module      : Amazonka.FinSpace.ListKxChangesets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the changesets for a database.
module Amazonka.FinSpace.ListKxChangesets
  ( -- * Creating a Request
    ListKxChangesets (..),
    newListKxChangesets,

    -- * Request Lenses
    listKxChangesets_maxResults,
    listKxChangesets_nextToken,
    listKxChangesets_environmentId,
    listKxChangesets_databaseName,

    -- * Destructuring the Response
    ListKxChangesetsResponse (..),
    newListKxChangesetsResponse,

    -- * Response Lenses
    listKxChangesetsResponse_kxChangesets,
    listKxChangesetsResponse_nextToken,
    listKxChangesetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxChangesets' smart constructor.
data ListKxChangesets = ListKxChangesets'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxChangesets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKxChangesets_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxChangesets_nextToken' - A token that indicates where a results page should begin.
--
-- 'environmentId', 'listKxChangesets_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'listKxChangesets_databaseName' - The name of the kdb database.
newListKxChangesets ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  ListKxChangesets
newListKxChangesets pEnvironmentId_ pDatabaseName_ =
  ListKxChangesets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      environmentId = pEnvironmentId_,
      databaseName = pDatabaseName_
    }

-- | The maximum number of results to return in this request.
listKxChangesets_maxResults :: Lens.Lens' ListKxChangesets (Prelude.Maybe Prelude.Natural)
listKxChangesets_maxResults = Lens.lens (\ListKxChangesets' {maxResults} -> maxResults) (\s@ListKxChangesets' {} a -> s {maxResults = a} :: ListKxChangesets)

-- | A token that indicates where a results page should begin.
listKxChangesets_nextToken :: Lens.Lens' ListKxChangesets (Prelude.Maybe Prelude.Text)
listKxChangesets_nextToken = Lens.lens (\ListKxChangesets' {nextToken} -> nextToken) (\s@ListKxChangesets' {} a -> s {nextToken = a} :: ListKxChangesets)

-- | A unique identifier for the kdb environment.
listKxChangesets_environmentId :: Lens.Lens' ListKxChangesets Prelude.Text
listKxChangesets_environmentId = Lens.lens (\ListKxChangesets' {environmentId} -> environmentId) (\s@ListKxChangesets' {} a -> s {environmentId = a} :: ListKxChangesets)

-- | The name of the kdb database.
listKxChangesets_databaseName :: Lens.Lens' ListKxChangesets Prelude.Text
listKxChangesets_databaseName = Lens.lens (\ListKxChangesets' {databaseName} -> databaseName) (\s@ListKxChangesets' {} a -> s {databaseName = a} :: ListKxChangesets)

instance Core.AWSRequest ListKxChangesets where
  type
    AWSResponse ListKxChangesets =
      ListKxChangesetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxChangesetsResponse'
            Prelude.<$> (x Data..?> "kxChangesets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxChangesets where
  hashWithSalt _salt ListKxChangesets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData ListKxChangesets where
  rnf ListKxChangesets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToHeaders ListKxChangesets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxChangesets where
  toPath ListKxChangesets' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName,
        "/changesets"
      ]

instance Data.ToQuery ListKxChangesets where
  toQuery ListKxChangesets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxChangesetsResponse' smart constructor.
data ListKxChangesetsResponse = ListKxChangesetsResponse'
  { -- | A list of changesets for a database.
    kxChangesets :: Prelude.Maybe [KxChangesetListEntry],
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxChangesetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kxChangesets', 'listKxChangesetsResponse_kxChangesets' - A list of changesets for a database.
--
-- 'nextToken', 'listKxChangesetsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'httpStatus', 'listKxChangesetsResponse_httpStatus' - The response's http status code.
newListKxChangesetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxChangesetsResponse
newListKxChangesetsResponse pHttpStatus_ =
  ListKxChangesetsResponse'
    { kxChangesets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of changesets for a database.
listKxChangesetsResponse_kxChangesets :: Lens.Lens' ListKxChangesetsResponse (Prelude.Maybe [KxChangesetListEntry])
listKxChangesetsResponse_kxChangesets = Lens.lens (\ListKxChangesetsResponse' {kxChangesets} -> kxChangesets) (\s@ListKxChangesetsResponse' {} a -> s {kxChangesets = a} :: ListKxChangesetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where a results page should begin.
listKxChangesetsResponse_nextToken :: Lens.Lens' ListKxChangesetsResponse (Prelude.Maybe Prelude.Text)
listKxChangesetsResponse_nextToken = Lens.lens (\ListKxChangesetsResponse' {nextToken} -> nextToken) (\s@ListKxChangesetsResponse' {} a -> s {nextToken = a} :: ListKxChangesetsResponse)

-- | The response's http status code.
listKxChangesetsResponse_httpStatus :: Lens.Lens' ListKxChangesetsResponse Prelude.Int
listKxChangesetsResponse_httpStatus = Lens.lens (\ListKxChangesetsResponse' {httpStatus} -> httpStatus) (\s@ListKxChangesetsResponse' {} a -> s {httpStatus = a} :: ListKxChangesetsResponse)

instance Prelude.NFData ListKxChangesetsResponse where
  rnf ListKxChangesetsResponse' {..} =
    Prelude.rnf kxChangesets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
