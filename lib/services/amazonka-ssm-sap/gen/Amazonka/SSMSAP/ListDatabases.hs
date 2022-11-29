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
-- Module      : Amazonka.SSMSAP.ListDatabases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAP HANA databases of an application registered with AWS
-- Systems Manager for SAP.
--
-- This operation returns paginated results.
module Amazonka.SSMSAP.ListDatabases
  ( -- * Creating a Request
    ListDatabases (..),
    newListDatabases,

    -- * Request Lenses
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_componentId,
    listDatabases_applicationId,

    -- * Destructuring the Response
    ListDatabasesResponse (..),
    newListDatabasesResponse,

    -- * Response Lenses
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newListDatabases' smart constructor.
data ListDatabases = ListDatabases'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    componentId :: Prelude.Maybe Prelude.Text,
    applicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatabases_nextToken' -
--
-- 'maxResults', 'listDatabases_maxResults' -
--
-- 'componentId', 'listDatabases_componentId' -
--
-- 'applicationId', 'listDatabases_applicationId' -
newListDatabases ::
  ListDatabases
newListDatabases =
  ListDatabases'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      componentId = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- |
listDatabases_nextToken :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_nextToken = Lens.lens (\ListDatabases' {nextToken} -> nextToken) (\s@ListDatabases' {} a -> s {nextToken = a} :: ListDatabases)

-- |
listDatabases_maxResults :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Natural)
listDatabases_maxResults = Lens.lens (\ListDatabases' {maxResults} -> maxResults) (\s@ListDatabases' {} a -> s {maxResults = a} :: ListDatabases)

-- |
listDatabases_componentId :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_componentId = Lens.lens (\ListDatabases' {componentId} -> componentId) (\s@ListDatabases' {} a -> s {componentId = a} :: ListDatabases)

-- |
listDatabases_applicationId :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_applicationId = Lens.lens (\ListDatabases' {applicationId} -> applicationId) (\s@ListDatabases' {} a -> s {applicationId = a} :: ListDatabases)

instance Core.AWSPager ListDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatabasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatabasesResponse_databases Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatabases_nextToken
          Lens..~ rs
          Lens.^? listDatabasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDatabases where
  type
    AWSResponse ListDatabases =
      ListDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatabasesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Databases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatabases where
  hashWithSalt _salt ListDatabases' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListDatabases where
  rnf ListDatabases' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders ListDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDatabases where
  toJSON ListDatabases' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ComponentId" Core..=) Prelude.<$> componentId,
            ("ApplicationId" Core..=) Prelude.<$> applicationId
          ]
      )

instance Core.ToPath ListDatabases where
  toPath = Prelude.const "/list-databases"

instance Core.ToQuery ListDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatabasesResponse' smart constructor.
data ListDatabasesResponse = ListDatabasesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    databases :: Prelude.Maybe [DatabaseSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatabasesResponse_nextToken' -
--
-- 'databases', 'listDatabasesResponse_databases' -
--
-- 'httpStatus', 'listDatabasesResponse_httpStatus' - The response's http status code.
newListDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatabasesResponse
newListDatabasesResponse pHttpStatus_ =
  ListDatabasesResponse'
    { nextToken = Prelude.Nothing,
      databases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
listDatabasesResponse_nextToken :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe Prelude.Text)
listDatabasesResponse_nextToken = Lens.lens (\ListDatabasesResponse' {nextToken} -> nextToken) (\s@ListDatabasesResponse' {} a -> s {nextToken = a} :: ListDatabasesResponse)

-- |
listDatabasesResponse_databases :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe [DatabaseSummary])
listDatabasesResponse_databases = Lens.lens (\ListDatabasesResponse' {databases} -> databases) (\s@ListDatabasesResponse' {} a -> s {databases = a} :: ListDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatabasesResponse_httpStatus :: Lens.Lens' ListDatabasesResponse Prelude.Int
listDatabasesResponse_httpStatus = Lens.lens (\ListDatabasesResponse' {httpStatus} -> httpStatus) (\s@ListDatabasesResponse' {} a -> s {httpStatus = a} :: ListDatabasesResponse)

instance Prelude.NFData ListDatabasesResponse where
  rnf ListDatabasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf databases
      `Prelude.seq` Prelude.rnf httpStatus
