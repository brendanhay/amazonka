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
-- Module      : Amazonka.Panorama.ListApplicationInstanceDependencies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of application instance dependencies.
module Amazonka.Panorama.ListApplicationInstanceDependencies
  ( -- * Creating a Request
    ListApplicationInstanceDependencies (..),
    newListApplicationInstanceDependencies,

    -- * Request Lenses
    listApplicationInstanceDependencies_nextToken,
    listApplicationInstanceDependencies_maxResults,
    listApplicationInstanceDependencies_applicationInstanceId,

    -- * Destructuring the Response
    ListApplicationInstanceDependenciesResponse (..),
    newListApplicationInstanceDependenciesResponse,

    -- * Response Lenses
    listApplicationInstanceDependenciesResponse_nextToken,
    listApplicationInstanceDependenciesResponse_packageObjects,
    listApplicationInstanceDependenciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationInstanceDependencies' smart constructor.
data ListApplicationInstanceDependencies = ListApplicationInstanceDependencies'
  { -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of application instance dependencies to return in one
    -- page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationInstanceDependencies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationInstanceDependencies_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'maxResults', 'listApplicationInstanceDependencies_maxResults' - The maximum number of application instance dependencies to return in one
-- page of results.
--
-- 'applicationInstanceId', 'listApplicationInstanceDependencies_applicationInstanceId' - The application instance\'s ID.
newListApplicationInstanceDependencies ::
  -- | 'applicationInstanceId'
  Prelude.Text ->
  ListApplicationInstanceDependencies
newListApplicationInstanceDependencies
  pApplicationInstanceId_ =
    ListApplicationInstanceDependencies'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        applicationInstanceId =
          pApplicationInstanceId_
      }

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listApplicationInstanceDependencies_nextToken :: Lens.Lens' ListApplicationInstanceDependencies (Prelude.Maybe Prelude.Text)
listApplicationInstanceDependencies_nextToken = Lens.lens (\ListApplicationInstanceDependencies' {nextToken} -> nextToken) (\s@ListApplicationInstanceDependencies' {} a -> s {nextToken = a} :: ListApplicationInstanceDependencies)

-- | The maximum number of application instance dependencies to return in one
-- page of results.
listApplicationInstanceDependencies_maxResults :: Lens.Lens' ListApplicationInstanceDependencies (Prelude.Maybe Prelude.Natural)
listApplicationInstanceDependencies_maxResults = Lens.lens (\ListApplicationInstanceDependencies' {maxResults} -> maxResults) (\s@ListApplicationInstanceDependencies' {} a -> s {maxResults = a} :: ListApplicationInstanceDependencies)

-- | The application instance\'s ID.
listApplicationInstanceDependencies_applicationInstanceId :: Lens.Lens' ListApplicationInstanceDependencies Prelude.Text
listApplicationInstanceDependencies_applicationInstanceId = Lens.lens (\ListApplicationInstanceDependencies' {applicationInstanceId} -> applicationInstanceId) (\s@ListApplicationInstanceDependencies' {} a -> s {applicationInstanceId = a} :: ListApplicationInstanceDependencies)

instance
  Core.AWSRequest
    ListApplicationInstanceDependencies
  where
  type
    AWSResponse ListApplicationInstanceDependencies =
      ListApplicationInstanceDependenciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationInstanceDependenciesResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (x Core..?> "PackageObjects" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListApplicationInstanceDependencies
  where
  hashWithSalt
    _salt
    ListApplicationInstanceDependencies' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` applicationInstanceId

instance
  Prelude.NFData
    ListApplicationInstanceDependencies
  where
  rnf ListApplicationInstanceDependencies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf applicationInstanceId

instance
  Core.ToHeaders
    ListApplicationInstanceDependencies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    ListApplicationInstanceDependencies
  where
  toPath ListApplicationInstanceDependencies' {..} =
    Prelude.mconcat
      [ "/application-instances/",
        Core.toBS applicationInstanceId,
        "/package-dependencies"
      ]

instance
  Core.ToQuery
    ListApplicationInstanceDependencies
  where
  toQuery ListApplicationInstanceDependencies' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListApplicationInstanceDependenciesResponse' smart constructor.
data ListApplicationInstanceDependenciesResponse = ListApplicationInstanceDependenciesResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of package objects.
    packageObjects :: Prelude.Maybe [PackageObject],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationInstanceDependenciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationInstanceDependenciesResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'packageObjects', 'listApplicationInstanceDependenciesResponse_packageObjects' - A list of package objects.
--
-- 'httpStatus', 'listApplicationInstanceDependenciesResponse_httpStatus' - The response's http status code.
newListApplicationInstanceDependenciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationInstanceDependenciesResponse
newListApplicationInstanceDependenciesResponse
  pHttpStatus_ =
    ListApplicationInstanceDependenciesResponse'
      { nextToken =
          Prelude.Nothing,
        packageObjects =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token that\'s included if more results are available.
listApplicationInstanceDependenciesResponse_nextToken :: Lens.Lens' ListApplicationInstanceDependenciesResponse (Prelude.Maybe Prelude.Text)
listApplicationInstanceDependenciesResponse_nextToken = Lens.lens (\ListApplicationInstanceDependenciesResponse' {nextToken} -> nextToken) (\s@ListApplicationInstanceDependenciesResponse' {} a -> s {nextToken = a} :: ListApplicationInstanceDependenciesResponse)

-- | A list of package objects.
listApplicationInstanceDependenciesResponse_packageObjects :: Lens.Lens' ListApplicationInstanceDependenciesResponse (Prelude.Maybe [PackageObject])
listApplicationInstanceDependenciesResponse_packageObjects = Lens.lens (\ListApplicationInstanceDependenciesResponse' {packageObjects} -> packageObjects) (\s@ListApplicationInstanceDependenciesResponse' {} a -> s {packageObjects = a} :: ListApplicationInstanceDependenciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApplicationInstanceDependenciesResponse_httpStatus :: Lens.Lens' ListApplicationInstanceDependenciesResponse Prelude.Int
listApplicationInstanceDependenciesResponse_httpStatus = Lens.lens (\ListApplicationInstanceDependenciesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationInstanceDependenciesResponse' {} a -> s {httpStatus = a} :: ListApplicationInstanceDependenciesResponse)

instance
  Prelude.NFData
    ListApplicationInstanceDependenciesResponse
  where
  rnf ListApplicationInstanceDependenciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageObjects
      `Prelude.seq` Prelude.rnf httpStatus
