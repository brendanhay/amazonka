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
-- Module      : Amazonka.ServerlessApplicationRepository.ListApplicationDependencies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of applications nested in the containing application.
--
-- This operation returns paginated results.
module Amazonka.ServerlessApplicationRepository.ListApplicationDependencies
  ( -- * Creating a Request
    ListApplicationDependencies (..),
    newListApplicationDependencies,

    -- * Request Lenses
    listApplicationDependencies_maxItems,
    listApplicationDependencies_nextToken,
    listApplicationDependencies_semanticVersion,
    listApplicationDependencies_applicationId,

    -- * Destructuring the Response
    ListApplicationDependenciesResponse (..),
    newListApplicationDependenciesResponse,

    -- * Response Lenses
    listApplicationDependenciesResponse_dependencies,
    listApplicationDependenciesResponse_nextToken,
    listApplicationDependenciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { -- | The total number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The semantic version of the application to get.
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationDependencies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listApplicationDependencies_maxItems' - The total number of items to return.
--
-- 'nextToken', 'listApplicationDependencies_nextToken' - A token to specify where to start paginating.
--
-- 'semanticVersion', 'listApplicationDependencies_semanticVersion' - The semantic version of the application to get.
--
-- 'applicationId', 'listApplicationDependencies_applicationId' - The Amazon Resource Name (ARN) of the application.
newListApplicationDependencies ::
  -- | 'applicationId'
  Prelude.Text ->
  ListApplicationDependencies
newListApplicationDependencies pApplicationId_ =
  ListApplicationDependencies'
    { maxItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      semanticVersion = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The total number of items to return.
listApplicationDependencies_maxItems :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Natural)
listApplicationDependencies_maxItems = Lens.lens (\ListApplicationDependencies' {maxItems} -> maxItems) (\s@ListApplicationDependencies' {} a -> s {maxItems = a} :: ListApplicationDependencies)

-- | A token to specify where to start paginating.
listApplicationDependencies_nextToken :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Text)
listApplicationDependencies_nextToken = Lens.lens (\ListApplicationDependencies' {nextToken} -> nextToken) (\s@ListApplicationDependencies' {} a -> s {nextToken = a} :: ListApplicationDependencies)

-- | The semantic version of the application to get.
listApplicationDependencies_semanticVersion :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Text)
listApplicationDependencies_semanticVersion = Lens.lens (\ListApplicationDependencies' {semanticVersion} -> semanticVersion) (\s@ListApplicationDependencies' {} a -> s {semanticVersion = a} :: ListApplicationDependencies)

-- | The Amazon Resource Name (ARN) of the application.
listApplicationDependencies_applicationId :: Lens.Lens' ListApplicationDependencies Prelude.Text
listApplicationDependencies_applicationId = Lens.lens (\ListApplicationDependencies' {applicationId} -> applicationId) (\s@ListApplicationDependencies' {} a -> s {applicationId = a} :: ListApplicationDependencies)

instance Core.AWSPager ListApplicationDependencies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_dependencies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApplicationDependencies_nextToken
          Lens..~ rs
          Lens.^? listApplicationDependenciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationDependencies where
  type
    AWSResponse ListApplicationDependencies =
      ListApplicationDependenciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationDependenciesResponse'
            Prelude.<$> (x Data..?> "dependencies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationDependencies where
  hashWithSalt _salt ListApplicationDependencies' {..} =
    _salt `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListApplicationDependencies where
  rnf ListApplicationDependencies' {..} =
    Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListApplicationDependencies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApplicationDependencies where
  toPath ListApplicationDependencies' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/dependencies"
      ]

instance Data.ToQuery ListApplicationDependencies where
  toQuery ListApplicationDependencies' {..} =
    Prelude.mconcat
      [ "maxItems" Data.=: maxItems,
        "nextToken" Data.=: nextToken,
        "semanticVersion" Data.=: semanticVersion
      ]

-- | /See:/ 'newListApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { -- | An array of application summaries nested in the application.
    dependencies :: Prelude.Maybe [ApplicationDependencySummary],
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationDependenciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependencies', 'listApplicationDependenciesResponse_dependencies' - An array of application summaries nested in the application.
--
-- 'nextToken', 'listApplicationDependenciesResponse_nextToken' - The token to request the next page of results.
--
-- 'httpStatus', 'listApplicationDependenciesResponse_httpStatus' - The response's http status code.
newListApplicationDependenciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationDependenciesResponse
newListApplicationDependenciesResponse pHttpStatus_ =
  ListApplicationDependenciesResponse'
    { dependencies =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of application summaries nested in the application.
listApplicationDependenciesResponse_dependencies :: Lens.Lens' ListApplicationDependenciesResponse (Prelude.Maybe [ApplicationDependencySummary])
listApplicationDependenciesResponse_dependencies = Lens.lens (\ListApplicationDependenciesResponse' {dependencies} -> dependencies) (\s@ListApplicationDependenciesResponse' {} a -> s {dependencies = a} :: ListApplicationDependenciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to request the next page of results.
listApplicationDependenciesResponse_nextToken :: Lens.Lens' ListApplicationDependenciesResponse (Prelude.Maybe Prelude.Text)
listApplicationDependenciesResponse_nextToken = Lens.lens (\ListApplicationDependenciesResponse' {nextToken} -> nextToken) (\s@ListApplicationDependenciesResponse' {} a -> s {nextToken = a} :: ListApplicationDependenciesResponse)

-- | The response's http status code.
listApplicationDependenciesResponse_httpStatus :: Lens.Lens' ListApplicationDependenciesResponse Prelude.Int
listApplicationDependenciesResponse_httpStatus = Lens.lens (\ListApplicationDependenciesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationDependenciesResponse' {} a -> s {httpStatus = a} :: ListApplicationDependenciesResponse)

instance
  Prelude.NFData
    ListApplicationDependenciesResponse
  where
  rnf ListApplicationDependenciesResponse' {..} =
    Prelude.rnf dependencies
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
