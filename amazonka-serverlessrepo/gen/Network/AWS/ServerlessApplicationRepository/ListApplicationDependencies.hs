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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of applications nested in the containing application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
  ( -- * Creating a Request
    ListApplicationDependencies (..),
    newListApplicationDependencies,

    -- * Request Lenses
    listApplicationDependencies_nextToken,
    listApplicationDependencies_semanticVersion,
    listApplicationDependencies_maxItems,
    listApplicationDependencies_applicationId,

    -- * Destructuring the Response
    ListApplicationDependenciesResponse (..),
    newListApplicationDependenciesResponse,

    -- * Response Lenses
    listApplicationDependenciesResponse_nextToken,
    listApplicationDependenciesResponse_dependencies,
    listApplicationDependenciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { -- | A token to specify where to start paginating.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The semantic version of the application to get.
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationDependencies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationDependencies_nextToken' - A token to specify where to start paginating.
--
-- 'semanticVersion', 'listApplicationDependencies_semanticVersion' - The semantic version of the application to get.
--
-- 'maxItems', 'listApplicationDependencies_maxItems' - The total number of items to return.
--
-- 'applicationId', 'listApplicationDependencies_applicationId' - The Amazon Resource Name (ARN) of the application.
newListApplicationDependencies ::
  -- | 'applicationId'
  Prelude.Text ->
  ListApplicationDependencies
newListApplicationDependencies pApplicationId_ =
  ListApplicationDependencies'
    { nextToken =
        Prelude.Nothing,
      semanticVersion = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | A token to specify where to start paginating.
listApplicationDependencies_nextToken :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Text)
listApplicationDependencies_nextToken = Lens.lens (\ListApplicationDependencies' {nextToken} -> nextToken) (\s@ListApplicationDependencies' {} a -> s {nextToken = a} :: ListApplicationDependencies)

-- | The semantic version of the application to get.
listApplicationDependencies_semanticVersion :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Text)
listApplicationDependencies_semanticVersion = Lens.lens (\ListApplicationDependencies' {semanticVersion} -> semanticVersion) (\s@ListApplicationDependencies' {} a -> s {semanticVersion = a} :: ListApplicationDependencies)

-- | The total number of items to return.
listApplicationDependencies_maxItems :: Lens.Lens' ListApplicationDependencies (Prelude.Maybe Prelude.Natural)
listApplicationDependencies_maxItems = Lens.lens (\ListApplicationDependencies' {maxItems} -> maxItems) (\s@ListApplicationDependencies' {} a -> s {maxItems = a} :: ListApplicationDependencies)

-- | The Amazon Resource Name (ARN) of the application.
listApplicationDependencies_applicationId :: Lens.Lens' ListApplicationDependencies Prelude.Text
listApplicationDependencies_applicationId = Lens.lens (\ListApplicationDependencies' {applicationId} -> applicationId) (\s@ListApplicationDependencies' {} a -> s {applicationId = a} :: ListApplicationDependencies)

instance Pager.AWSPager ListApplicationDependencies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_dependencies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listApplicationDependencies_nextToken
          Lens..~ rs
          Lens.^? listApplicationDependenciesResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListApplicationDependencies
  where
  type
    Rs ListApplicationDependencies =
      ListApplicationDependenciesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationDependenciesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "dependencies"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationDependencies

instance Prelude.NFData ListApplicationDependencies

instance
  Prelude.ToHeaders
    ListApplicationDependencies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListApplicationDependencies where
  toPath ListApplicationDependencies' {..} =
    Prelude.mconcat
      [ "/applications/",
        Prelude.toBS applicationId,
        "/dependencies"
      ]

instance Prelude.ToQuery ListApplicationDependencies where
  toQuery ListApplicationDependencies' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "semanticVersion" Prelude.=: semanticVersion,
        "maxItems" Prelude.=: maxItems
      ]

-- | /See:/ 'newListApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of application summaries nested in the application.
    dependencies :: Prelude.Maybe [ApplicationDependencySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationDependenciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationDependenciesResponse_nextToken' - The token to request the next page of results.
--
-- 'dependencies', 'listApplicationDependenciesResponse_dependencies' - An array of application summaries nested in the application.
--
-- 'httpStatus', 'listApplicationDependenciesResponse_httpStatus' - The response's http status code.
newListApplicationDependenciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationDependenciesResponse
newListApplicationDependenciesResponse pHttpStatus_ =
  ListApplicationDependenciesResponse'
    { nextToken =
        Prelude.Nothing,
      dependencies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationDependenciesResponse_nextToken :: Lens.Lens' ListApplicationDependenciesResponse (Prelude.Maybe Prelude.Text)
listApplicationDependenciesResponse_nextToken = Lens.lens (\ListApplicationDependenciesResponse' {nextToken} -> nextToken) (\s@ListApplicationDependenciesResponse' {} a -> s {nextToken = a} :: ListApplicationDependenciesResponse)

-- | An array of application summaries nested in the application.
listApplicationDependenciesResponse_dependencies :: Lens.Lens' ListApplicationDependenciesResponse (Prelude.Maybe [ApplicationDependencySummary])
listApplicationDependenciesResponse_dependencies = Lens.lens (\ListApplicationDependenciesResponse' {dependencies} -> dependencies) (\s@ListApplicationDependenciesResponse' {} a -> s {dependencies = a} :: ListApplicationDependenciesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listApplicationDependenciesResponse_httpStatus :: Lens.Lens' ListApplicationDependenciesResponse Prelude.Int
listApplicationDependenciesResponse_httpStatus = Lens.lens (\ListApplicationDependenciesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationDependenciesResponse' {} a -> s {httpStatus = a} :: ListApplicationDependenciesResponse)

instance
  Prelude.NFData
    ListApplicationDependenciesResponse
