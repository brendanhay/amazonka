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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { -- | A token to specify where to start paginating.
    nextToken :: Core.Maybe Core.Text,
    -- | The semantic version of the application to get.
    semanticVersion :: Core.Maybe Core.Text,
    -- | The total number of items to return.
    maxItems :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListApplicationDependencies
newListApplicationDependencies pApplicationId_ =
  ListApplicationDependencies'
    { nextToken =
        Core.Nothing,
      semanticVersion = Core.Nothing,
      maxItems = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | A token to specify where to start paginating.
listApplicationDependencies_nextToken :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Text)
listApplicationDependencies_nextToken = Lens.lens (\ListApplicationDependencies' {nextToken} -> nextToken) (\s@ListApplicationDependencies' {} a -> s {nextToken = a} :: ListApplicationDependencies)

-- | The semantic version of the application to get.
listApplicationDependencies_semanticVersion :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Text)
listApplicationDependencies_semanticVersion = Lens.lens (\ListApplicationDependencies' {semanticVersion} -> semanticVersion) (\s@ListApplicationDependencies' {} a -> s {semanticVersion = a} :: ListApplicationDependencies)

-- | The total number of items to return.
listApplicationDependencies_maxItems :: Lens.Lens' ListApplicationDependencies (Core.Maybe Core.Natural)
listApplicationDependencies_maxItems = Lens.lens (\ListApplicationDependencies' {maxItems} -> maxItems) (\s@ListApplicationDependencies' {} a -> s {maxItems = a} :: ListApplicationDependencies)

-- | The Amazon Resource Name (ARN) of the application.
listApplicationDependencies_applicationId :: Lens.Lens' ListApplicationDependencies Core.Text
listApplicationDependencies_applicationId = Lens.lens (\ListApplicationDependencies' {applicationId} -> applicationId) (\s@ListApplicationDependencies' {} a -> s {applicationId = a} :: ListApplicationDependencies)

instance Core.AWSPager ListApplicationDependencies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationDependenciesResponse_dependencies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplicationDependencies_nextToken
          Lens..~ rs
          Lens.^? listApplicationDependenciesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListApplicationDependencies where
  type
    AWSResponse ListApplicationDependencies =
      ListApplicationDependenciesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationDependenciesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "dependencies" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplicationDependencies

instance Core.NFData ListApplicationDependencies

instance Core.ToHeaders ListApplicationDependencies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListApplicationDependencies where
  toPath ListApplicationDependencies' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/dependencies"
      ]

instance Core.ToQuery ListApplicationDependencies where
  toQuery ListApplicationDependencies' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "semanticVersion" Core.=: semanticVersion,
        "maxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of application summaries nested in the application.
    dependencies :: Core.Maybe [ApplicationDependencySummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListApplicationDependenciesResponse
newListApplicationDependenciesResponse pHttpStatus_ =
  ListApplicationDependenciesResponse'
    { nextToken =
        Core.Nothing,
      dependencies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationDependenciesResponse_nextToken :: Lens.Lens' ListApplicationDependenciesResponse (Core.Maybe Core.Text)
listApplicationDependenciesResponse_nextToken = Lens.lens (\ListApplicationDependenciesResponse' {nextToken} -> nextToken) (\s@ListApplicationDependenciesResponse' {} a -> s {nextToken = a} :: ListApplicationDependenciesResponse)

-- | An array of application summaries nested in the application.
listApplicationDependenciesResponse_dependencies :: Lens.Lens' ListApplicationDependenciesResponse (Core.Maybe [ApplicationDependencySummary])
listApplicationDependenciesResponse_dependencies = Lens.lens (\ListApplicationDependenciesResponse' {dependencies} -> dependencies) (\s@ListApplicationDependenciesResponse' {} a -> s {dependencies = a} :: ListApplicationDependenciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApplicationDependenciesResponse_httpStatus :: Lens.Lens' ListApplicationDependenciesResponse Core.Int
listApplicationDependenciesResponse_httpStatus = Lens.lens (\ListApplicationDependenciesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationDependenciesResponse' {} a -> s {httpStatus = a} :: ListApplicationDependenciesResponse)

instance
  Core.NFData
    ListApplicationDependenciesResponse
