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
-- Module      : Amazonka.SnowDeviceManagement.ListDeviceResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon Web Services resources available for a
-- device. Currently, Amazon EC2 instances are the only supported resource
-- type.
--
-- This operation returns paginated results.
module Amazonka.SnowDeviceManagement.ListDeviceResources
  ( -- * Creating a Request
    ListDeviceResources (..),
    newListDeviceResources,

    -- * Request Lenses
    listDeviceResources_maxResults,
    listDeviceResources_nextToken,
    listDeviceResources_type,
    listDeviceResources_managedDeviceId,

    -- * Destructuring the Response
    ListDeviceResourcesResponse (..),
    newListDeviceResourcesResponse,

    -- * Response Lenses
    listDeviceResourcesResponse_nextToken,
    listDeviceResourcesResponse_resources,
    listDeviceResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newListDeviceResources' smart constructor.
data ListDeviceResources = ListDeviceResources'
  { -- | The maximum number of resources per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure used to filter the results by type of resource.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ID of the managed device that you are listing the resources of.
    managedDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeviceResources_maxResults' - The maximum number of resources per page.
--
-- 'nextToken', 'listDeviceResources_nextToken' - A pagination token to continue to the next page of results.
--
-- 'type'', 'listDeviceResources_type' - A structure used to filter the results by type of resource.
--
-- 'managedDeviceId', 'listDeviceResources_managedDeviceId' - The ID of the managed device that you are listing the resources of.
newListDeviceResources ::
  -- | 'managedDeviceId'
  Prelude.Text ->
  ListDeviceResources
newListDeviceResources pManagedDeviceId_ =
  ListDeviceResources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      managedDeviceId = pManagedDeviceId_
    }

-- | The maximum number of resources per page.
listDeviceResources_maxResults :: Lens.Lens' ListDeviceResources (Prelude.Maybe Prelude.Natural)
listDeviceResources_maxResults = Lens.lens (\ListDeviceResources' {maxResults} -> maxResults) (\s@ListDeviceResources' {} a -> s {maxResults = a} :: ListDeviceResources)

-- | A pagination token to continue to the next page of results.
listDeviceResources_nextToken :: Lens.Lens' ListDeviceResources (Prelude.Maybe Prelude.Text)
listDeviceResources_nextToken = Lens.lens (\ListDeviceResources' {nextToken} -> nextToken) (\s@ListDeviceResources' {} a -> s {nextToken = a} :: ListDeviceResources)

-- | A structure used to filter the results by type of resource.
listDeviceResources_type :: Lens.Lens' ListDeviceResources (Prelude.Maybe Prelude.Text)
listDeviceResources_type = Lens.lens (\ListDeviceResources' {type'} -> type') (\s@ListDeviceResources' {} a -> s {type' = a} :: ListDeviceResources)

-- | The ID of the managed device that you are listing the resources of.
listDeviceResources_managedDeviceId :: Lens.Lens' ListDeviceResources Prelude.Text
listDeviceResources_managedDeviceId = Lens.lens (\ListDeviceResources' {managedDeviceId} -> managedDeviceId) (\s@ListDeviceResources' {} a -> s {managedDeviceId = a} :: ListDeviceResources)

instance Core.AWSPager ListDeviceResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceResourcesResponse_resources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDeviceResources_nextToken
          Lens..~ rs
          Lens.^? listDeviceResourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceResources where
  type
    AWSResponse ListDeviceResources =
      ListDeviceResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceResources where
  hashWithSalt _salt ListDeviceResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` managedDeviceId

instance Prelude.NFData ListDeviceResources where
  rnf ListDeviceResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf managedDeviceId

instance Data.ToHeaders ListDeviceResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeviceResources where
  toPath ListDeviceResources' {..} =
    Prelude.mconcat
      [ "/managed-device/",
        Data.toBS managedDeviceId,
        "/resources"
      ]

instance Data.ToQuery ListDeviceResources where
  toQuery ListDeviceResources' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "type" Data.=: type'
      ]

-- | /See:/ 'newListDeviceResourcesResponse' smart constructor.
data ListDeviceResourcesResponse = ListDeviceResourcesResponse'
  { -- | A pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure defining the resource\'s type, Amazon Resource Name (ARN),
    -- and ID.
    resources :: Prelude.Maybe [ResourceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceResourcesResponse_nextToken' - A pagination token to continue to the next page of results.
--
-- 'resources', 'listDeviceResourcesResponse_resources' - A structure defining the resource\'s type, Amazon Resource Name (ARN),
-- and ID.
--
-- 'httpStatus', 'listDeviceResourcesResponse_httpStatus' - The response's http status code.
newListDeviceResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceResourcesResponse
newListDeviceResourcesResponse pHttpStatus_ =
  ListDeviceResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token to continue to the next page of results.
listDeviceResourcesResponse_nextToken :: Lens.Lens' ListDeviceResourcesResponse (Prelude.Maybe Prelude.Text)
listDeviceResourcesResponse_nextToken = Lens.lens (\ListDeviceResourcesResponse' {nextToken} -> nextToken) (\s@ListDeviceResourcesResponse' {} a -> s {nextToken = a} :: ListDeviceResourcesResponse)

-- | A structure defining the resource\'s type, Amazon Resource Name (ARN),
-- and ID.
listDeviceResourcesResponse_resources :: Lens.Lens' ListDeviceResourcesResponse (Prelude.Maybe [ResourceSummary])
listDeviceResourcesResponse_resources = Lens.lens (\ListDeviceResourcesResponse' {resources} -> resources) (\s@ListDeviceResourcesResponse' {} a -> s {resources = a} :: ListDeviceResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDeviceResourcesResponse_httpStatus :: Lens.Lens' ListDeviceResourcesResponse Prelude.Int
listDeviceResourcesResponse_httpStatus = Lens.lens (\ListDeviceResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDeviceResourcesResponse' {} a -> s {httpStatus = a} :: ListDeviceResourcesResponse)

instance Prelude.NFData ListDeviceResourcesResponse where
  rnf ListDeviceResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
