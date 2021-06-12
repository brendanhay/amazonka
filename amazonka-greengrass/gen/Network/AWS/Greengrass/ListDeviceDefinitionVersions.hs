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
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitionVersions
  ( -- * Creating a Request
    ListDeviceDefinitionVersions (..),
    newListDeviceDefinitionVersions,

    -- * Request Lenses
    listDeviceDefinitionVersions_nextToken,
    listDeviceDefinitionVersions_maxResults,
    listDeviceDefinitionVersions_deviceDefinitionId,

    -- * Destructuring the Response
    ListDeviceDefinitionVersionsResponse (..),
    newListDeviceDefinitionVersionsResponse,

    -- * Response Lenses
    listDeviceDefinitionVersionsResponse_nextToken,
    listDeviceDefinitionVersionsResponse_versions,
    listDeviceDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listDeviceDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'deviceDefinitionId', 'listDeviceDefinitionVersions_deviceDefinitionId' - The ID of the device definition.
newListDeviceDefinitionVersions ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  ListDeviceDefinitionVersions
newListDeviceDefinitionVersions pDeviceDefinitionId_ =
  ListDeviceDefinitionVersions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionVersions_nextToken :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
listDeviceDefinitionVersions_nextToken = Lens.lens (\ListDeviceDefinitionVersions' {nextToken} -> nextToken) (\s@ListDeviceDefinitionVersions' {} a -> s {nextToken = a} :: ListDeviceDefinitionVersions)

-- | The maximum number of results to be returned per request.
listDeviceDefinitionVersions_maxResults :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
listDeviceDefinitionVersions_maxResults = Lens.lens (\ListDeviceDefinitionVersions' {maxResults} -> maxResults) (\s@ListDeviceDefinitionVersions' {} a -> s {maxResults = a} :: ListDeviceDefinitionVersions)

-- | The ID of the device definition.
listDeviceDefinitionVersions_deviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Core.Text
listDeviceDefinitionVersions_deviceDefinitionId = Lens.lens (\ListDeviceDefinitionVersions' {deviceDefinitionId} -> deviceDefinitionId) (\s@ListDeviceDefinitionVersions' {} a -> s {deviceDefinitionId = a} :: ListDeviceDefinitionVersions)

instance Core.AWSPager ListDeviceDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeviceDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listDeviceDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDeviceDefinitionVersions where
  type
    AWSResponse ListDeviceDefinitionVersions =
      ListDeviceDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeviceDefinitionVersions

instance Core.NFData ListDeviceDefinitionVersions

instance Core.ToHeaders ListDeviceDefinitionVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDeviceDefinitionVersions where
  toPath ListDeviceDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListDeviceDefinitionVersions where
  toQuery ListDeviceDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listDeviceDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listDeviceDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListDeviceDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeviceDefinitionVersionsResponse
newListDeviceDefinitionVersionsResponse pHttpStatus_ =
  ListDeviceDefinitionVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionVersionsResponse_nextToken :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe Core.Text)
listDeviceDefinitionVersionsResponse_nextToken = Lens.lens (\ListDeviceDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListDeviceDefinitionVersionsResponse)

-- | Information about a version.
listDeviceDefinitionVersionsResponse_versions :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listDeviceDefinitionVersionsResponse_versions = Lens.lens (\ListDeviceDefinitionVersionsResponse' {versions} -> versions) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {versions = a} :: ListDeviceDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeviceDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Core.Int
listDeviceDefinitionVersionsResponse_httpStatus = Lens.lens (\ListDeviceDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListDeviceDefinitionVersionsResponse)

instance
  Core.NFData
    ListDeviceDefinitionVersionsResponse
