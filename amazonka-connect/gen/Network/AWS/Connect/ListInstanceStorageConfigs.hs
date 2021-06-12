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
-- Module      : Network.AWS.Connect.ListInstanceStorageConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of storage configs for the identified instance
-- and resource type.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceStorageConfigs
  ( -- * Creating a Request
    ListInstanceStorageConfigs (..),
    newListInstanceStorageConfigs,

    -- * Request Lenses
    listInstanceStorageConfigs_nextToken,
    listInstanceStorageConfigs_maxResults,
    listInstanceStorageConfigs_instanceId,
    listInstanceStorageConfigs_resourceType,

    -- * Destructuring the Response
    ListInstanceStorageConfigsResponse (..),
    newListInstanceStorageConfigsResponse,

    -- * Response Lenses
    listInstanceStorageConfigsResponse_nextToken,
    listInstanceStorageConfigsResponse_storageConfigs,
    listInstanceStorageConfigsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInstanceStorageConfigs' smart constructor.
data ListInstanceStorageConfigs = ListInstanceStorageConfigs'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceStorageConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceStorageConfigs_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listInstanceStorageConfigs_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listInstanceStorageConfigs_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'resourceType', 'listInstanceStorageConfigs_resourceType' - A valid resource type.
newListInstanceStorageConfigs ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  ListInstanceStorageConfigs
newListInstanceStorageConfigs
  pInstanceId_
  pResourceType_ =
    ListInstanceStorageConfigs'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        instanceId = pInstanceId_,
        resourceType = pResourceType_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listInstanceStorageConfigs_nextToken :: Lens.Lens' ListInstanceStorageConfigs (Core.Maybe Core.Text)
listInstanceStorageConfigs_nextToken = Lens.lens (\ListInstanceStorageConfigs' {nextToken} -> nextToken) (\s@ListInstanceStorageConfigs' {} a -> s {nextToken = a} :: ListInstanceStorageConfigs)

-- | The maximum number of results to return per page.
listInstanceStorageConfigs_maxResults :: Lens.Lens' ListInstanceStorageConfigs (Core.Maybe Core.Natural)
listInstanceStorageConfigs_maxResults = Lens.lens (\ListInstanceStorageConfigs' {maxResults} -> maxResults) (\s@ListInstanceStorageConfigs' {} a -> s {maxResults = a} :: ListInstanceStorageConfigs)

-- | The identifier of the Amazon Connect instance.
listInstanceStorageConfigs_instanceId :: Lens.Lens' ListInstanceStorageConfigs Core.Text
listInstanceStorageConfigs_instanceId = Lens.lens (\ListInstanceStorageConfigs' {instanceId} -> instanceId) (\s@ListInstanceStorageConfigs' {} a -> s {instanceId = a} :: ListInstanceStorageConfigs)

-- | A valid resource type.
listInstanceStorageConfigs_resourceType :: Lens.Lens' ListInstanceStorageConfigs InstanceStorageResourceType
listInstanceStorageConfigs_resourceType = Lens.lens (\ListInstanceStorageConfigs' {resourceType} -> resourceType) (\s@ListInstanceStorageConfigs' {} a -> s {resourceType = a} :: ListInstanceStorageConfigs)

instance Core.AWSPager ListInstanceStorageConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceStorageConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceStorageConfigsResponse_storageConfigs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstanceStorageConfigs_nextToken
          Lens..~ rs
          Lens.^? listInstanceStorageConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListInstanceStorageConfigs where
  type
    AWSResponse ListInstanceStorageConfigs =
      ListInstanceStorageConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceStorageConfigsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "StorageConfigs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstanceStorageConfigs

instance Core.NFData ListInstanceStorageConfigs

instance Core.ToHeaders ListInstanceStorageConfigs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListInstanceStorageConfigs where
  toPath ListInstanceStorageConfigs' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/storage-configs"
      ]

instance Core.ToQuery ListInstanceStorageConfigs where
  toQuery ListInstanceStorageConfigs' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "resourceType" Core.=: resourceType
      ]

-- | /See:/ 'newListInstanceStorageConfigsResponse' smart constructor.
data ListInstanceStorageConfigsResponse = ListInstanceStorageConfigsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | A valid storage type.
    storageConfigs :: Core.Maybe [InstanceStorageConfig],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInstanceStorageConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceStorageConfigsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'storageConfigs', 'listInstanceStorageConfigsResponse_storageConfigs' - A valid storage type.
--
-- 'httpStatus', 'listInstanceStorageConfigsResponse_httpStatus' - The response's http status code.
newListInstanceStorageConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInstanceStorageConfigsResponse
newListInstanceStorageConfigsResponse pHttpStatus_ =
  ListInstanceStorageConfigsResponse'
    { nextToken =
        Core.Nothing,
      storageConfigs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listInstanceStorageConfigsResponse_nextToken :: Lens.Lens' ListInstanceStorageConfigsResponse (Core.Maybe Core.Text)
listInstanceStorageConfigsResponse_nextToken = Lens.lens (\ListInstanceStorageConfigsResponse' {nextToken} -> nextToken) (\s@ListInstanceStorageConfigsResponse' {} a -> s {nextToken = a} :: ListInstanceStorageConfigsResponse)

-- | A valid storage type.
listInstanceStorageConfigsResponse_storageConfigs :: Lens.Lens' ListInstanceStorageConfigsResponse (Core.Maybe [InstanceStorageConfig])
listInstanceStorageConfigsResponse_storageConfigs = Lens.lens (\ListInstanceStorageConfigsResponse' {storageConfigs} -> storageConfigs) (\s@ListInstanceStorageConfigsResponse' {} a -> s {storageConfigs = a} :: ListInstanceStorageConfigsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInstanceStorageConfigsResponse_httpStatus :: Lens.Lens' ListInstanceStorageConfigsResponse Core.Int
listInstanceStorageConfigsResponse_httpStatus = Lens.lens (\ListInstanceStorageConfigsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceStorageConfigsResponse' {} a -> s {httpStatus = a} :: ListInstanceStorageConfigsResponse)

instance
  Core.NFData
    ListInstanceStorageConfigsResponse
