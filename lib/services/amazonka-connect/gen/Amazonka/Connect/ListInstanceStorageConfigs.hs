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
-- Module      : Amazonka.Connect.ListInstanceStorageConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Connect.ListInstanceStorageConfigs
  ( -- * Creating a Request
    ListInstanceStorageConfigs (..),
    newListInstanceStorageConfigs,

    -- * Request Lenses
    listInstanceStorageConfigs_maxResults,
    listInstanceStorageConfigs_nextToken,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceStorageConfigs' smart constructor.
data ListInstanceStorageConfigs = ListInstanceStorageConfigs'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceStorageConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInstanceStorageConfigs_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listInstanceStorageConfigs_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listInstanceStorageConfigs_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'resourceType', 'listInstanceStorageConfigs_resourceType' - A valid resource type.
newListInstanceStorageConfigs ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  ListInstanceStorageConfigs
newListInstanceStorageConfigs
  pInstanceId_
  pResourceType_ =
    ListInstanceStorageConfigs'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        resourceType = pResourceType_
      }

-- | The maximum number of results to return per page.
listInstanceStorageConfigs_maxResults :: Lens.Lens' ListInstanceStorageConfigs (Prelude.Maybe Prelude.Natural)
listInstanceStorageConfigs_maxResults = Lens.lens (\ListInstanceStorageConfigs' {maxResults} -> maxResults) (\s@ListInstanceStorageConfigs' {} a -> s {maxResults = a} :: ListInstanceStorageConfigs)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listInstanceStorageConfigs_nextToken :: Lens.Lens' ListInstanceStorageConfigs (Prelude.Maybe Prelude.Text)
listInstanceStorageConfigs_nextToken = Lens.lens (\ListInstanceStorageConfigs' {nextToken} -> nextToken) (\s@ListInstanceStorageConfigs' {} a -> s {nextToken = a} :: ListInstanceStorageConfigs)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listInstanceStorageConfigs_instanceId :: Lens.Lens' ListInstanceStorageConfigs Prelude.Text
listInstanceStorageConfigs_instanceId = Lens.lens (\ListInstanceStorageConfigs' {instanceId} -> instanceId) (\s@ListInstanceStorageConfigs' {} a -> s {instanceId = a} :: ListInstanceStorageConfigs)

-- | A valid resource type.
listInstanceStorageConfigs_resourceType :: Lens.Lens' ListInstanceStorageConfigs InstanceStorageResourceType
listInstanceStorageConfigs_resourceType = Lens.lens (\ListInstanceStorageConfigs' {resourceType} -> resourceType) (\s@ListInstanceStorageConfigs' {} a -> s {resourceType = a} :: ListInstanceStorageConfigs)

instance Core.AWSPager ListInstanceStorageConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceStorageConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceStorageConfigsResponse_storageConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceStorageConfigs_nextToken
          Lens..~ rs
          Lens.^? listInstanceStorageConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceStorageConfigs where
  type
    AWSResponse ListInstanceStorageConfigs =
      ListInstanceStorageConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceStorageConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StorageConfigs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceStorageConfigs where
  hashWithSalt _salt ListInstanceStorageConfigs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListInstanceStorageConfigs where
  rnf ListInstanceStorageConfigs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders ListInstanceStorageConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListInstanceStorageConfigs where
  toPath ListInstanceStorageConfigs' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/storage-configs"
      ]

instance Data.ToQuery ListInstanceStorageConfigs where
  toQuery ListInstanceStorageConfigs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "resourceType" Data.=: resourceType
      ]

-- | /See:/ 'newListInstanceStorageConfigsResponse' smart constructor.
data ListInstanceStorageConfigsResponse = ListInstanceStorageConfigsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A valid storage type.
    storageConfigs :: Prelude.Maybe [InstanceStorageConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListInstanceStorageConfigsResponse
newListInstanceStorageConfigsResponse pHttpStatus_ =
  ListInstanceStorageConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      storageConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listInstanceStorageConfigsResponse_nextToken :: Lens.Lens' ListInstanceStorageConfigsResponse (Prelude.Maybe Prelude.Text)
listInstanceStorageConfigsResponse_nextToken = Lens.lens (\ListInstanceStorageConfigsResponse' {nextToken} -> nextToken) (\s@ListInstanceStorageConfigsResponse' {} a -> s {nextToken = a} :: ListInstanceStorageConfigsResponse)

-- | A valid storage type.
listInstanceStorageConfigsResponse_storageConfigs :: Lens.Lens' ListInstanceStorageConfigsResponse (Prelude.Maybe [InstanceStorageConfig])
listInstanceStorageConfigsResponse_storageConfigs = Lens.lens (\ListInstanceStorageConfigsResponse' {storageConfigs} -> storageConfigs) (\s@ListInstanceStorageConfigsResponse' {} a -> s {storageConfigs = a} :: ListInstanceStorageConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstanceStorageConfigsResponse_httpStatus :: Lens.Lens' ListInstanceStorageConfigsResponse Prelude.Int
listInstanceStorageConfigsResponse_httpStatus = Lens.lens (\ListInstanceStorageConfigsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceStorageConfigsResponse' {} a -> s {httpStatus = a} :: ListInstanceStorageConfigsResponse)

instance
  Prelude.NFData
    ListInstanceStorageConfigsResponse
  where
  rnf ListInstanceStorageConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storageConfigs
      `Prelude.seq` Prelude.rnf httpStatus
