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
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity
-- has access only to its own data. Thus, the credentials used to make this
-- API call need to have access to the identity data.
--
-- ListDatasets can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use the
-- Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_identityId,
    listDatasets_identityPoolId,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_count,
    listDatasetsResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned.
    maxResults :: Core.Maybe Core.Int,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Core.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatasets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasets_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'maxResults', 'listDatasets_maxResults' - The maximum number of results to be returned.
--
-- 'identityId', 'listDatasets_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityPoolId', 'listDatasets_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newListDatasets ::
  -- | 'identityId'
  Core.Text ->
  -- | 'identityPoolId'
  Core.Text ->
  ListDatasets
newListDatasets pIdentityId_ pIdentityPoolId_ =
  ListDatasets'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      identityId = pIdentityId_,
      identityPoolId = pIdentityPoolId_
    }

-- | A pagination token for obtaining the next page of results.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Core.Maybe Core.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

-- | The maximum number of results to be returned.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Core.Maybe Core.Int)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
listDatasets_identityId :: Lens.Lens' ListDatasets Core.Text
listDatasets_identityId = Lens.lens (\ListDatasets' {identityId} -> identityId) (\s@ListDatasets' {} a -> s {identityId = a} :: ListDatasets)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
listDatasets_identityPoolId :: Lens.Lens' ListDatasets Core.Text
listDatasets_identityPoolId = Lens.lens (\ListDatasets' {identityPoolId} -> identityPoolId) (\s@ListDatasets' {} a -> s {identityPoolId = a} :: ListDatasets)

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Datasets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Count")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDatasets

instance Core.NFData ListDatasets

instance Core.ToHeaders ListDatasets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDatasets where
  toPath ListDatasets' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/identities/",
        Core.toBS identityId,
        "/datasets"
      ]

instance Core.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A set of datasets.
    datasets :: Core.Maybe [Dataset],
    -- | Number of datasets returned.
    count :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'datasets', 'listDatasetsResponse_datasets' - A set of datasets.
--
-- 'count', 'listDatasetsResponse_count' - Number of datasets returned.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { nextToken = Core.Nothing,
      datasets = Core.Nothing,
      count = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token for obtaining the next page of results.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | A set of datasets.
listDatasetsResponse_datasets :: Lens.Lens' ListDatasetsResponse (Core.Maybe [Dataset])
listDatasetsResponse_datasets = Lens.lens (\ListDatasetsResponse' {datasets} -> datasets) (\s@ListDatasetsResponse' {} a -> s {datasets = a} :: ListDatasetsResponse) Core.. Lens.mapping Lens._Coerce

-- | Number of datasets returned.
listDatasetsResponse_count :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Int)
listDatasetsResponse_count = Lens.lens (\ListDatasetsResponse' {count} -> count) (\s@ListDatasetsResponse' {} a -> s {count = a} :: ListDatasetsResponse)

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Core.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Core.NFData ListDatasetsResponse
