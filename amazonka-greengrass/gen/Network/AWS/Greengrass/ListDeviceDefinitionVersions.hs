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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListDeviceDefinitionVersions
newListDeviceDefinitionVersions pDeviceDefinitionId_ =
  ListDeviceDefinitionVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionVersions_nextToken :: Lens.Lens' ListDeviceDefinitionVersions (Prelude.Maybe Prelude.Text)
listDeviceDefinitionVersions_nextToken = Lens.lens (\ListDeviceDefinitionVersions' {nextToken} -> nextToken) (\s@ListDeviceDefinitionVersions' {} a -> s {nextToken = a} :: ListDeviceDefinitionVersions)

-- | The maximum number of results to be returned per request.
listDeviceDefinitionVersions_maxResults :: Lens.Lens' ListDeviceDefinitionVersions (Prelude.Maybe Prelude.Text)
listDeviceDefinitionVersions_maxResults = Lens.lens (\ListDeviceDefinitionVersions' {maxResults} -> maxResults) (\s@ListDeviceDefinitionVersions' {} a -> s {maxResults = a} :: ListDeviceDefinitionVersions)

-- | The ID of the device definition.
listDeviceDefinitionVersions_deviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Prelude.Text
listDeviceDefinitionVersions_deviceDefinitionId = Lens.lens (\ListDeviceDefinitionVersions' {deviceDefinitionId} -> deviceDefinitionId) (\s@ListDeviceDefinitionVersions' {} a -> s {deviceDefinitionId = a} :: ListDeviceDefinitionVersions)

instance Pager.AWSPager ListDeviceDefinitionVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeviceDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listDeviceDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListDeviceDefinitionVersions
  where
  type
    Rs ListDeviceDefinitionVersions =
      ListDeviceDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionVersionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Versions" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDeviceDefinitionVersions

instance Prelude.NFData ListDeviceDefinitionVersions

instance
  Prelude.ToHeaders
    ListDeviceDefinitionVersions
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

instance Prelude.ToPath ListDeviceDefinitionVersions where
  toPath ListDeviceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Prelude.toBS deviceDefinitionId,
        "/versions"
      ]

instance Prelude.ToQuery ListDeviceDefinitionVersions where
  toQuery ListDeviceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a version.
    versions :: Prelude.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListDeviceDefinitionVersionsResponse
newListDeviceDefinitionVersionsResponse pHttpStatus_ =
  ListDeviceDefinitionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionVersionsResponse_nextToken :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listDeviceDefinitionVersionsResponse_nextToken = Lens.lens (\ListDeviceDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListDeviceDefinitionVersionsResponse)

-- | Information about a version.
listDeviceDefinitionVersionsResponse_versions :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listDeviceDefinitionVersionsResponse_versions = Lens.lens (\ListDeviceDefinitionVersionsResponse' {versions} -> versions) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {versions = a} :: ListDeviceDefinitionVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDeviceDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Prelude.Int
listDeviceDefinitionVersionsResponse_httpStatus = Lens.lens (\ListDeviceDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListDeviceDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListDeviceDefinitionVersionsResponse
