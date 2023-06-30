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
-- Module      : Amazonka.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListDeviceDefinitionVersions
  ( -- * Creating a Request
    ListDeviceDefinitionVersions (..),
    newListDeviceDefinitionVersions,

    -- * Request Lenses
    listDeviceDefinitionVersions_maxResults,
    listDeviceDefinitionVersions_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeviceDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDeviceDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'deviceDefinitionId', 'listDeviceDefinitionVersions_deviceDefinitionId' - The ID of the device definition.
newListDeviceDefinitionVersions ::
  -- | 'deviceDefinitionId'
  Prelude.Text ->
  ListDeviceDefinitionVersions
newListDeviceDefinitionVersions pDeviceDefinitionId_ =
  ListDeviceDefinitionVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The maximum number of results to be returned per request.
listDeviceDefinitionVersions_maxResults :: Lens.Lens' ListDeviceDefinitionVersions (Prelude.Maybe Prelude.Text)
listDeviceDefinitionVersions_maxResults = Lens.lens (\ListDeviceDefinitionVersions' {maxResults} -> maxResults) (\s@ListDeviceDefinitionVersions' {} a -> s {maxResults = a} :: ListDeviceDefinitionVersions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionVersions_nextToken :: Lens.Lens' ListDeviceDefinitionVersions (Prelude.Maybe Prelude.Text)
listDeviceDefinitionVersions_nextToken = Lens.lens (\ListDeviceDefinitionVersions' {nextToken} -> nextToken) (\s@ListDeviceDefinitionVersions' {} a -> s {nextToken = a} :: ListDeviceDefinitionVersions)

-- | The ID of the device definition.
listDeviceDefinitionVersions_deviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Prelude.Text
listDeviceDefinitionVersions_deviceDefinitionId = Lens.lens (\ListDeviceDefinitionVersions' {deviceDefinitionId} -> deviceDefinitionId) (\s@ListDeviceDefinitionVersions' {} a -> s {deviceDefinitionId = a} :: ListDeviceDefinitionVersions)

instance Core.AWSPager ListDeviceDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionVersionsResponse_versions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDeviceDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listDeviceDefinitionVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceDefinitionVersions where
  type
    AWSResponse ListDeviceDefinitionVersions =
      ListDeviceDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDeviceDefinitionVersions
  where
  hashWithSalt _salt ListDeviceDefinitionVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` deviceDefinitionId

instance Prelude.NFData ListDeviceDefinitionVersions where
  rnf ListDeviceDefinitionVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deviceDefinitionId

instance Data.ToHeaders ListDeviceDefinitionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeviceDefinitionVersions where
  toPath ListDeviceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Data.toBS deviceDefinitionId,
        "/versions"
      ]

instance Data.ToQuery ListDeviceDefinitionVersions where
  toQuery ListDeviceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listDeviceDefinitionVersionsResponse_versions = Lens.lens (\ListDeviceDefinitionVersionsResponse' {versions} -> versions) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {versions = a} :: ListDeviceDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDeviceDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Prelude.Int
listDeviceDefinitionVersionsResponse_httpStatus = Lens.lens (\ListDeviceDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListDeviceDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListDeviceDefinitionVersionsResponse
  where
  rnf ListDeviceDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
