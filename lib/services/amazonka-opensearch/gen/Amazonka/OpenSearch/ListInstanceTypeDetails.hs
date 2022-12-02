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
-- Module      : Amazonka.OpenSearch.ListInstanceTypeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all instance types and available features for a given OpenSearch
-- or Elasticsearch version.
module Amazonka.OpenSearch.ListInstanceTypeDetails
  ( -- * Creating a Request
    ListInstanceTypeDetails (..),
    newListInstanceTypeDetails,

    -- * Request Lenses
    listInstanceTypeDetails_nextToken,
    listInstanceTypeDetails_domainName,
    listInstanceTypeDetails_maxResults,
    listInstanceTypeDetails_engineVersion,

    -- * Destructuring the Response
    ListInstanceTypeDetailsResponse (..),
    newListInstanceTypeDetailsResponse,

    -- * Response Lenses
    listInstanceTypeDetailsResponse_nextToken,
    listInstanceTypeDetailsResponse_instanceTypeDetails,
    listInstanceTypeDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceTypeDetails' smart constructor.
data ListInstanceTypeDetails = ListInstanceTypeDetails'
  { -- | If your initial @ListInstanceTypeDetails@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @ListInstanceTypeDetails@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of the domain to list instance type details for.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
    -- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
    engineVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceTypeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceTypeDetails_nextToken' - If your initial @ListInstanceTypeDetails@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @ListInstanceTypeDetails@ operations, which returns results in the next
-- page.
--
-- 'domainName', 'listInstanceTypeDetails_domainName' - Name of the domain to list instance type details for.
--
-- 'maxResults', 'listInstanceTypeDetails_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'engineVersion', 'listInstanceTypeDetails_engineVersion' - Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
-- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
newListInstanceTypeDetails ::
  -- | 'engineVersion'
  Prelude.Text ->
  ListInstanceTypeDetails
newListInstanceTypeDetails pEngineVersion_ =
  ListInstanceTypeDetails'
    { nextToken =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      engineVersion = pEngineVersion_
    }

-- | If your initial @ListInstanceTypeDetails@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @ListInstanceTypeDetails@ operations, which returns results in the next
-- page.
listInstanceTypeDetails_nextToken :: Lens.Lens' ListInstanceTypeDetails (Prelude.Maybe Prelude.Text)
listInstanceTypeDetails_nextToken = Lens.lens (\ListInstanceTypeDetails' {nextToken} -> nextToken) (\s@ListInstanceTypeDetails' {} a -> s {nextToken = a} :: ListInstanceTypeDetails)

-- | Name of the domain to list instance type details for.
listInstanceTypeDetails_domainName :: Lens.Lens' ListInstanceTypeDetails (Prelude.Maybe Prelude.Text)
listInstanceTypeDetails_domainName = Lens.lens (\ListInstanceTypeDetails' {domainName} -> domainName) (\s@ListInstanceTypeDetails' {} a -> s {domainName = a} :: ListInstanceTypeDetails)

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listInstanceTypeDetails_maxResults :: Lens.Lens' ListInstanceTypeDetails (Prelude.Maybe Prelude.Int)
listInstanceTypeDetails_maxResults = Lens.lens (\ListInstanceTypeDetails' {maxResults} -> maxResults) (\s@ListInstanceTypeDetails' {} a -> s {maxResults = a} :: ListInstanceTypeDetails)

-- | Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
-- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
listInstanceTypeDetails_engineVersion :: Lens.Lens' ListInstanceTypeDetails Prelude.Text
listInstanceTypeDetails_engineVersion = Lens.lens (\ListInstanceTypeDetails' {engineVersion} -> engineVersion) (\s@ListInstanceTypeDetails' {} a -> s {engineVersion = a} :: ListInstanceTypeDetails)

instance Core.AWSRequest ListInstanceTypeDetails where
  type
    AWSResponse ListInstanceTypeDetails =
      ListInstanceTypeDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceTypeDetailsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "InstanceTypeDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceTypeDetails where
  hashWithSalt _salt ListInstanceTypeDetails' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData ListInstanceTypeDetails where
  rnf ListInstanceTypeDetails' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf engineVersion

instance Data.ToHeaders ListInstanceTypeDetails where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListInstanceTypeDetails where
  toPath ListInstanceTypeDetails' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/instanceTypeDetails/",
        Data.toBS engineVersion
      ]

instance Data.ToQuery ListInstanceTypeDetails where
  toQuery ListInstanceTypeDetails' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "domainName" Data.=: domainName,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListInstanceTypeDetailsResponse' smart constructor.
data ListInstanceTypeDetailsResponse = ListInstanceTypeDetailsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all supported instance types and features for the given OpenSearch
    -- or Elasticsearch version.
    instanceTypeDetails :: Prelude.Maybe [InstanceTypeDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceTypeDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceTypeDetailsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'instanceTypeDetails', 'listInstanceTypeDetailsResponse_instanceTypeDetails' - Lists all supported instance types and features for the given OpenSearch
-- or Elasticsearch version.
--
-- 'httpStatus', 'listInstanceTypeDetailsResponse_httpStatus' - The response's http status code.
newListInstanceTypeDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceTypeDetailsResponse
newListInstanceTypeDetailsResponse pHttpStatus_ =
  ListInstanceTypeDetailsResponse'
    { nextToken =
        Prelude.Nothing,
      instanceTypeDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listInstanceTypeDetailsResponse_nextToken :: Lens.Lens' ListInstanceTypeDetailsResponse (Prelude.Maybe Prelude.Text)
listInstanceTypeDetailsResponse_nextToken = Lens.lens (\ListInstanceTypeDetailsResponse' {nextToken} -> nextToken) (\s@ListInstanceTypeDetailsResponse' {} a -> s {nextToken = a} :: ListInstanceTypeDetailsResponse)

-- | Lists all supported instance types and features for the given OpenSearch
-- or Elasticsearch version.
listInstanceTypeDetailsResponse_instanceTypeDetails :: Lens.Lens' ListInstanceTypeDetailsResponse (Prelude.Maybe [InstanceTypeDetails])
listInstanceTypeDetailsResponse_instanceTypeDetails = Lens.lens (\ListInstanceTypeDetailsResponse' {instanceTypeDetails} -> instanceTypeDetails) (\s@ListInstanceTypeDetailsResponse' {} a -> s {instanceTypeDetails = a} :: ListInstanceTypeDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstanceTypeDetailsResponse_httpStatus :: Lens.Lens' ListInstanceTypeDetailsResponse Prelude.Int
listInstanceTypeDetailsResponse_httpStatus = Lens.lens (\ListInstanceTypeDetailsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceTypeDetailsResponse' {} a -> s {httpStatus = a} :: ListInstanceTypeDetailsResponse)

instance
  Prelude.NFData
    ListInstanceTypeDetailsResponse
  where
  rnf ListInstanceTypeDetailsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceTypeDetails
      `Prelude.seq` Prelude.rnf httpStatus
