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
-- Module      : Amazonka.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListSubscriptionDefinitionVersions
  ( -- * Creating a Request
    ListSubscriptionDefinitionVersions (..),
    newListSubscriptionDefinitionVersions,

    -- * Request Lenses
    listSubscriptionDefinitionVersions_nextToken,
    listSubscriptionDefinitionVersions_maxResults,
    listSubscriptionDefinitionVersions_subscriptionDefinitionId,

    -- * Destructuring the Response
    ListSubscriptionDefinitionVersionsResponse (..),
    newListSubscriptionDefinitionVersionsResponse,

    -- * Response Lenses
    listSubscriptionDefinitionVersionsResponse_nextToken,
    listSubscriptionDefinitionVersionsResponse_versions,
    listSubscriptionDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listSubscriptionDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'subscriptionDefinitionId', 'listSubscriptionDefinitionVersions_subscriptionDefinitionId' - The ID of the subscription definition.
newListSubscriptionDefinitionVersions ::
  -- | 'subscriptionDefinitionId'
  Prelude.Text ->
  ListSubscriptionDefinitionVersions
newListSubscriptionDefinitionVersions
  pSubscriptionDefinitionId_ =
    ListSubscriptionDefinitionVersions'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionVersions_nextToken :: Lens.Lens' ListSubscriptionDefinitionVersions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitionVersions_nextToken = Lens.lens (\ListSubscriptionDefinitionVersions' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionVersions' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionVersions)

-- | The maximum number of results to be returned per request.
listSubscriptionDefinitionVersions_maxResults :: Lens.Lens' ListSubscriptionDefinitionVersions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitionVersions_maxResults = Lens.lens (\ListSubscriptionDefinitionVersions' {maxResults} -> maxResults) (\s@ListSubscriptionDefinitionVersions' {} a -> s {maxResults = a} :: ListSubscriptionDefinitionVersions)

-- | The ID of the subscription definition.
listSubscriptionDefinitionVersions_subscriptionDefinitionId :: Lens.Lens' ListSubscriptionDefinitionVersions Prelude.Text
listSubscriptionDefinitionVersions_subscriptionDefinitionId = Lens.lens (\ListSubscriptionDefinitionVersions' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@ListSubscriptionDefinitionVersions' {} a -> s {subscriptionDefinitionId = a} :: ListSubscriptionDefinitionVersions)

instance
  Core.AWSPager
    ListSubscriptionDefinitionVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscriptionDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListSubscriptionDefinitionVersions
  where
  type
    AWSResponse ListSubscriptionDefinitionVersions =
      ListSubscriptionDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSubscriptionDefinitionVersions
  where
  hashWithSalt
    _salt
    ListSubscriptionDefinitionVersions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` subscriptionDefinitionId

instance
  Prelude.NFData
    ListSubscriptionDefinitionVersions
  where
  rnf ListSubscriptionDefinitionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf subscriptionDefinitionId

instance
  Data.ToHeaders
    ListSubscriptionDefinitionVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListSubscriptionDefinitionVersions
  where
  toPath ListSubscriptionDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Data.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance
  Data.ToQuery
    ListSubscriptionDefinitionVersions
  where
  toQuery ListSubscriptionDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
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
-- Create a value of 'ListSubscriptionDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listSubscriptionDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listSubscriptionDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListSubscriptionDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscriptionDefinitionVersionsResponse
newListSubscriptionDefinitionVersionsResponse
  pHttpStatus_ =
    ListSubscriptionDefinitionVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        versions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionVersionsResponse_nextToken :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitionVersionsResponse_nextToken = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionVersionsResponse)

-- | Information about a version.
listSubscriptionDefinitionVersionsResponse_versions :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listSubscriptionDefinitionVersionsResponse_versions = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {versions} -> versions) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {versions = a} :: ListSubscriptionDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubscriptionDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse Prelude.Int
listSubscriptionDefinitionVersionsResponse_httpStatus = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListSubscriptionDefinitionVersionsResponse
  where
  rnf ListSubscriptionDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
