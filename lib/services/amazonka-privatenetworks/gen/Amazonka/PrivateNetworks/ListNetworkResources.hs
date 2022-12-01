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
-- Module      : Amazonka.PrivateNetworks.ListNetworkResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists network resources. Add filters to your request to return a more
-- specific list of results. Use filters to match the Amazon Resource Name
-- (ARN) of an order or the status of network resources.
--
-- If you specify multiple filters, filters are joined with an OR, and the
-- request returns results that match all of the specified filters.
--
-- This operation returns paginated results.
module Amazonka.PrivateNetworks.ListNetworkResources
  ( -- * Creating a Request
    ListNetworkResources (..),
    newListNetworkResources,

    -- * Request Lenses
    listNetworkResources_filters,
    listNetworkResources_maxResults,
    listNetworkResources_startToken,
    listNetworkResources_networkArn,

    -- * Destructuring the Response
    ListNetworkResourcesResponse (..),
    newListNetworkResourcesResponse,

    -- * Response Lenses
    listNetworkResourcesResponse_nextToken,
    listNetworkResourcesResponse_networkResources,
    listNetworkResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworkResources' smart constructor.
data ListNetworkResources = ListNetworkResources'
  { -- | The filters.
    --
    -- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
    --
    -- -   @STATUS@ - The status (@AVAILABLE@ | @DELETED@ | @DELETING@ |
    --     @PENDING@ | @PENDING_RETURN@ | @PROVISIONING@ | @SHIPPED@).
    --
    -- Filter values are case sensitive. If you specify multiple values for a
    -- filter, the values are joined with an @OR@, and the request returns all
    -- results that match any of the specified values.
    filters :: Prelude.Maybe (Prelude.HashMap NetworkResourceFilterKeys [Prelude.Text]),
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    startToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listNetworkResources_filters' - The filters.
--
-- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @DELETED@ | @DELETING@ |
--     @PENDING@ | @PENDING_RETURN@ | @PROVISIONING@ | @SHIPPED@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
--
-- 'maxResults', 'listNetworkResources_maxResults' - The maximum number of results to return.
--
-- 'startToken', 'listNetworkResources_startToken' - The token for the next page of results.
--
-- 'networkArn', 'listNetworkResources_networkArn' - The Amazon Resource Name (ARN) of the network.
newListNetworkResources ::
  -- | 'networkArn'
  Prelude.Text ->
  ListNetworkResources
newListNetworkResources pNetworkArn_ =
  ListNetworkResources'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startToken = Prelude.Nothing,
      networkArn = pNetworkArn_
    }

-- | The filters.
--
-- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @DELETED@ | @DELETING@ |
--     @PENDING@ | @PENDING_RETURN@ | @PROVISIONING@ | @SHIPPED@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
listNetworkResources_filters :: Lens.Lens' ListNetworkResources (Prelude.Maybe (Prelude.HashMap NetworkResourceFilterKeys [Prelude.Text]))
listNetworkResources_filters = Lens.lens (\ListNetworkResources' {filters} -> filters) (\s@ListNetworkResources' {} a -> s {filters = a} :: ListNetworkResources) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
listNetworkResources_maxResults :: Lens.Lens' ListNetworkResources (Prelude.Maybe Prelude.Natural)
listNetworkResources_maxResults = Lens.lens (\ListNetworkResources' {maxResults} -> maxResults) (\s@ListNetworkResources' {} a -> s {maxResults = a} :: ListNetworkResources)

-- | The token for the next page of results.
listNetworkResources_startToken :: Lens.Lens' ListNetworkResources (Prelude.Maybe Prelude.Text)
listNetworkResources_startToken = Lens.lens (\ListNetworkResources' {startToken} -> startToken) (\s@ListNetworkResources' {} a -> s {startToken = a} :: ListNetworkResources)

-- | The Amazon Resource Name (ARN) of the network.
listNetworkResources_networkArn :: Lens.Lens' ListNetworkResources Prelude.Text
listNetworkResources_networkArn = Lens.lens (\ListNetworkResources' {networkArn} -> networkArn) (\s@ListNetworkResources' {} a -> s {networkArn = a} :: ListNetworkResources)

instance Core.AWSPager ListNetworkResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNetworkResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNetworkResourcesResponse_networkResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNetworkResources_startToken
          Lens..~ rs
          Lens.^? listNetworkResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListNetworkResources where
  type
    AWSResponse ListNetworkResources =
      ListNetworkResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkResourcesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "networkResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworkResources where
  hashWithSalt _salt ListNetworkResources' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startToken
      `Prelude.hashWithSalt` networkArn

instance Prelude.NFData ListNetworkResources where
  rnf ListNetworkResources' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startToken
      `Prelude.seq` Prelude.rnf networkArn

instance Core.ToHeaders ListNetworkResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListNetworkResources where
  toJSON ListNetworkResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("startToken" Core..=) Prelude.<$> startToken,
            Prelude.Just ("networkArn" Core..= networkArn)
          ]
      )

instance Core.ToPath ListNetworkResources where
  toPath = Prelude.const "/v1/network-resources"

instance Core.ToQuery ListNetworkResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNetworkResourcesResponse' smart constructor.
data ListNetworkResourcesResponse = ListNetworkResourcesResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about network resources.
    networkResources :: Prelude.Maybe [NetworkResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkResourcesResponse_nextToken' - The token for the next page of results.
--
-- 'networkResources', 'listNetworkResourcesResponse_networkResources' - Information about network resources.
--
-- 'httpStatus', 'listNetworkResourcesResponse_httpStatus' - The response's http status code.
newListNetworkResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworkResourcesResponse
newListNetworkResourcesResponse pHttpStatus_ =
  ListNetworkResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      networkResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listNetworkResourcesResponse_nextToken :: Lens.Lens' ListNetworkResourcesResponse (Prelude.Maybe Prelude.Text)
listNetworkResourcesResponse_nextToken = Lens.lens (\ListNetworkResourcesResponse' {nextToken} -> nextToken) (\s@ListNetworkResourcesResponse' {} a -> s {nextToken = a} :: ListNetworkResourcesResponse)

-- | Information about network resources.
listNetworkResourcesResponse_networkResources :: Lens.Lens' ListNetworkResourcesResponse (Prelude.Maybe [NetworkResource])
listNetworkResourcesResponse_networkResources = Lens.lens (\ListNetworkResourcesResponse' {networkResources} -> networkResources) (\s@ListNetworkResourcesResponse' {} a -> s {networkResources = a} :: ListNetworkResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNetworkResourcesResponse_httpStatus :: Lens.Lens' ListNetworkResourcesResponse Prelude.Int
listNetworkResourcesResponse_httpStatus = Lens.lens (\ListNetworkResourcesResponse' {httpStatus} -> httpStatus) (\s@ListNetworkResourcesResponse' {} a -> s {httpStatus = a} :: ListNetworkResourcesResponse)

instance Prelude.NFData ListNetworkResourcesResponse where
  rnf ListNetworkResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkResources
      `Prelude.seq` Prelude.rnf httpStatus
