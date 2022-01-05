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
-- Module      : Amazonka.Discovery.ListConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items as specified by the value passed
-- to the required parameter @configurationType@. Optional filtering may be
-- applied to refine search results.
--
-- This operation returns paginated results.
module Amazonka.Discovery.ListConfigurations
  ( -- * Creating a Request
    ListConfigurations (..),
    newListConfigurations,

    -- * Request Lenses
    listConfigurations_orderBy,
    listConfigurations_filters,
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurations_configurationType,

    -- * Destructuring the Response
    ListConfigurationsResponse (..),
    newListConfigurationsResponse,

    -- * Response Lenses
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Discovery.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { -- | Certain filter criteria return output that can be sorted in ascending or
    -- descending order. For a list of output characteristics for each filter,
    -- see
    -- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
    -- in the /AWS Application Discovery Service User Guide/.
    orderBy :: Prelude.Maybe [OrderByElement],
    -- | You can filter the request using various logical operators and a
    -- /key/-/value/ format. For example:
    --
    -- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
    --
    -- For a complete list of filter options and guidance about using them with
    -- this action, see
    -- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
    -- in the /AWS Application Discovery Service User Guide/.
    filters :: Prelude.Maybe [Filter],
    -- | Token to retrieve the next set of results. For example, if a previous
    -- call to ListConfigurations returned 100 items, but you set
    -- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
    -- results along with a token. Use that token in this query to get the next
    -- set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return. The maximum value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A valid configuration identified by Application Discovery Service.
    configurationType :: ConfigurationItemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderBy', 'listConfigurations_orderBy' - Certain filter criteria return output that can be sorted in ascending or
-- descending order. For a list of output characteristics for each filter,
-- see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
--
-- 'filters', 'listConfigurations_filters' - You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
--
-- For a complete list of filter options and guidance about using them with
-- this action, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
--
-- 'nextToken', 'listConfigurations_nextToken' - Token to retrieve the next set of results. For example, if a previous
-- call to ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
--
-- 'maxResults', 'listConfigurations_maxResults' - The total number of items to return. The maximum value is 100.
--
-- 'configurationType', 'listConfigurations_configurationType' - A valid configuration identified by Application Discovery Service.
newListConfigurations ::
  -- | 'configurationType'
  ConfigurationItemType ->
  ListConfigurations
newListConfigurations pConfigurationType_ =
  ListConfigurations'
    { orderBy = Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      configurationType = pConfigurationType_
    }

-- | Certain filter criteria return output that can be sorted in ascending or
-- descending order. For a list of output characteristics for each filter,
-- see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
listConfigurations_orderBy :: Lens.Lens' ListConfigurations (Prelude.Maybe [OrderByElement])
listConfigurations_orderBy = Lens.lens (\ListConfigurations' {orderBy} -> orderBy) (\s@ListConfigurations' {} a -> s {orderBy = a} :: ListConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
--
-- For a complete list of filter options and guidance about using them with
-- this action, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
listConfigurations_filters :: Lens.Lens' ListConfigurations (Prelude.Maybe [Filter])
listConfigurations_filters = Lens.lens (\ListConfigurations' {filters} -> filters) (\s@ListConfigurations' {} a -> s {filters = a} :: ListConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | Token to retrieve the next set of results. For example, if a previous
-- call to ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
listConfigurations_nextToken :: Lens.Lens' ListConfigurations (Prelude.Maybe Prelude.Text)
listConfigurations_nextToken = Lens.lens (\ListConfigurations' {nextToken} -> nextToken) (\s@ListConfigurations' {} a -> s {nextToken = a} :: ListConfigurations)

-- | The total number of items to return. The maximum value is 100.
listConfigurations_maxResults :: Lens.Lens' ListConfigurations (Prelude.Maybe Prelude.Int)
listConfigurations_maxResults = Lens.lens (\ListConfigurations' {maxResults} -> maxResults) (\s@ListConfigurations' {} a -> s {maxResults = a} :: ListConfigurations)

-- | A valid configuration identified by Application Discovery Service.
listConfigurations_configurationType :: Lens.Lens' ListConfigurations ConfigurationItemType
listConfigurations_configurationType = Lens.lens (\ListConfigurations' {configurationType} -> configurationType) (\s@ListConfigurations' {} a -> s {configurationType = a} :: ListConfigurations)

instance Core.AWSPager ListConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationsResponse_configurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConfigurations_nextToken
          Lens..~ rs
          Lens.^? listConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConfigurations where
  type
    AWSResponse ListConfigurations =
      ListConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationsResponse'
            Prelude.<$> (x Core..?> "configurations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurations where
  hashWithSalt _salt ListConfigurations' {..} =
    _salt `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` configurationType

instance Prelude.NFData ListConfigurations where
  rnf ListConfigurations' {..} =
    Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf configurationType

instance Core.ToHeaders ListConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.ListConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListConfigurations where
  toJSON ListConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("orderBy" Core..=) Prelude.<$> orderBy,
            ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("configurationType" Core..= configurationType)
          ]
      )

instance Core.ToPath ListConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { -- | Returns configuration details, including the configuration ID, attribute
    -- names, and attribute values.
    configurations :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | Token to retrieve the next set of results. For example, if your call to
    -- ListConfigurations returned 100 items, but you set
    -- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
    -- results along with this token. Use this token in the next query to
    -- retrieve the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'listConfigurationsResponse_configurations' - Returns configuration details, including the configuration ID, attribute
-- names, and attribute values.
--
-- 'nextToken', 'listConfigurationsResponse_nextToken' - Token to retrieve the next set of results. For example, if your call to
-- ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
--
-- 'httpStatus', 'listConfigurationsResponse_httpStatus' - The response's http status code.
newListConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationsResponse
newListConfigurationsResponse pHttpStatus_ =
  ListConfigurationsResponse'
    { configurations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns configuration details, including the configuration ID, attribute
-- names, and attribute values.
listConfigurationsResponse_configurations :: Lens.Lens' ListConfigurationsResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
listConfigurationsResponse_configurations = Lens.lens (\ListConfigurationsResponse' {configurations} -> configurations) (\s@ListConfigurationsResponse' {} a -> s {configurations = a} :: ListConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token to retrieve the next set of results. For example, if your call to
-- ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
listConfigurationsResponse_nextToken :: Lens.Lens' ListConfigurationsResponse (Prelude.Maybe Prelude.Text)
listConfigurationsResponse_nextToken = Lens.lens (\ListConfigurationsResponse' {nextToken} -> nextToken) (\s@ListConfigurationsResponse' {} a -> s {nextToken = a} :: ListConfigurationsResponse)

-- | The response's http status code.
listConfigurationsResponse_httpStatus :: Lens.Lens' ListConfigurationsResponse Prelude.Int
listConfigurationsResponse_httpStatus = Lens.lens (\ListConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationsResponse' {} a -> s {httpStatus = a} :: ListConfigurationsResponse)

instance Prelude.NFData ListConfigurationsResponse where
  rnf ListConfigurationsResponse' {..} =
    Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
