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
-- Module      : Amazonka.Comprehend.ListEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all existing endpoints that you\'ve created. For
-- information about endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_filter,
    listEndpoints_maxResults,
    listEndpoints_nextToken,

    -- * Destructuring the Response
    ListEndpointsResponse (..),
    newListEndpointsResponse,

    -- * Response Lenses
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | Filters the endpoints that are returned. You can filter endpoints on
    -- their name, model, status, or the date and time that they were created.
    -- You can only set one filter at a time.
    filter' :: Prelude.Maybe EndpointFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listEndpoints_filter' - Filters the endpoints that are returned. You can filter endpoints on
-- their name, model, status, or the date and time that they were created.
-- You can only set one filter at a time.
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listEndpoints_nextToken' - Identifies the next page of results to return.
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the endpoints that are returned. You can filter endpoints on
-- their name, model, status, or the date and time that they were created.
-- You can only set one filter at a time.
listEndpoints_filter :: Lens.Lens' ListEndpoints (Prelude.Maybe EndpointFilter)
listEndpoints_filter = Lens.lens (\ListEndpoints' {filter'} -> filter') (\s@ListEndpoints' {} a -> s {filter' = a} :: ListEndpoints)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | Identifies the next page of results to return.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

instance Core.AWSPager ListEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_endpointPropertiesList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEndpoints_nextToken
          Lens..~ rs
          Lens.^? listEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Prelude.<$> ( x
                            Data..?> "EndpointPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEndpoints where
  hashWithSalt _salt ListEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEndpoints where
  rnf ListEndpoints' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | Displays a list of endpoint properties being retrieved by the service in
    -- response to the request.
    endpointPropertiesList :: Prelude.Maybe [EndpointProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointPropertiesList', 'listEndpointsResponse_endpointPropertiesList' - Displays a list of endpoint properties being retrieved by the service in
-- response to the request.
--
-- 'nextToken', 'listEndpointsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { endpointPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Displays a list of endpoint properties being retrieved by the service in
-- response to the request.
listEndpointsResponse_endpointPropertiesList :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe [EndpointProperties])
listEndpointsResponse_endpointPropertiesList = Lens.lens (\ListEndpointsResponse' {endpointPropertiesList} -> endpointPropertiesList) (\s@ListEndpointsResponse' {} a -> s {endpointPropertiesList = a} :: ListEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe Prelude.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Prelude.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

instance Prelude.NFData ListEndpointsResponse where
  rnf ListEndpointsResponse' {..} =
    Prelude.rnf endpointPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
