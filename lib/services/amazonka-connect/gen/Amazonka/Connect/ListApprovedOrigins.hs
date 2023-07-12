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
-- Module      : Amazonka.Connect.ListApprovedOrigins
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all approved origins associated with the
-- instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListApprovedOrigins
  ( -- * Creating a Request
    ListApprovedOrigins (..),
    newListApprovedOrigins,

    -- * Request Lenses
    listApprovedOrigins_maxResults,
    listApprovedOrigins_nextToken,
    listApprovedOrigins_instanceId,

    -- * Destructuring the Response
    ListApprovedOriginsResponse (..),
    newListApprovedOriginsResponse,

    -- * Response Lenses
    listApprovedOriginsResponse_nextToken,
    listApprovedOriginsResponse_origins,
    listApprovedOriginsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApprovedOrigins' smart constructor.
data ListApprovedOrigins = ListApprovedOrigins'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApprovedOrigins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listApprovedOrigins_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listApprovedOrigins_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listApprovedOrigins_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListApprovedOrigins ::
  -- | 'instanceId'
  Prelude.Text ->
  ListApprovedOrigins
newListApprovedOrigins pInstanceId_ =
  ListApprovedOrigins'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
listApprovedOrigins_maxResults :: Lens.Lens' ListApprovedOrigins (Prelude.Maybe Prelude.Natural)
listApprovedOrigins_maxResults = Lens.lens (\ListApprovedOrigins' {maxResults} -> maxResults) (\s@ListApprovedOrigins' {} a -> s {maxResults = a} :: ListApprovedOrigins)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listApprovedOrigins_nextToken :: Lens.Lens' ListApprovedOrigins (Prelude.Maybe Prelude.Text)
listApprovedOrigins_nextToken = Lens.lens (\ListApprovedOrigins' {nextToken} -> nextToken) (\s@ListApprovedOrigins' {} a -> s {nextToken = a} :: ListApprovedOrigins)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listApprovedOrigins_instanceId :: Lens.Lens' ListApprovedOrigins Prelude.Text
listApprovedOrigins_instanceId = Lens.lens (\ListApprovedOrigins' {instanceId} -> instanceId) (\s@ListApprovedOrigins' {} a -> s {instanceId = a} :: ListApprovedOrigins)

instance Core.AWSPager ListApprovedOrigins where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApprovedOriginsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApprovedOriginsResponse_origins
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApprovedOrigins_nextToken
          Lens..~ rs
          Lens.^? listApprovedOriginsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApprovedOrigins where
  type
    AWSResponse ListApprovedOrigins =
      ListApprovedOriginsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApprovedOriginsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Origins" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApprovedOrigins where
  hashWithSalt _salt ListApprovedOrigins' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListApprovedOrigins where
  rnf ListApprovedOrigins' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListApprovedOrigins where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApprovedOrigins where
  toPath ListApprovedOrigins' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/approved-origins"
      ]

instance Data.ToQuery ListApprovedOrigins where
  toQuery ListApprovedOrigins' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListApprovedOriginsResponse' smart constructor.
data ListApprovedOriginsResponse = ListApprovedOriginsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The approved origins.
    origins :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApprovedOriginsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApprovedOriginsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'origins', 'listApprovedOriginsResponse_origins' - The approved origins.
--
-- 'httpStatus', 'listApprovedOriginsResponse_httpStatus' - The response's http status code.
newListApprovedOriginsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApprovedOriginsResponse
newListApprovedOriginsResponse pHttpStatus_ =
  ListApprovedOriginsResponse'
    { nextToken =
        Prelude.Nothing,
      origins = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listApprovedOriginsResponse_nextToken :: Lens.Lens' ListApprovedOriginsResponse (Prelude.Maybe Prelude.Text)
listApprovedOriginsResponse_nextToken = Lens.lens (\ListApprovedOriginsResponse' {nextToken} -> nextToken) (\s@ListApprovedOriginsResponse' {} a -> s {nextToken = a} :: ListApprovedOriginsResponse)

-- | The approved origins.
listApprovedOriginsResponse_origins :: Lens.Lens' ListApprovedOriginsResponse (Prelude.Maybe [Prelude.Text])
listApprovedOriginsResponse_origins = Lens.lens (\ListApprovedOriginsResponse' {origins} -> origins) (\s@ListApprovedOriginsResponse' {} a -> s {origins = a} :: ListApprovedOriginsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApprovedOriginsResponse_httpStatus :: Lens.Lens' ListApprovedOriginsResponse Prelude.Int
listApprovedOriginsResponse_httpStatus = Lens.lens (\ListApprovedOriginsResponse' {httpStatus} -> httpStatus) (\s@ListApprovedOriginsResponse' {} a -> s {httpStatus = a} :: ListApprovedOriginsResponse)

instance Prelude.NFData ListApprovedOriginsResponse where
  rnf ListApprovedOriginsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf origins
      `Prelude.seq` Prelude.rnf httpStatus
