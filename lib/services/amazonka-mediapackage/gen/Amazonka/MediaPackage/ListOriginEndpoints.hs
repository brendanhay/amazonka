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
-- Module      : Amazonka.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Amazonka.MediaPackage.ListOriginEndpoints
  ( -- * Creating a Request
    ListOriginEndpoints (..),
    newListOriginEndpoints,

    -- * Request Lenses
    listOriginEndpoints_nextToken,
    listOriginEndpoints_maxResults,
    listOriginEndpoints_channelId,

    -- * Destructuring the Response
    ListOriginEndpointsResponse (..),
    newListOriginEndpointsResponse,

    -- * Response Lenses
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_originEndpoints,
    listOriginEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When specified, the request will return only OriginEndpoints associated
    -- with the given Channel ID.
    channelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOriginEndpoints_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listOriginEndpoints_maxResults' - The upper bound on the number of records to return.
--
-- 'channelId', 'listOriginEndpoints_channelId' - When specified, the request will return only OriginEndpoints associated
-- with the given Channel ID.
newListOriginEndpoints ::
  ListOriginEndpoints
newListOriginEndpoints =
  ListOriginEndpoints'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      channelId = Prelude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listOriginEndpoints_nextToken :: Lens.Lens' ListOriginEndpoints (Prelude.Maybe Prelude.Text)
listOriginEndpoints_nextToken = Lens.lens (\ListOriginEndpoints' {nextToken} -> nextToken) (\s@ListOriginEndpoints' {} a -> s {nextToken = a} :: ListOriginEndpoints)

-- | The upper bound on the number of records to return.
listOriginEndpoints_maxResults :: Lens.Lens' ListOriginEndpoints (Prelude.Maybe Prelude.Natural)
listOriginEndpoints_maxResults = Lens.lens (\ListOriginEndpoints' {maxResults} -> maxResults) (\s@ListOriginEndpoints' {} a -> s {maxResults = a} :: ListOriginEndpoints)

-- | When specified, the request will return only OriginEndpoints associated
-- with the given Channel ID.
listOriginEndpoints_channelId :: Lens.Lens' ListOriginEndpoints (Prelude.Maybe Prelude.Text)
listOriginEndpoints_channelId = Lens.lens (\ListOriginEndpoints' {channelId} -> channelId) (\s@ListOriginEndpoints' {} a -> s {channelId = a} :: ListOriginEndpoints)

instance Core.AWSPager ListOriginEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOriginEndpointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOriginEndpointsResponse_originEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOriginEndpoints_nextToken
          Lens..~ rs
          Lens.^? listOriginEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOriginEndpoints where
  type
    AWSResponse ListOriginEndpoints =
      ListOriginEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOriginEndpointsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "originEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOriginEndpoints where
  hashWithSalt _salt ListOriginEndpoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData ListOriginEndpoints where
  rnf ListOriginEndpoints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf channelId

instance Data.ToHeaders ListOriginEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListOriginEndpoints where
  toPath = Prelude.const "/origin_endpoints"

instance Data.ToQuery ListOriginEndpoints where
  toQuery ListOriginEndpoints' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "channelId" Data.=: channelId
      ]

-- | /See:/ 'newListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of OriginEndpoint records.
    originEndpoints :: Prelude.Maybe [OriginEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOriginEndpointsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'originEndpoints', 'listOriginEndpointsResponse_originEndpoints' - A list of OriginEndpoint records.
--
-- 'httpStatus', 'listOriginEndpointsResponse_httpStatus' - The response's http status code.
newListOriginEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOriginEndpointsResponse
newListOriginEndpointsResponse pHttpStatus_ =
  ListOriginEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      originEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to resume pagination from the end of the
-- collection.
listOriginEndpointsResponse_nextToken :: Lens.Lens' ListOriginEndpointsResponse (Prelude.Maybe Prelude.Text)
listOriginEndpointsResponse_nextToken = Lens.lens (\ListOriginEndpointsResponse' {nextToken} -> nextToken) (\s@ListOriginEndpointsResponse' {} a -> s {nextToken = a} :: ListOriginEndpointsResponse)

-- | A list of OriginEndpoint records.
listOriginEndpointsResponse_originEndpoints :: Lens.Lens' ListOriginEndpointsResponse (Prelude.Maybe [OriginEndpoint])
listOriginEndpointsResponse_originEndpoints = Lens.lens (\ListOriginEndpointsResponse' {originEndpoints} -> originEndpoints) (\s@ListOriginEndpointsResponse' {} a -> s {originEndpoints = a} :: ListOriginEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOriginEndpointsResponse_httpStatus :: Lens.Lens' ListOriginEndpointsResponse Prelude.Int
listOriginEndpointsResponse_httpStatus = Lens.lens (\ListOriginEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListOriginEndpointsResponse' {} a -> s {httpStatus = a} :: ListOriginEndpointsResponse)

instance Prelude.NFData ListOriginEndpointsResponse where
  rnf ListOriginEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf originEndpoints
      `Prelude.seq` Prelude.rnf httpStatus
