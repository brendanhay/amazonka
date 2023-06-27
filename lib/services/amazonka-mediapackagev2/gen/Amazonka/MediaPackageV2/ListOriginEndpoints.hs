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
-- Module      : Amazonka.MediaPackageV2.ListOriginEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all origin endpoints in a specific channel that are configured
-- in AWS Elemental MediaPackage.
--
-- This operation returns paginated results.
module Amazonka.MediaPackageV2.ListOriginEndpoints
  ( -- * Creating a Request
    ListOriginEndpoints (..),
    newListOriginEndpoints,

    -- * Request Lenses
    listOriginEndpoints_maxResults,
    listOriginEndpoints_nextToken,
    listOriginEndpoints_channelGroupName,
    listOriginEndpoints_channelName,

    -- * Destructuring the Response
    ListOriginEndpointsResponse (..),
    newListOriginEndpointsResponse,

    -- * Response Lenses
    listOriginEndpointsResponse_items,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the GET list request. Use the token to fetch
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text
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
-- 'maxResults', 'listOriginEndpoints_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listOriginEndpoints_nextToken' - The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
--
-- 'channelGroupName', 'listOriginEndpoints_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'listOriginEndpoints_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newListOriginEndpoints ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  ListOriginEndpoints
newListOriginEndpoints
  pChannelGroupName_
  pChannelName_ =
    ListOriginEndpoints'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_
      }

-- | The maximum number of results to return in the response.
listOriginEndpoints_maxResults :: Lens.Lens' ListOriginEndpoints (Prelude.Maybe Prelude.Natural)
listOriginEndpoints_maxResults = Lens.lens (\ListOriginEndpoints' {maxResults} -> maxResults) (\s@ListOriginEndpoints' {} a -> s {maxResults = a} :: ListOriginEndpoints)

-- | The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
listOriginEndpoints_nextToken :: Lens.Lens' ListOriginEndpoints (Prelude.Maybe Prelude.Text)
listOriginEndpoints_nextToken = Lens.lens (\ListOriginEndpoints' {nextToken} -> nextToken) (\s@ListOriginEndpoints' {} a -> s {nextToken = a} :: ListOriginEndpoints)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
listOriginEndpoints_channelGroupName :: Lens.Lens' ListOriginEndpoints Prelude.Text
listOriginEndpoints_channelGroupName = Lens.lens (\ListOriginEndpoints' {channelGroupName} -> channelGroupName) (\s@ListOriginEndpoints' {} a -> s {channelGroupName = a} :: ListOriginEndpoints)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
listOriginEndpoints_channelName :: Lens.Lens' ListOriginEndpoints Prelude.Text
listOriginEndpoints_channelName = Lens.lens (\ListOriginEndpoints' {channelName} -> channelName) (\s@ListOriginEndpoints' {} a -> s {channelName = a} :: ListOriginEndpoints)

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
            Lens.^? listOriginEndpointsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOriginEndpoints where
  hashWithSalt _salt ListOriginEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData ListOriginEndpoints where
  rnf ListOriginEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

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
  toPath ListOriginEndpoints' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint"
      ]

instance Data.ToQuery ListOriginEndpoints where
  toQuery ListOriginEndpoints' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { -- | The objects being returned.
    items :: Prelude.Maybe [OriginEndpointListConfiguration],
    -- | The pagination token from the GET list request. Use the token to fetch
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'items', 'listOriginEndpointsResponse_items' - The objects being returned.
--
-- 'nextToken', 'listOriginEndpointsResponse_nextToken' - The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
--
-- 'httpStatus', 'listOriginEndpointsResponse_httpStatus' - The response's http status code.
newListOriginEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOriginEndpointsResponse
newListOriginEndpointsResponse pHttpStatus_ =
  ListOriginEndpointsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The objects being returned.
listOriginEndpointsResponse_items :: Lens.Lens' ListOriginEndpointsResponse (Prelude.Maybe [OriginEndpointListConfiguration])
listOriginEndpointsResponse_items = Lens.lens (\ListOriginEndpointsResponse' {items} -> items) (\s@ListOriginEndpointsResponse' {} a -> s {items = a} :: ListOriginEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
listOriginEndpointsResponse_nextToken :: Lens.Lens' ListOriginEndpointsResponse (Prelude.Maybe Prelude.Text)
listOriginEndpointsResponse_nextToken = Lens.lens (\ListOriginEndpointsResponse' {nextToken} -> nextToken) (\s@ListOriginEndpointsResponse' {} a -> s {nextToken = a} :: ListOriginEndpointsResponse)

-- | The response's http status code.
listOriginEndpointsResponse_httpStatus :: Lens.Lens' ListOriginEndpointsResponse Prelude.Int
listOriginEndpointsResponse_httpStatus = Lens.lens (\ListOriginEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListOriginEndpointsResponse' {} a -> s {httpStatus = a} :: ListOriginEndpointsResponse)

instance Prelude.NFData ListOriginEndpointsResponse where
  rnf ListOriginEndpointsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
