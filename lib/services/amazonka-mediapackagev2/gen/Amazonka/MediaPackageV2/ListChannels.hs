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
-- Module      : Amazonka.MediaPackageV2.ListChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all channels in a specific channel group that are configured
-- in AWS Elemental MediaPackage, including the origin endpoints that are
-- associated with it.
--
-- This operation returns paginated results.
module Amazonka.MediaPackageV2.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_maxResults,
    listChannels_nextToken,
    listChannels_channelGroupName,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_items,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the GET list request. Use the token to fetch
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listChannels_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listChannels_nextToken' - The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
--
-- 'channelGroupName', 'listChannels_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
newListChannels ::
  -- | 'channelGroupName'
  Prelude.Text ->
  ListChannels
newListChannels pChannelGroupName_ =
  ListChannels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      channelGroupName = pChannelGroupName_
    }

-- | The maximum number of results to return in the response.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

-- | The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
listChannels_channelGroupName :: Lens.Lens' ListChannels Prelude.Text
listChannels_channelGroupName = Lens.lens (\ListChannels' {channelGroupName} -> channelGroupName) (\s@ListChannels' {} a -> s {channelGroupName = a} :: ListChannels)

instance Core.AWSPager ListChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChannelsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listChannelsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listChannels_nextToken
          Lens..~ rs
          Lens.^? listChannelsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannels where
  hashWithSalt _salt ListChannels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelGroupName

instance Prelude.NFData ListChannels where
  rnf ListChannels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelGroupName

instance Data.ToHeaders ListChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListChannels where
  toPath ListChannels' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel"
      ]

instance Data.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | The objects being returned.
    items :: Prelude.Maybe [ChannelListConfiguration],
    -- | The pagination token from the GET list request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listChannelsResponse_items' - The objects being returned.
--
-- 'nextToken', 'listChannelsResponse_nextToken' - The pagination token from the GET list request.
--
-- 'httpStatus', 'listChannelsResponse_httpStatus' - The response's http status code.
newListChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelsResponse
newListChannelsResponse pHttpStatus_ =
  ListChannelsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The objects being returned.
listChannelsResponse_items :: Lens.Lens' ListChannelsResponse (Prelude.Maybe [ChannelListConfiguration])
listChannelsResponse_items = Lens.lens (\ListChannelsResponse' {items} -> items) (\s@ListChannelsResponse' {} a -> s {items = a} :: ListChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the GET list request.
listChannelsResponse_nextToken :: Lens.Lens' ListChannelsResponse (Prelude.Maybe Prelude.Text)
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse)

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Prelude.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

instance Prelude.NFData ListChannelsResponse where
  rnf ListChannelsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
