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
-- Module      : Amazonka.IoT.ListStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the streams in your Amazon Web Services account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListStreams>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_ascendingOrder,
    listStreams_maxResults,
    listStreams_nextToken,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_nextToken,
    listStreamsResponse_streams,
    listStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | Set to true to return the list of streams in ascending order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return at a time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ascendingOrder', 'listStreams_ascendingOrder' - Set to true to return the list of streams in ascending order.
--
-- 'maxResults', 'listStreams_maxResults' - The maximum number of results to return at a time.
--
-- 'nextToken', 'listStreams_nextToken' - A token used to get the next set of results.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { ascendingOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Set to true to return the list of streams in ascending order.
listStreams_ascendingOrder :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Bool)
listStreams_ascendingOrder = Lens.lens (\ListStreams' {ascendingOrder} -> ascendingOrder) (\s@ListStreams' {} a -> s {ascendingOrder = a} :: ListStreams)

-- | The maximum number of results to return at a time.
listStreams_maxResults :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Natural)
listStreams_maxResults = Lens.lens (\ListStreams' {maxResults} -> maxResults) (\s@ListStreams' {} a -> s {maxResults = a} :: ListStreams)

-- | A token used to get the next set of results.
listStreams_nextToken :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_nextToken = Lens.lens (\ListStreams' {nextToken} -> nextToken) (\s@ListStreams' {} a -> s {nextToken = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_streams Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreams_nextToken
          Lens..~ rs
          Lens.^? listStreamsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "streams" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreams where
  hashWithSalt _salt ListStreams' {..} =
    _salt `Prelude.hashWithSalt` ascendingOrder
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStreams where
  rnf ListStreams' {..} =
    Prelude.rnf ascendingOrder
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListStreams where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListStreams where
  toPath = Prelude.const "/streams"

instance Data.ToQuery ListStreams where
  toQuery ListStreams' {..} =
    Prelude.mconcat
      [ "isAscendingOrder" Data.=: ascendingOrder,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | A token used to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of streams.
    streams :: Prelude.Maybe [StreamSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamsResponse_nextToken' - A token used to get the next set of results.
--
-- 'streams', 'listStreamsResponse_streams' - A list of streams.
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
newListStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ =
  ListStreamsResponse'
    { nextToken = Prelude.Nothing,
      streams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used to get the next set of results.
listStreamsResponse_nextToken :: Lens.Lens' ListStreamsResponse (Prelude.Maybe Prelude.Text)
listStreamsResponse_nextToken = Lens.lens (\ListStreamsResponse' {nextToken} -> nextToken) (\s@ListStreamsResponse' {} a -> s {nextToken = a} :: ListStreamsResponse)

-- | A list of streams.
listStreamsResponse_streams :: Lens.Lens' ListStreamsResponse (Prelude.Maybe [StreamSummary])
listStreamsResponse_streams = Lens.lens (\ListStreamsResponse' {streams} -> streams) (\s@ListStreamsResponse' {} a -> s {streams = a} :: ListStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Prelude.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

instance Prelude.NFData ListStreamsResponse where
  rnf ListStreamsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streams
      `Prelude.seq` Prelude.rnf httpStatus
