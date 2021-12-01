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
-- Module      : Amazonka.KinesisVideo.ListStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @StreamInfo@ objects. Each object describes a
-- stream. To retrieve only streams that satisfy a specific condition, you
-- can specify a @StreamNameCondition@.
--
-- This operation returns paginated results.
module Amazonka.KinesisVideo.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_nextToken,
    listStreams_streamNameCondition,
    listStreams_maxResults,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_streamInfoList,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | If you specify this parameter, when the result of a @ListStreams@
    -- operation is truncated, the call returns the @NextToken@ in the
    -- response. To get another batch of streams, provide this token in your
    -- next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional: Returns only streams that satisfy a specific condition.
    -- Currently, you can specify only the prefix of a stream name as a
    -- condition.
    streamNameCondition :: Prelude.Maybe StreamNameCondition,
    -- | The maximum number of streams to return in the response. The default is
    -- 10,000.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listStreams_nextToken' - If you specify this parameter, when the result of a @ListStreams@
-- operation is truncated, the call returns the @NextToken@ in the
-- response. To get another batch of streams, provide this token in your
-- next request.
--
-- 'streamNameCondition', 'listStreams_streamNameCondition' - Optional: Returns only streams that satisfy a specific condition.
-- Currently, you can specify only the prefix of a stream name as a
-- condition.
--
-- 'maxResults', 'listStreams_maxResults' - The maximum number of streams to return in the response. The default is
-- 10,000.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { nextToken = Prelude.Nothing,
      streamNameCondition = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If you specify this parameter, when the result of a @ListStreams@
-- operation is truncated, the call returns the @NextToken@ in the
-- response. To get another batch of streams, provide this token in your
-- next request.
listStreams_nextToken :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_nextToken = Lens.lens (\ListStreams' {nextToken} -> nextToken) (\s@ListStreams' {} a -> s {nextToken = a} :: ListStreams)

-- | Optional: Returns only streams that satisfy a specific condition.
-- Currently, you can specify only the prefix of a stream name as a
-- condition.
listStreams_streamNameCondition :: Lens.Lens' ListStreams (Prelude.Maybe StreamNameCondition)
listStreams_streamNameCondition = Lens.lens (\ListStreams' {streamNameCondition} -> streamNameCondition) (\s@ListStreams' {} a -> s {streamNameCondition = a} :: ListStreams)

-- | The maximum number of streams to return in the response. The default is
-- 10,000.
listStreams_maxResults :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Natural)
listStreams_maxResults = Lens.lens (\ListStreams' {maxResults} -> maxResults) (\s@ListStreams' {} a -> s {maxResults = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_streamInfoList
              Prelude.. Lens._Just
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Prelude.<$> (x Core..?> "StreamInfoList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreams where
  hashWithSalt salt' ListStreams' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` streamNameCondition
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStreams where
  rnf ListStreams' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf streamNameCondition

instance Core.ToHeaders ListStreams where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StreamNameCondition" Core..=)
              Prelude.<$> streamNameCondition,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListStreams where
  toPath = Prelude.const "/listStreams"

instance Core.ToQuery ListStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | An array of @StreamInfo@ objects.
    streamInfoList :: Prelude.Maybe [StreamInfo],
    -- | If the response is truncated, the call returns this element with a
    -- token. To get the next batch of streams, use this token in your next
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'streamInfoList', 'listStreamsResponse_streamInfoList' - An array of @StreamInfo@ objects.
--
-- 'nextToken', 'listStreamsResponse_nextToken' - If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
newListStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ =
  ListStreamsResponse'
    { streamInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @StreamInfo@ objects.
listStreamsResponse_streamInfoList :: Lens.Lens' ListStreamsResponse (Prelude.Maybe [StreamInfo])
listStreamsResponse_streamInfoList = Lens.lens (\ListStreamsResponse' {streamInfoList} -> streamInfoList) (\s@ListStreamsResponse' {} a -> s {streamInfoList = a} :: ListStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
listStreamsResponse_nextToken :: Lens.Lens' ListStreamsResponse (Prelude.Maybe Prelude.Text)
listStreamsResponse_nextToken = Lens.lens (\ListStreamsResponse' {nextToken} -> nextToken) (\s@ListStreamsResponse' {} a -> s {nextToken = a} :: ListStreamsResponse)

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Prelude.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

instance Prelude.NFData ListStreamsResponse where
  rnf ListStreamsResponse' {..} =
    Prelude.rnf streamInfoList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
