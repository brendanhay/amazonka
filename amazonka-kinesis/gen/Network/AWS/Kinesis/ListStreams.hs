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
-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Kinesis data streams.
--
-- The number of streams may be too large to return from a single call to
-- @ListStreams@. You can limit the number of returned streams using the
-- @Limit@ parameter. If you do not specify a value for the @Limit@
-- parameter, Kinesis Data Streams uses the default limit, which is
-- currently 10.
--
-- You can detect if there are more streams available to list by using the
-- @HasMoreStreams@ flag from the returned output. If there are more
-- streams available, you can request more streams by using the name of the
-- last stream returned by the @ListStreams@ request in the
-- @ExclusiveStartStreamName@ parameter in a subsequent request to
-- @ListStreams@. The group of stream names returned by the subsequent
-- request is then added to the list. You can continue this process until
-- all the stream names have been collected in the list.
--
-- ListStreams has a limit of five transactions per second per account.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_exclusiveStartStreamName,
    listStreams_limit,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_httpStatus,
    listStreamsResponse_streamNames,
    listStreamsResponse_hasMoreStreams,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @ListStreams@.
--
-- /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | The name of the stream to start the list with.
    exclusiveStartStreamName :: Core.Maybe Core.Text,
    -- | The maximum number of streams to list.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartStreamName', 'listStreams_exclusiveStartStreamName' - The name of the stream to start the list with.
--
-- 'limit', 'listStreams_limit' - The maximum number of streams to list.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { exclusiveStartStreamName =
        Core.Nothing,
      limit = Core.Nothing
    }

-- | The name of the stream to start the list with.
listStreams_exclusiveStartStreamName :: Lens.Lens' ListStreams (Core.Maybe Core.Text)
listStreams_exclusiveStartStreamName = Lens.lens (\ListStreams' {exclusiveStartStreamName} -> exclusiveStartStreamName) (\s@ListStreams' {} a -> s {exclusiveStartStreamName = a} :: ListStreams)

-- | The maximum number of streams to list.
listStreams_limit :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
listStreams_limit = Lens.lens (\ListStreams' {limit} -> limit) (\s@ListStreams' {} a -> s {limit = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        (rs Lens.^. listStreamsResponse_hasMoreStreams) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listStreamsResponse_streamNames Core.. Lens._last
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStreams_exclusiveStartStreamName
          Lens..~ rs
          Lens.^? listStreamsResponse_streamNames Core.. Lens._last

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "StreamNames" Core..!@ Core.mempty)
            Core.<*> (x Core..:> "HasMoreStreams")
      )

instance Core.Hashable ListStreams

instance Core.NFData ListStreams

instance Core.ToHeaders ListStreams where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Kinesis_20131202.ListStreams" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartStreamName" Core..=)
              Core.<$> exclusiveStartStreamName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListStreams where
  toPath = Core.const "/"

instance Core.ToQuery ListStreams where
  toQuery = Core.const Core.mempty

-- | Represents the output for @ListStreams@.
--
-- /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The names of the streams that are associated with the AWS account making
    -- the @ListStreams@ request.
    streamNames :: [Core.Text],
    -- | If set to @true@, there are more streams available to list.
    hasMoreStreams :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
--
-- 'streamNames', 'listStreamsResponse_streamNames' - The names of the streams that are associated with the AWS account making
-- the @ListStreams@ request.
--
-- 'hasMoreStreams', 'listStreamsResponse_hasMoreStreams' - If set to @true@, there are more streams available to list.
newListStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'hasMoreStreams'
  Core.Bool ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ pHasMoreStreams_ =
  ListStreamsResponse'
    { httpStatus = pHttpStatus_,
      streamNames = Core.mempty,
      hasMoreStreams = pHasMoreStreams_
    }

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Core.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

-- | The names of the streams that are associated with the AWS account making
-- the @ListStreams@ request.
listStreamsResponse_streamNames :: Lens.Lens' ListStreamsResponse [Core.Text]
listStreamsResponse_streamNames = Lens.lens (\ListStreamsResponse' {streamNames} -> streamNames) (\s@ListStreamsResponse' {} a -> s {streamNames = a} :: ListStreamsResponse) Core.. Lens._Coerce

-- | If set to @true@, there are more streams available to list.
listStreamsResponse_hasMoreStreams :: Lens.Lens' ListStreamsResponse Core.Bool
listStreamsResponse_hasMoreStreams = Lens.lens (\ListStreamsResponse' {hasMoreStreams} -> hasMoreStreams) (\s@ListStreamsResponse' {} a -> s {hasMoreStreams = a} :: ListStreamsResponse)

instance Core.NFData ListStreamsResponse
