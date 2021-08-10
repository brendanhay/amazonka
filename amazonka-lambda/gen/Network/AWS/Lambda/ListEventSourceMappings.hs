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
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists event source mappings. Specify an @EventSourceArn@ to only show
-- event source mappings for a single event source.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListEventSourceMappings
  ( -- * Creating a Request
    ListEventSourceMappings (..),
    newListEventSourceMappings,

    -- * Request Lenses
    listEventSourceMappings_eventSourceArn,
    listEventSourceMappings_functionName,
    listEventSourceMappings_maxItems,
    listEventSourceMappings_marker,

    -- * Destructuring the Response
    ListEventSourceMappingsResponse (..),
    newListEventSourceMappingsResponse,

    -- * Response Lenses
    listEventSourceMappingsResponse_eventSourceMappings,
    listEventSourceMappingsResponse_nextMarker,
    listEventSourceMappingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEventSourceMappings' smart constructor.
data ListEventSourceMappings = ListEventSourceMappings'
  { -- | The Amazon Resource Name (ARN) of the event source.
    --
    -- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
    --     consumer.
    --
    -- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
    --
    -- -   __Amazon Simple Queue Service__ - The ARN of the queue.
    --
    -- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
    --     cluster.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Version or Alias ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it\'s limited to 64 characters in length.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of event source mappings to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token returned by a previous call.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventSourceMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'listEventSourceMappings_eventSourceArn' - The Amazon Resource Name (ARN) of the event source.
--
-- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
--     consumer.
--
-- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
--
-- -   __Amazon Simple Queue Service__ - The ARN of the queue.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
--     cluster.
--
-- 'functionName', 'listEventSourceMappings_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Version or Alias ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it\'s limited to 64 characters in length.
--
-- 'maxItems', 'listEventSourceMappings_maxItems' - The maximum number of event source mappings to return.
--
-- 'marker', 'listEventSourceMappings_marker' - A pagination token returned by a previous call.
newListEventSourceMappings ::
  ListEventSourceMappings
newListEventSourceMappings =
  ListEventSourceMappings'
    { eventSourceArn =
        Prelude.Nothing,
      functionName = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
--
-- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
--     consumer.
--
-- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
--
-- -   __Amazon Simple Queue Service__ - The ARN of the queue.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
--     cluster.
listEventSourceMappings_eventSourceArn :: Lens.Lens' ListEventSourceMappings (Prelude.Maybe Prelude.Text)
listEventSourceMappings_eventSourceArn = Lens.lens (\ListEventSourceMappings' {eventSourceArn} -> eventSourceArn) (\s@ListEventSourceMappings' {} a -> s {eventSourceArn = a} :: ListEventSourceMappings)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Version or Alias ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it\'s limited to 64 characters in length.
listEventSourceMappings_functionName :: Lens.Lens' ListEventSourceMappings (Prelude.Maybe Prelude.Text)
listEventSourceMappings_functionName = Lens.lens (\ListEventSourceMappings' {functionName} -> functionName) (\s@ListEventSourceMappings' {} a -> s {functionName = a} :: ListEventSourceMappings)

-- | The maximum number of event source mappings to return.
listEventSourceMappings_maxItems :: Lens.Lens' ListEventSourceMappings (Prelude.Maybe Prelude.Natural)
listEventSourceMappings_maxItems = Lens.lens (\ListEventSourceMappings' {maxItems} -> maxItems) (\s@ListEventSourceMappings' {} a -> s {maxItems = a} :: ListEventSourceMappings)

-- | A pagination token returned by a previous call.
listEventSourceMappings_marker :: Lens.Lens' ListEventSourceMappings (Prelude.Maybe Prelude.Text)
listEventSourceMappings_marker = Lens.lens (\ListEventSourceMappings' {marker} -> marker) (\s@ListEventSourceMappings' {} a -> s {marker = a} :: ListEventSourceMappings)

instance Core.AWSPager ListEventSourceMappings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventSourceMappingsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEventSourceMappingsResponse_eventSourceMappings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEventSourceMappings_marker
          Lens..~ rs
          Lens.^? listEventSourceMappingsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListEventSourceMappings where
  type
    AWSResponse ListEventSourceMappings =
      ListEventSourceMappingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSourceMappingsResponse'
            Prelude.<$> ( x Core..?> "EventSourceMappings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventSourceMappings

instance Prelude.NFData ListEventSourceMappings

instance Core.ToHeaders ListEventSourceMappings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListEventSourceMappings where
  toPath =
    Prelude.const "/2015-03-31/event-source-mappings/"

instance Core.ToQuery ListEventSourceMappings where
  toQuery ListEventSourceMappings' {..} =
    Prelude.mconcat
      [ "EventSourceArn" Core.=: eventSourceArn,
        "FunctionName" Core.=: functionName,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListEventSourceMappingsResponse' smart constructor.
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
  { -- | A list of event source mappings.
    eventSourceMappings :: Prelude.Maybe [EventSourceMappingConfiguration],
    -- | A pagination token that\'s returned when the response doesn\'t contain
    -- all event source mappings.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventSourceMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceMappings', 'listEventSourceMappingsResponse_eventSourceMappings' - A list of event source mappings.
--
-- 'nextMarker', 'listEventSourceMappingsResponse_nextMarker' - A pagination token that\'s returned when the response doesn\'t contain
-- all event source mappings.
--
-- 'httpStatus', 'listEventSourceMappingsResponse_httpStatus' - The response's http status code.
newListEventSourceMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventSourceMappingsResponse
newListEventSourceMappingsResponse pHttpStatus_ =
  ListEventSourceMappingsResponse'
    { eventSourceMappings =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event source mappings.
listEventSourceMappingsResponse_eventSourceMappings :: Lens.Lens' ListEventSourceMappingsResponse (Prelude.Maybe [EventSourceMappingConfiguration])
listEventSourceMappingsResponse_eventSourceMappings = Lens.lens (\ListEventSourceMappingsResponse' {eventSourceMappings} -> eventSourceMappings) (\s@ListEventSourceMappingsResponse' {} a -> s {eventSourceMappings = a} :: ListEventSourceMappingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A pagination token that\'s returned when the response doesn\'t contain
-- all event source mappings.
listEventSourceMappingsResponse_nextMarker :: Lens.Lens' ListEventSourceMappingsResponse (Prelude.Maybe Prelude.Text)
listEventSourceMappingsResponse_nextMarker = Lens.lens (\ListEventSourceMappingsResponse' {nextMarker} -> nextMarker) (\s@ListEventSourceMappingsResponse' {} a -> s {nextMarker = a} :: ListEventSourceMappingsResponse)

-- | The response's http status code.
listEventSourceMappingsResponse_httpStatus :: Lens.Lens' ListEventSourceMappingsResponse Prelude.Int
listEventSourceMappingsResponse_httpStatus = Lens.lens (\ListEventSourceMappingsResponse' {httpStatus} -> httpStatus) (\s@ListEventSourceMappingsResponse' {} a -> s {httpStatus = a} :: ListEventSourceMappingsResponse)

instance
  Prelude.NFData
    ListEventSourceMappingsResponse
