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
-- Module      : Amazonka.IoTThingsGraph.ListFlowExecutionMessages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of objects that contain information about events in a
-- flow execution.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.ListFlowExecutionMessages
  ( -- * Creating a Request
    ListFlowExecutionMessages (..),
    newListFlowExecutionMessages,

    -- * Request Lenses
    listFlowExecutionMessages_nextToken,
    listFlowExecutionMessages_maxResults,
    listFlowExecutionMessages_flowExecutionId,

    -- * Destructuring the Response
    ListFlowExecutionMessagesResponse (..),
    newListFlowExecutionMessagesResponse,

    -- * Response Lenses
    listFlowExecutionMessagesResponse_nextToken,
    listFlowExecutionMessagesResponse_messages,
    listFlowExecutionMessagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFlowExecutionMessages' smart constructor.
data ListFlowExecutionMessages = ListFlowExecutionMessages'
  { -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the flow execution.
    flowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowExecutionMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowExecutionMessages_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'listFlowExecutionMessages_maxResults' - The maximum number of results to return in the response.
--
-- 'flowExecutionId', 'listFlowExecutionMessages_flowExecutionId' - The ID of the flow execution.
newListFlowExecutionMessages ::
  -- | 'flowExecutionId'
  Prelude.Text ->
  ListFlowExecutionMessages
newListFlowExecutionMessages pFlowExecutionId_ =
  ListFlowExecutionMessages'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      flowExecutionId = pFlowExecutionId_
    }

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
listFlowExecutionMessages_nextToken :: Lens.Lens' ListFlowExecutionMessages (Prelude.Maybe Prelude.Text)
listFlowExecutionMessages_nextToken = Lens.lens (\ListFlowExecutionMessages' {nextToken} -> nextToken) (\s@ListFlowExecutionMessages' {} a -> s {nextToken = a} :: ListFlowExecutionMessages)

-- | The maximum number of results to return in the response.
listFlowExecutionMessages_maxResults :: Lens.Lens' ListFlowExecutionMessages (Prelude.Maybe Prelude.Natural)
listFlowExecutionMessages_maxResults = Lens.lens (\ListFlowExecutionMessages' {maxResults} -> maxResults) (\s@ListFlowExecutionMessages' {} a -> s {maxResults = a} :: ListFlowExecutionMessages)

-- | The ID of the flow execution.
listFlowExecutionMessages_flowExecutionId :: Lens.Lens' ListFlowExecutionMessages Prelude.Text
listFlowExecutionMessages_flowExecutionId = Lens.lens (\ListFlowExecutionMessages' {flowExecutionId} -> flowExecutionId) (\s@ListFlowExecutionMessages' {} a -> s {flowExecutionId = a} :: ListFlowExecutionMessages)

instance Core.AWSPager ListFlowExecutionMessages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFlowExecutionMessagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFlowExecutionMessagesResponse_messages
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFlowExecutionMessages_nextToken
          Lens..~ rs
          Lens.^? listFlowExecutionMessagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFlowExecutionMessages where
  type
    AWSResponse ListFlowExecutionMessages =
      ListFlowExecutionMessagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlowExecutionMessagesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "messages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFlowExecutionMessages where
  hashWithSalt _salt ListFlowExecutionMessages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` flowExecutionId

instance Prelude.NFData ListFlowExecutionMessages where
  rnf ListFlowExecutionMessages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf flowExecutionId

instance Core.ToHeaders ListFlowExecutionMessages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.ListFlowExecutionMessages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFlowExecutionMessages where
  toJSON ListFlowExecutionMessages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("flowExecutionId" Core..= flowExecutionId)
          ]
      )

instance Core.ToPath ListFlowExecutionMessages where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFlowExecutionMessages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFlowExecutionMessagesResponse' smart constructor.
data ListFlowExecutionMessagesResponse = ListFlowExecutionMessagesResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects that contain information about events in the specified
    -- flow execution.
    messages :: Prelude.Maybe [FlowExecutionMessage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowExecutionMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowExecutionMessagesResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'messages', 'listFlowExecutionMessagesResponse_messages' - A list of objects that contain information about events in the specified
-- flow execution.
--
-- 'httpStatus', 'listFlowExecutionMessagesResponse_httpStatus' - The response's http status code.
newListFlowExecutionMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlowExecutionMessagesResponse
newListFlowExecutionMessagesResponse pHttpStatus_ =
  ListFlowExecutionMessagesResponse'
    { nextToken =
        Prelude.Nothing,
      messages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
listFlowExecutionMessagesResponse_nextToken :: Lens.Lens' ListFlowExecutionMessagesResponse (Prelude.Maybe Prelude.Text)
listFlowExecutionMessagesResponse_nextToken = Lens.lens (\ListFlowExecutionMessagesResponse' {nextToken} -> nextToken) (\s@ListFlowExecutionMessagesResponse' {} a -> s {nextToken = a} :: ListFlowExecutionMessagesResponse)

-- | A list of objects that contain information about events in the specified
-- flow execution.
listFlowExecutionMessagesResponse_messages :: Lens.Lens' ListFlowExecutionMessagesResponse (Prelude.Maybe [FlowExecutionMessage])
listFlowExecutionMessagesResponse_messages = Lens.lens (\ListFlowExecutionMessagesResponse' {messages} -> messages) (\s@ListFlowExecutionMessagesResponse' {} a -> s {messages = a} :: ListFlowExecutionMessagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFlowExecutionMessagesResponse_httpStatus :: Lens.Lens' ListFlowExecutionMessagesResponse Prelude.Int
listFlowExecutionMessagesResponse_httpStatus = Lens.lens (\ListFlowExecutionMessagesResponse' {httpStatus} -> httpStatus) (\s@ListFlowExecutionMessagesResponse' {} a -> s {httpStatus = a} :: ListFlowExecutionMessagesResponse)

instance
  Prelude.NFData
    ListFlowExecutionMessagesResponse
  where
  rnf ListFlowExecutionMessagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf messages
      `Prelude.seq` Prelude.rnf httpStatus
