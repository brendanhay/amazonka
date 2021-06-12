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
-- Module      : Network.AWS.CloudWatchEvents.ListReplays
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your replays. You can either list all the replays or you can
-- provide a prefix to match to the replay names. Filter parameters are
-- exclusive.
module Network.AWS.CloudWatchEvents.ListReplays
  ( -- * Creating a Request
    ListReplays (..),
    newListReplays,

    -- * Request Lenses
    listReplays_nextToken,
    listReplays_eventSourceArn,
    listReplays_state,
    listReplays_namePrefix,
    listReplays_limit,

    -- * Destructuring the Response
    ListReplaysResponse (..),
    newListReplaysResponse,

    -- * Response Lenses
    listReplaysResponse_nextToken,
    listReplaysResponse_replays,
    listReplaysResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReplays' smart constructor.
data ListReplays = ListReplays'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARN of the event source associated with the replay.
    eventSourceArn :: Core.Maybe Core.Text,
    -- | The state of the replay.
    state :: Core.Maybe ReplayState,
    -- | A name prefix to filter the replays returned. Only replays with name
    -- that match the prefix are returned.
    namePrefix :: Core.Maybe Core.Text,
    -- | The maximum number of replays to retrieve.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReplays' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReplays_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'eventSourceArn', 'listReplays_eventSourceArn' - The ARN of the event source associated with the replay.
--
-- 'state', 'listReplays_state' - The state of the replay.
--
-- 'namePrefix', 'listReplays_namePrefix' - A name prefix to filter the replays returned. Only replays with name
-- that match the prefix are returned.
--
-- 'limit', 'listReplays_limit' - The maximum number of replays to retrieve.
newListReplays ::
  ListReplays
newListReplays =
  ListReplays'
    { nextToken = Core.Nothing,
      eventSourceArn = Core.Nothing,
      state = Core.Nothing,
      namePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listReplays_nextToken :: Lens.Lens' ListReplays (Core.Maybe Core.Text)
listReplays_nextToken = Lens.lens (\ListReplays' {nextToken} -> nextToken) (\s@ListReplays' {} a -> s {nextToken = a} :: ListReplays)

-- | The ARN of the event source associated with the replay.
listReplays_eventSourceArn :: Lens.Lens' ListReplays (Core.Maybe Core.Text)
listReplays_eventSourceArn = Lens.lens (\ListReplays' {eventSourceArn} -> eventSourceArn) (\s@ListReplays' {} a -> s {eventSourceArn = a} :: ListReplays)

-- | The state of the replay.
listReplays_state :: Lens.Lens' ListReplays (Core.Maybe ReplayState)
listReplays_state = Lens.lens (\ListReplays' {state} -> state) (\s@ListReplays' {} a -> s {state = a} :: ListReplays)

-- | A name prefix to filter the replays returned. Only replays with name
-- that match the prefix are returned.
listReplays_namePrefix :: Lens.Lens' ListReplays (Core.Maybe Core.Text)
listReplays_namePrefix = Lens.lens (\ListReplays' {namePrefix} -> namePrefix) (\s@ListReplays' {} a -> s {namePrefix = a} :: ListReplays)

-- | The maximum number of replays to retrieve.
listReplays_limit :: Lens.Lens' ListReplays (Core.Maybe Core.Natural)
listReplays_limit = Lens.lens (\ListReplays' {limit} -> limit) (\s@ListReplays' {} a -> s {limit = a} :: ListReplays)

instance Core.AWSRequest ListReplays where
  type AWSResponse ListReplays = ListReplaysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReplaysResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Replays" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReplays

instance Core.NFData ListReplays

instance Core.ToHeaders ListReplays where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListReplays" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListReplays where
  toJSON ListReplays' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("EventSourceArn" Core..=) Core.<$> eventSourceArn,
            ("State" Core..=) Core.<$> state,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListReplays where
  toPath = Core.const "/"

instance Core.ToQuery ListReplays where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListReplaysResponse' smart constructor.
data ListReplaysResponse = ListReplaysResponse'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @Replay@ objects that contain information about the replay.
    replays :: Core.Maybe [Replay],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReplaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReplaysResponse_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'replays', 'listReplaysResponse_replays' - An array of @Replay@ objects that contain information about the replay.
--
-- 'httpStatus', 'listReplaysResponse_httpStatus' - The response's http status code.
newListReplaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReplaysResponse
newListReplaysResponse pHttpStatus_ =
  ListReplaysResponse'
    { nextToken = Core.Nothing,
      replays = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listReplaysResponse_nextToken :: Lens.Lens' ListReplaysResponse (Core.Maybe Core.Text)
listReplaysResponse_nextToken = Lens.lens (\ListReplaysResponse' {nextToken} -> nextToken) (\s@ListReplaysResponse' {} a -> s {nextToken = a} :: ListReplaysResponse)

-- | An array of @Replay@ objects that contain information about the replay.
listReplaysResponse_replays :: Lens.Lens' ListReplaysResponse (Core.Maybe [Replay])
listReplaysResponse_replays = Lens.lens (\ListReplaysResponse' {replays} -> replays) (\s@ListReplaysResponse' {} a -> s {replays = a} :: ListReplaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReplaysResponse_httpStatus :: Lens.Lens' ListReplaysResponse Core.Int
listReplaysResponse_httpStatus = Lens.lens (\ListReplaysResponse' {httpStatus} -> httpStatus) (\s@ListReplaysResponse' {} a -> s {httpStatus = a} :: ListReplaysResponse)

instance Core.NFData ListReplaysResponse
