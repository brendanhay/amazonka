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
-- Module      : Network.AWS.DirectoryService.ListLogSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active log subscriptions for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListLogSubscriptions
  ( -- * Creating a Request
    ListLogSubscriptions (..),
    newListLogSubscriptions,

    -- * Request Lenses
    listLogSubscriptions_nextToken,
    listLogSubscriptions_directoryId,
    listLogSubscriptions_limit,

    -- * Destructuring the Response
    ListLogSubscriptionsResponse (..),
    newListLogSubscriptionsResponse,

    -- * Response Lenses
    listLogSubscriptionsResponse_nextToken,
    listLogSubscriptionsResponse_logSubscriptions,
    listLogSubscriptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLogSubscriptions' smart constructor.
data ListLogSubscriptions = ListLogSubscriptions'
  { -- | The token for the next set of items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | If a /DirectoryID/ is provided, lists only the log subscription
    -- associated with that directory. If no /DirectoryId/ is provided, lists
    -- all log subscriptions associated with your AWS account. If there are no
    -- log subscriptions for the AWS account or the directory, an empty list
    -- will be returned.
    directoryId :: Core.Maybe Core.Text,
    -- | The maximum number of items returned.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLogSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLogSubscriptions_nextToken' - The token for the next set of items to return.
--
-- 'directoryId', 'listLogSubscriptions_directoryId' - If a /DirectoryID/ is provided, lists only the log subscription
-- associated with that directory. If no /DirectoryId/ is provided, lists
-- all log subscriptions associated with your AWS account. If there are no
-- log subscriptions for the AWS account or the directory, an empty list
-- will be returned.
--
-- 'limit', 'listLogSubscriptions_limit' - The maximum number of items returned.
newListLogSubscriptions ::
  ListLogSubscriptions
newListLogSubscriptions =
  ListLogSubscriptions'
    { nextToken = Core.Nothing,
      directoryId = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token for the next set of items to return.
listLogSubscriptions_nextToken :: Lens.Lens' ListLogSubscriptions (Core.Maybe Core.Text)
listLogSubscriptions_nextToken = Lens.lens (\ListLogSubscriptions' {nextToken} -> nextToken) (\s@ListLogSubscriptions' {} a -> s {nextToken = a} :: ListLogSubscriptions)

-- | If a /DirectoryID/ is provided, lists only the log subscription
-- associated with that directory. If no /DirectoryId/ is provided, lists
-- all log subscriptions associated with your AWS account. If there are no
-- log subscriptions for the AWS account or the directory, an empty list
-- will be returned.
listLogSubscriptions_directoryId :: Lens.Lens' ListLogSubscriptions (Core.Maybe Core.Text)
listLogSubscriptions_directoryId = Lens.lens (\ListLogSubscriptions' {directoryId} -> directoryId) (\s@ListLogSubscriptions' {} a -> s {directoryId = a} :: ListLogSubscriptions)

-- | The maximum number of items returned.
listLogSubscriptions_limit :: Lens.Lens' ListLogSubscriptions (Core.Maybe Core.Natural)
listLogSubscriptions_limit = Lens.lens (\ListLogSubscriptions' {limit} -> limit) (\s@ListLogSubscriptions' {} a -> s {limit = a} :: ListLogSubscriptions)

instance Core.AWSPager ListLogSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLogSubscriptionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listLogSubscriptionsResponse_logSubscriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLogSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listLogSubscriptionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListLogSubscriptions where
  type
    AWSResponse ListLogSubscriptions =
      ListLogSubscriptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogSubscriptionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "LogSubscriptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLogSubscriptions

instance Core.NFData ListLogSubscriptions

instance Core.ToHeaders ListLogSubscriptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ListLogSubscriptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListLogSubscriptions where
  toJSON ListLogSubscriptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("DirectoryId" Core..=) Core.<$> directoryId,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListLogSubscriptions where
  toPath = Core.const "/"

instance Core.ToQuery ListLogSubscriptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListLogSubscriptionsResponse' smart constructor.
data ListLogSubscriptionsResponse = ListLogSubscriptionsResponse'
  { -- | The token for the next set of items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of active LogSubscription objects for calling the AWS account.
    logSubscriptions :: Core.Maybe [LogSubscription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLogSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLogSubscriptionsResponse_nextToken' - The token for the next set of items to return.
--
-- 'logSubscriptions', 'listLogSubscriptionsResponse_logSubscriptions' - A list of active LogSubscription objects for calling the AWS account.
--
-- 'httpStatus', 'listLogSubscriptionsResponse_httpStatus' - The response's http status code.
newListLogSubscriptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListLogSubscriptionsResponse
newListLogSubscriptionsResponse pHttpStatus_ =
  ListLogSubscriptionsResponse'
    { nextToken =
        Core.Nothing,
      logSubscriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return.
listLogSubscriptionsResponse_nextToken :: Lens.Lens' ListLogSubscriptionsResponse (Core.Maybe Core.Text)
listLogSubscriptionsResponse_nextToken = Lens.lens (\ListLogSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListLogSubscriptionsResponse' {} a -> s {nextToken = a} :: ListLogSubscriptionsResponse)

-- | A list of active LogSubscription objects for calling the AWS account.
listLogSubscriptionsResponse_logSubscriptions :: Lens.Lens' ListLogSubscriptionsResponse (Core.Maybe [LogSubscription])
listLogSubscriptionsResponse_logSubscriptions = Lens.lens (\ListLogSubscriptionsResponse' {logSubscriptions} -> logSubscriptions) (\s@ListLogSubscriptionsResponse' {} a -> s {logSubscriptions = a} :: ListLogSubscriptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLogSubscriptionsResponse_httpStatus :: Lens.Lens' ListLogSubscriptionsResponse Core.Int
listLogSubscriptionsResponse_httpStatus = Lens.lens (\ListLogSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListLogSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListLogSubscriptionsResponse)

instance Core.NFData ListLogSubscriptionsResponse
