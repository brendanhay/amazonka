{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLogSubscriptions' smart constructor.
data ListLogSubscriptions = ListLogSubscriptions'
  { -- | The token for the next set of items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If a /DirectoryID/ is provided, lists only the log subscription
    -- associated with that directory. If no /DirectoryId/ is provided, lists
    -- all log subscriptions associated with your AWS account. If there are no
    -- log subscriptions for the AWS account or the directory, an empty list
    -- will be returned.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The token for the next set of items to return.
listLogSubscriptions_nextToken :: Lens.Lens' ListLogSubscriptions (Prelude.Maybe Prelude.Text)
listLogSubscriptions_nextToken = Lens.lens (\ListLogSubscriptions' {nextToken} -> nextToken) (\s@ListLogSubscriptions' {} a -> s {nextToken = a} :: ListLogSubscriptions)

-- | If a /DirectoryID/ is provided, lists only the log subscription
-- associated with that directory. If no /DirectoryId/ is provided, lists
-- all log subscriptions associated with your AWS account. If there are no
-- log subscriptions for the AWS account or the directory, an empty list
-- will be returned.
listLogSubscriptions_directoryId :: Lens.Lens' ListLogSubscriptions (Prelude.Maybe Prelude.Text)
listLogSubscriptions_directoryId = Lens.lens (\ListLogSubscriptions' {directoryId} -> directoryId) (\s@ListLogSubscriptions' {} a -> s {directoryId = a} :: ListLogSubscriptions)

-- | The maximum number of items returned.
listLogSubscriptions_limit :: Lens.Lens' ListLogSubscriptions (Prelude.Maybe Prelude.Natural)
listLogSubscriptions_limit = Lens.lens (\ListLogSubscriptions' {limit} -> limit) (\s@ListLogSubscriptions' {} a -> s {limit = a} :: ListLogSubscriptions)

instance Pager.AWSPager ListLogSubscriptions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listLogSubscriptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listLogSubscriptionsResponse_logSubscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listLogSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listLogSubscriptionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListLogSubscriptions where
  type
    Rs ListLogSubscriptions =
      ListLogSubscriptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogSubscriptionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "LogSubscriptions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLogSubscriptions

instance Prelude.NFData ListLogSubscriptions

instance Prelude.ToHeaders ListLogSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.ListLogSubscriptions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListLogSubscriptions where
  toJSON ListLogSubscriptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("DirectoryId" Prelude..=) Prelude.<$> directoryId,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListLogSubscriptions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListLogSubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLogSubscriptionsResponse' smart constructor.
data ListLogSubscriptionsResponse = ListLogSubscriptionsResponse'
  { -- | The token for the next set of items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of active LogSubscription objects for calling the AWS account.
    logSubscriptions :: Prelude.Maybe [LogSubscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListLogSubscriptionsResponse
newListLogSubscriptionsResponse pHttpStatus_ =
  ListLogSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      logSubscriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return.
listLogSubscriptionsResponse_nextToken :: Lens.Lens' ListLogSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listLogSubscriptionsResponse_nextToken = Lens.lens (\ListLogSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListLogSubscriptionsResponse' {} a -> s {nextToken = a} :: ListLogSubscriptionsResponse)

-- | A list of active LogSubscription objects for calling the AWS account.
listLogSubscriptionsResponse_logSubscriptions :: Lens.Lens' ListLogSubscriptionsResponse (Prelude.Maybe [LogSubscription])
listLogSubscriptionsResponse_logSubscriptions = Lens.lens (\ListLogSubscriptionsResponse' {logSubscriptions} -> logSubscriptions) (\s@ListLogSubscriptionsResponse' {} a -> s {logSubscriptions = a} :: ListLogSubscriptionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listLogSubscriptionsResponse_httpStatus :: Lens.Lens' ListLogSubscriptionsResponse Prelude.Int
listLogSubscriptionsResponse_httpStatus = Lens.lens (\ListLogSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListLogSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListLogSubscriptionsResponse)

instance Prelude.NFData ListLogSubscriptionsResponse
