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
-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester\'s topics. Each call returns a limited
-- list of topics, up to 100. If there are more topics, a @NextToken@ is
-- also returned. Use the @NextToken@ parameter in a new @ListTopics@ call
-- to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListTopics
  ( -- * Creating a Request
    ListTopics (..),
    newListTopics,

    -- * Request Lenses
    listTopics_nextToken,

    -- * Destructuring the Response
    ListTopicsResponse (..),
    newListTopicsResponse,

    -- * Response Lenses
    listTopicsResponse_nextToken,
    listTopicsResponse_topics,
    listTopicsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | /See:/ 'newListTopics' smart constructor.
data ListTopics = ListTopics'
  { -- | Token returned by the previous @ListTopics@ request.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopics_nextToken' - Token returned by the previous @ListTopics@ request.
newListTopics ::
  ListTopics
newListTopics = ListTopics' {nextToken = Core.Nothing}

-- | Token returned by the previous @ListTopics@ request.
listTopics_nextToken :: Lens.Lens' ListTopics (Core.Maybe Core.Text)
listTopics_nextToken = Lens.lens (\ListTopics' {nextToken} -> nextToken) (\s@ListTopics' {} a -> s {nextToken = a} :: ListTopics)

instance Core.AWSPager ListTopics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicsResponse_topics Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTopics_nextToken
          Lens..~ rs
          Lens.^? listTopicsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTopics where
  type AWSResponse ListTopics = ListTopicsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTopicsResult"
      ( \s h x ->
          ListTopicsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Topics" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTopics

instance Core.NFData ListTopics

instance Core.ToHeaders ListTopics where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTopics where
  toPath = Core.const "/"

instance Core.ToQuery ListTopics where
  toQuery ListTopics' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ListTopics" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | Response for ListTopics action.
--
-- /See:/ 'newListTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
  { -- | Token to pass along to the next @ListTopics@ request. This element is
    -- returned if there are additional topics to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of topic ARNs.
    topics :: Core.Maybe [Topic],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopicsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicsResponse_nextToken' - Token to pass along to the next @ListTopics@ request. This element is
-- returned if there are additional topics to retrieve.
--
-- 'topics', 'listTopicsResponse_topics' - A list of topic ARNs.
--
-- 'httpStatus', 'listTopicsResponse_httpStatus' - The response's http status code.
newListTopicsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTopicsResponse
newListTopicsResponse pHttpStatus_ =
  ListTopicsResponse'
    { nextToken = Core.Nothing,
      topics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to pass along to the next @ListTopics@ request. This element is
-- returned if there are additional topics to retrieve.
listTopicsResponse_nextToken :: Lens.Lens' ListTopicsResponse (Core.Maybe Core.Text)
listTopicsResponse_nextToken = Lens.lens (\ListTopicsResponse' {nextToken} -> nextToken) (\s@ListTopicsResponse' {} a -> s {nextToken = a} :: ListTopicsResponse)

-- | A list of topic ARNs.
listTopicsResponse_topics :: Lens.Lens' ListTopicsResponse (Core.Maybe [Topic])
listTopicsResponse_topics = Lens.lens (\ListTopicsResponse' {topics} -> topics) (\s@ListTopicsResponse' {} a -> s {topics = a} :: ListTopicsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTopicsResponse_httpStatus :: Lens.Lens' ListTopicsResponse Core.Int
listTopicsResponse_httpStatus = Lens.lens (\ListTopicsResponse' {httpStatus} -> httpStatus) (\s@ListTopicsResponse' {} a -> s {httpStatus = a} :: ListTopicsResponse)

instance Core.NFData ListTopicsResponse
