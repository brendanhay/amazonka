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
    listTopicsResponse_topics,
    listTopicsResponse_nextToken,
    listTopicsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | /See:/ 'newListTopics' smart constructor.
data ListTopics = ListTopics'
  { -- | Token returned by the previous @ListTopics@ request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newListTopics =
  ListTopics' {nextToken = Prelude.Nothing}

-- | Token returned by the previous @ListTopics@ request.
listTopics_nextToken :: Lens.Lens' ListTopics (Prelude.Maybe Prelude.Text)
listTopics_nextToken = Lens.lens (\ListTopics' {nextToken} -> nextToken) (\s@ListTopics' {} a -> s {nextToken = a} :: ListTopics)

instance Core.AWSPager ListTopics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicsResponse_topics Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTopics_nextToken
          Lens..~ rs
          Lens.^? listTopicsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTopics where
  type AWSResponse ListTopics = ListTopicsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTopicsResult"
      ( \s h x ->
          ListTopicsResponse'
            Prelude.<$> ( x Core..@? "Topics" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopics

instance Prelude.NFData ListTopics

instance Core.ToHeaders ListTopics where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTopics where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTopics where
  toQuery ListTopics' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListTopics" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | Response for ListTopics action.
--
-- /See:/ 'newListTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
  { -- | A list of topic ARNs.
    topics :: Prelude.Maybe [Topic],
    -- | Token to pass along to the next @ListTopics@ request. This element is
    -- returned if there are additional topics to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topics', 'listTopicsResponse_topics' - A list of topic ARNs.
--
-- 'nextToken', 'listTopicsResponse_nextToken' - Token to pass along to the next @ListTopics@ request. This element is
-- returned if there are additional topics to retrieve.
--
-- 'httpStatus', 'listTopicsResponse_httpStatus' - The response's http status code.
newListTopicsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTopicsResponse
newListTopicsResponse pHttpStatus_ =
  ListTopicsResponse'
    { topics = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of topic ARNs.
listTopicsResponse_topics :: Lens.Lens' ListTopicsResponse (Prelude.Maybe [Topic])
listTopicsResponse_topics = Lens.lens (\ListTopicsResponse' {topics} -> topics) (\s@ListTopicsResponse' {} a -> s {topics = a} :: ListTopicsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token to pass along to the next @ListTopics@ request. This element is
-- returned if there are additional topics to retrieve.
listTopicsResponse_nextToken :: Lens.Lens' ListTopicsResponse (Prelude.Maybe Prelude.Text)
listTopicsResponse_nextToken = Lens.lens (\ListTopicsResponse' {nextToken} -> nextToken) (\s@ListTopicsResponse' {} a -> s {nextToken = a} :: ListTopicsResponse)

-- | The response's http status code.
listTopicsResponse_httpStatus :: Lens.Lens' ListTopicsResponse Prelude.Int
listTopicsResponse_httpStatus = Lens.lens (\ListTopicsResponse' {httpStatus} -> httpStatus) (\s@ListTopicsResponse' {} a -> s {httpStatus = a} :: ListTopicsResponse)

instance Prelude.NFData ListTopicsResponse
