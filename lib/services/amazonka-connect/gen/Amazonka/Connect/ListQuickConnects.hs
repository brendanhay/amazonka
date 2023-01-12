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
-- Module      : Amazonka.Connect.ListQuickConnects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the quick connects for the specified Amazon
-- Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListQuickConnects
  ( -- * Creating a Request
    ListQuickConnects (..),
    newListQuickConnects,

    -- * Request Lenses
    listQuickConnects_maxResults,
    listQuickConnects_nextToken,
    listQuickConnects_quickConnectTypes,
    listQuickConnects_instanceId,

    -- * Destructuring the Response
    ListQuickConnectsResponse (..),
    newListQuickConnectsResponse,

    -- * Response Lenses
    listQuickConnectsResponse_nextToken,
    listQuickConnectsResponse_quickConnectSummaryList,
    listQuickConnectsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListQuickConnects' smart constructor.
data ListQuickConnects = ListQuickConnects'
  { -- | The maximum number of results to return per page. The default MaxResult
    -- size is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of quick connect. In the Amazon Connect console, when you
    -- create a quick connect, you are prompted to assign one of the following
    -- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
    quickConnectTypes :: Prelude.Maybe [QuickConnectType],
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listQuickConnects_maxResults' - The maximum number of results to return per page. The default MaxResult
-- size is 100.
--
-- 'nextToken', 'listQuickConnects_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'quickConnectTypes', 'listQuickConnects_quickConnectTypes' - The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
--
-- 'instanceId', 'listQuickConnects_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListQuickConnects ::
  -- | 'instanceId'
  Prelude.Text ->
  ListQuickConnects
newListQuickConnects pInstanceId_ =
  ListQuickConnects'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      quickConnectTypes = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page. The default MaxResult
-- size is 100.
listQuickConnects_maxResults :: Lens.Lens' ListQuickConnects (Prelude.Maybe Prelude.Natural)
listQuickConnects_maxResults = Lens.lens (\ListQuickConnects' {maxResults} -> maxResults) (\s@ListQuickConnects' {} a -> s {maxResults = a} :: ListQuickConnects)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listQuickConnects_nextToken :: Lens.Lens' ListQuickConnects (Prelude.Maybe Prelude.Text)
listQuickConnects_nextToken = Lens.lens (\ListQuickConnects' {nextToken} -> nextToken) (\s@ListQuickConnects' {} a -> s {nextToken = a} :: ListQuickConnects)

-- | The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
listQuickConnects_quickConnectTypes :: Lens.Lens' ListQuickConnects (Prelude.Maybe [QuickConnectType])
listQuickConnects_quickConnectTypes = Lens.lens (\ListQuickConnects' {quickConnectTypes} -> quickConnectTypes) (\s@ListQuickConnects' {} a -> s {quickConnectTypes = a} :: ListQuickConnects) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listQuickConnects_instanceId :: Lens.Lens' ListQuickConnects Prelude.Text
listQuickConnects_instanceId = Lens.lens (\ListQuickConnects' {instanceId} -> instanceId) (\s@ListQuickConnects' {} a -> s {instanceId = a} :: ListQuickConnects)

instance Core.AWSPager ListQuickConnects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQuickConnectsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listQuickConnectsResponse_quickConnectSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listQuickConnects_nextToken
          Lens..~ rs
          Lens.^? listQuickConnectsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListQuickConnects where
  type
    AWSResponse ListQuickConnects =
      ListQuickConnectsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQuickConnectsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "QuickConnectSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQuickConnects where
  hashWithSalt _salt ListQuickConnects' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` quickConnectTypes
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListQuickConnects where
  rnf ListQuickConnects' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf quickConnectTypes
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListQuickConnects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListQuickConnects where
  toPath ListQuickConnects' {..} =
    Prelude.mconcat
      ["/quick-connects/", Data.toBS instanceId]

instance Data.ToQuery ListQuickConnects where
  toQuery ListQuickConnects' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "QuickConnectTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> quickConnectTypes
            )
      ]

-- | /See:/ 'newListQuickConnectsResponse' smart constructor.
data ListQuickConnectsResponse = ListQuickConnectsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the quick connects.
    quickConnectSummaryList :: Prelude.Maybe [QuickConnectSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQuickConnectsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'quickConnectSummaryList', 'listQuickConnectsResponse_quickConnectSummaryList' - Information about the quick connects.
--
-- 'httpStatus', 'listQuickConnectsResponse_httpStatus' - The response's http status code.
newListQuickConnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQuickConnectsResponse
newListQuickConnectsResponse pHttpStatus_ =
  ListQuickConnectsResponse'
    { nextToken =
        Prelude.Nothing,
      quickConnectSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listQuickConnectsResponse_nextToken :: Lens.Lens' ListQuickConnectsResponse (Prelude.Maybe Prelude.Text)
listQuickConnectsResponse_nextToken = Lens.lens (\ListQuickConnectsResponse' {nextToken} -> nextToken) (\s@ListQuickConnectsResponse' {} a -> s {nextToken = a} :: ListQuickConnectsResponse)

-- | Information about the quick connects.
listQuickConnectsResponse_quickConnectSummaryList :: Lens.Lens' ListQuickConnectsResponse (Prelude.Maybe [QuickConnectSummary])
listQuickConnectsResponse_quickConnectSummaryList = Lens.lens (\ListQuickConnectsResponse' {quickConnectSummaryList} -> quickConnectSummaryList) (\s@ListQuickConnectsResponse' {} a -> s {quickConnectSummaryList = a} :: ListQuickConnectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listQuickConnectsResponse_httpStatus :: Lens.Lens' ListQuickConnectsResponse Prelude.Int
listQuickConnectsResponse_httpStatus = Lens.lens (\ListQuickConnectsResponse' {httpStatus} -> httpStatus) (\s@ListQuickConnectsResponse' {} a -> s {httpStatus = a} :: ListQuickConnectsResponse)

instance Prelude.NFData ListQuickConnectsResponse where
  rnf ListQuickConnectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf quickConnectSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
