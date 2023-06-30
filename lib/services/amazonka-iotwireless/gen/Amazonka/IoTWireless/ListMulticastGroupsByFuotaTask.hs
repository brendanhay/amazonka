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
-- Module      : Amazonka.IoTWireless.ListMulticastGroupsByFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all multicast groups associated with a fuota task.
module Amazonka.IoTWireless.ListMulticastGroupsByFuotaTask
  ( -- * Creating a Request
    ListMulticastGroupsByFuotaTask (..),
    newListMulticastGroupsByFuotaTask,

    -- * Request Lenses
    listMulticastGroupsByFuotaTask_maxResults,
    listMulticastGroupsByFuotaTask_nextToken,
    listMulticastGroupsByFuotaTask_id,

    -- * Destructuring the Response
    ListMulticastGroupsByFuotaTaskResponse (..),
    newListMulticastGroupsByFuotaTaskResponse,

    -- * Response Lenses
    listMulticastGroupsByFuotaTaskResponse_multicastGroupList,
    listMulticastGroupsByFuotaTaskResponse_nextToken,
    listMulticastGroupsByFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMulticastGroupsByFuotaTask' smart constructor.
data ListMulticastGroupsByFuotaTask = ListMulticastGroupsByFuotaTask'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMulticastGroupsByFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMulticastGroupsByFuotaTask_maxResults' - Undocumented member.
--
-- 'nextToken', 'listMulticastGroupsByFuotaTask_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'id', 'listMulticastGroupsByFuotaTask_id' - Undocumented member.
newListMulticastGroupsByFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  ListMulticastGroupsByFuotaTask
newListMulticastGroupsByFuotaTask pId_ =
  ListMulticastGroupsByFuotaTask'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
listMulticastGroupsByFuotaTask_maxResults :: Lens.Lens' ListMulticastGroupsByFuotaTask (Prelude.Maybe Prelude.Natural)
listMulticastGroupsByFuotaTask_maxResults = Lens.lens (\ListMulticastGroupsByFuotaTask' {maxResults} -> maxResults) (\s@ListMulticastGroupsByFuotaTask' {} a -> s {maxResults = a} :: ListMulticastGroupsByFuotaTask)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listMulticastGroupsByFuotaTask_nextToken :: Lens.Lens' ListMulticastGroupsByFuotaTask (Prelude.Maybe Prelude.Text)
listMulticastGroupsByFuotaTask_nextToken = Lens.lens (\ListMulticastGroupsByFuotaTask' {nextToken} -> nextToken) (\s@ListMulticastGroupsByFuotaTask' {} a -> s {nextToken = a} :: ListMulticastGroupsByFuotaTask)

-- | Undocumented member.
listMulticastGroupsByFuotaTask_id :: Lens.Lens' ListMulticastGroupsByFuotaTask Prelude.Text
listMulticastGroupsByFuotaTask_id = Lens.lens (\ListMulticastGroupsByFuotaTask' {id} -> id) (\s@ListMulticastGroupsByFuotaTask' {} a -> s {id = a} :: ListMulticastGroupsByFuotaTask)

instance
  Core.AWSRequest
    ListMulticastGroupsByFuotaTask
  where
  type
    AWSResponse ListMulticastGroupsByFuotaTask =
      ListMulticastGroupsByFuotaTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMulticastGroupsByFuotaTaskResponse'
            Prelude.<$> ( x
                            Data..?> "MulticastGroupList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMulticastGroupsByFuotaTask
  where
  hashWithSalt
    _salt
    ListMulticastGroupsByFuotaTask' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    ListMulticastGroupsByFuotaTask
  where
  rnf ListMulticastGroupsByFuotaTask' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    ListMulticastGroupsByFuotaTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMulticastGroupsByFuotaTask where
  toPath ListMulticastGroupsByFuotaTask' {..} =
    Prelude.mconcat
      ["/fuota-tasks/", Data.toBS id, "/multicast-groups"]

instance Data.ToQuery ListMulticastGroupsByFuotaTask where
  toQuery ListMulticastGroupsByFuotaTask' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListMulticastGroupsByFuotaTaskResponse' smart constructor.
data ListMulticastGroupsByFuotaTaskResponse = ListMulticastGroupsByFuotaTaskResponse'
  { multicastGroupList :: Prelude.Maybe [MulticastGroupByFuotaTask],
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMulticastGroupsByFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multicastGroupList', 'listMulticastGroupsByFuotaTaskResponse_multicastGroupList' - Undocumented member.
--
-- 'nextToken', 'listMulticastGroupsByFuotaTaskResponse_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'httpStatus', 'listMulticastGroupsByFuotaTaskResponse_httpStatus' - The response's http status code.
newListMulticastGroupsByFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMulticastGroupsByFuotaTaskResponse
newListMulticastGroupsByFuotaTaskResponse
  pHttpStatus_ =
    ListMulticastGroupsByFuotaTaskResponse'
      { multicastGroupList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
listMulticastGroupsByFuotaTaskResponse_multicastGroupList :: Lens.Lens' ListMulticastGroupsByFuotaTaskResponse (Prelude.Maybe [MulticastGroupByFuotaTask])
listMulticastGroupsByFuotaTaskResponse_multicastGroupList = Lens.lens (\ListMulticastGroupsByFuotaTaskResponse' {multicastGroupList} -> multicastGroupList) (\s@ListMulticastGroupsByFuotaTaskResponse' {} a -> s {multicastGroupList = a} :: ListMulticastGroupsByFuotaTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listMulticastGroupsByFuotaTaskResponse_nextToken :: Lens.Lens' ListMulticastGroupsByFuotaTaskResponse (Prelude.Maybe Prelude.Text)
listMulticastGroupsByFuotaTaskResponse_nextToken = Lens.lens (\ListMulticastGroupsByFuotaTaskResponse' {nextToken} -> nextToken) (\s@ListMulticastGroupsByFuotaTaskResponse' {} a -> s {nextToken = a} :: ListMulticastGroupsByFuotaTaskResponse)

-- | The response's http status code.
listMulticastGroupsByFuotaTaskResponse_httpStatus :: Lens.Lens' ListMulticastGroupsByFuotaTaskResponse Prelude.Int
listMulticastGroupsByFuotaTaskResponse_httpStatus = Lens.lens (\ListMulticastGroupsByFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@ListMulticastGroupsByFuotaTaskResponse' {} a -> s {httpStatus = a} :: ListMulticastGroupsByFuotaTaskResponse)

instance
  Prelude.NFData
    ListMulticastGroupsByFuotaTaskResponse
  where
  rnf ListMulticastGroupsByFuotaTaskResponse' {..} =
    Prelude.rnf multicastGroupList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
