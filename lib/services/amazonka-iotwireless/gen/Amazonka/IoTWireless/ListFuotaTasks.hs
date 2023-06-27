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
-- Module      : Amazonka.IoTWireless.ListFuotaTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the FUOTA tasks registered to your AWS account.
module Amazonka.IoTWireless.ListFuotaTasks
  ( -- * Creating a Request
    ListFuotaTasks (..),
    newListFuotaTasks,

    -- * Request Lenses
    listFuotaTasks_maxResults,
    listFuotaTasks_nextToken,

    -- * Destructuring the Response
    ListFuotaTasksResponse (..),
    newListFuotaTasksResponse,

    -- * Response Lenses
    listFuotaTasksResponse_fuotaTaskList,
    listFuotaTasksResponse_nextToken,
    listFuotaTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFuotaTasks' smart constructor.
data ListFuotaTasks = ListFuotaTasks'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFuotaTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFuotaTasks_maxResults' - Undocumented member.
--
-- 'nextToken', 'listFuotaTasks_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
newListFuotaTasks ::
  ListFuotaTasks
newListFuotaTasks =
  ListFuotaTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listFuotaTasks_maxResults :: Lens.Lens' ListFuotaTasks (Prelude.Maybe Prelude.Natural)
listFuotaTasks_maxResults = Lens.lens (\ListFuotaTasks' {maxResults} -> maxResults) (\s@ListFuotaTasks' {} a -> s {maxResults = a} :: ListFuotaTasks)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listFuotaTasks_nextToken :: Lens.Lens' ListFuotaTasks (Prelude.Maybe Prelude.Text)
listFuotaTasks_nextToken = Lens.lens (\ListFuotaTasks' {nextToken} -> nextToken) (\s@ListFuotaTasks' {} a -> s {nextToken = a} :: ListFuotaTasks)

instance Core.AWSRequest ListFuotaTasks where
  type
    AWSResponse ListFuotaTasks =
      ListFuotaTasksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFuotaTasksResponse'
            Prelude.<$> (x Data..?> "FuotaTaskList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFuotaTasks where
  hashWithSalt _salt ListFuotaTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFuotaTasks where
  rnf ListFuotaTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFuotaTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFuotaTasks where
  toPath = Prelude.const "/fuota-tasks"

instance Data.ToQuery ListFuotaTasks where
  toQuery ListFuotaTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFuotaTasksResponse' smart constructor.
data ListFuotaTasksResponse = ListFuotaTasksResponse'
  { fuotaTaskList :: Prelude.Maybe [FuotaTask],
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFuotaTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fuotaTaskList', 'listFuotaTasksResponse_fuotaTaskList' - Undocumented member.
--
-- 'nextToken', 'listFuotaTasksResponse_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'httpStatus', 'listFuotaTasksResponse_httpStatus' - The response's http status code.
newListFuotaTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFuotaTasksResponse
newListFuotaTasksResponse pHttpStatus_ =
  ListFuotaTasksResponse'
    { fuotaTaskList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listFuotaTasksResponse_fuotaTaskList :: Lens.Lens' ListFuotaTasksResponse (Prelude.Maybe [FuotaTask])
listFuotaTasksResponse_fuotaTaskList = Lens.lens (\ListFuotaTasksResponse' {fuotaTaskList} -> fuotaTaskList) (\s@ListFuotaTasksResponse' {} a -> s {fuotaTaskList = a} :: ListFuotaTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listFuotaTasksResponse_nextToken :: Lens.Lens' ListFuotaTasksResponse (Prelude.Maybe Prelude.Text)
listFuotaTasksResponse_nextToken = Lens.lens (\ListFuotaTasksResponse' {nextToken} -> nextToken) (\s@ListFuotaTasksResponse' {} a -> s {nextToken = a} :: ListFuotaTasksResponse)

-- | The response's http status code.
listFuotaTasksResponse_httpStatus :: Lens.Lens' ListFuotaTasksResponse Prelude.Int
listFuotaTasksResponse_httpStatus = Lens.lens (\ListFuotaTasksResponse' {httpStatus} -> httpStatus) (\s@ListFuotaTasksResponse' {} a -> s {httpStatus = a} :: ListFuotaTasksResponse)

instance Prelude.NFData ListFuotaTasksResponse where
  rnf ListFuotaTasksResponse' {..} =
    Prelude.rnf fuotaTaskList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
