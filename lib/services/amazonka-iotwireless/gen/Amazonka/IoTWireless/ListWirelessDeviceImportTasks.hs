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
-- Module      : Amazonka.IoTWireless.ListWirelessDeviceImportTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List wireless devices that have been added to an import task.
module Amazonka.IoTWireless.ListWirelessDeviceImportTasks
  ( -- * Creating a Request
    ListWirelessDeviceImportTasks (..),
    newListWirelessDeviceImportTasks,

    -- * Request Lenses
    listWirelessDeviceImportTasks_maxResults,
    listWirelessDeviceImportTasks_nextToken,

    -- * Destructuring the Response
    ListWirelessDeviceImportTasksResponse (..),
    newListWirelessDeviceImportTasksResponse,

    -- * Response Lenses
    listWirelessDeviceImportTasksResponse_nextToken,
    listWirelessDeviceImportTasksResponse_wirelessDeviceImportTaskList,
    listWirelessDeviceImportTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWirelessDeviceImportTasks' smart constructor.
data ListWirelessDeviceImportTasks = ListWirelessDeviceImportTasks'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise @null@ to receive the first set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWirelessDeviceImportTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWirelessDeviceImportTasks_maxResults' - Undocumented member.
--
-- 'nextToken', 'listWirelessDeviceImportTasks_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
newListWirelessDeviceImportTasks ::
  ListWirelessDeviceImportTasks
newListWirelessDeviceImportTasks =
  ListWirelessDeviceImportTasks'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listWirelessDeviceImportTasks_maxResults :: Lens.Lens' ListWirelessDeviceImportTasks (Prelude.Maybe Prelude.Natural)
listWirelessDeviceImportTasks_maxResults = Lens.lens (\ListWirelessDeviceImportTasks' {maxResults} -> maxResults) (\s@ListWirelessDeviceImportTasks' {} a -> s {maxResults = a} :: ListWirelessDeviceImportTasks)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
listWirelessDeviceImportTasks_nextToken :: Lens.Lens' ListWirelessDeviceImportTasks (Prelude.Maybe Prelude.Text)
listWirelessDeviceImportTasks_nextToken = Lens.lens (\ListWirelessDeviceImportTasks' {nextToken} -> nextToken) (\s@ListWirelessDeviceImportTasks' {} a -> s {nextToken = a} :: ListWirelessDeviceImportTasks)

instance
  Core.AWSRequest
    ListWirelessDeviceImportTasks
  where
  type
    AWSResponse ListWirelessDeviceImportTasks =
      ListWirelessDeviceImportTasksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWirelessDeviceImportTasksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "WirelessDeviceImportTaskList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListWirelessDeviceImportTasks
  where
  hashWithSalt _salt ListWirelessDeviceImportTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWirelessDeviceImportTasks where
  rnf ListWirelessDeviceImportTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWirelessDeviceImportTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListWirelessDeviceImportTasks where
  toPath =
    Prelude.const "/wireless_device_import_tasks"

instance Data.ToQuery ListWirelessDeviceImportTasks where
  toQuery ListWirelessDeviceImportTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListWirelessDeviceImportTasksResponse' smart constructor.
data ListWirelessDeviceImportTasksResponse = ListWirelessDeviceImportTasksResponse'
  { -- | The token to use to get the next set of results, or @null@ if there are
    -- no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of import tasks and summary information of onboarding status of
    -- devices in each import task.
    wirelessDeviceImportTaskList :: Prelude.Maybe [WirelessDeviceImportTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWirelessDeviceImportTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWirelessDeviceImportTasksResponse_nextToken' - The token to use to get the next set of results, or @null@ if there are
-- no additional results.
--
-- 'wirelessDeviceImportTaskList', 'listWirelessDeviceImportTasksResponse_wirelessDeviceImportTaskList' - List of import tasks and summary information of onboarding status of
-- devices in each import task.
--
-- 'httpStatus', 'listWirelessDeviceImportTasksResponse_httpStatus' - The response's http status code.
newListWirelessDeviceImportTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWirelessDeviceImportTasksResponse
newListWirelessDeviceImportTasksResponse pHttpStatus_ =
  ListWirelessDeviceImportTasksResponse'
    { nextToken =
        Prelude.Nothing,
      wirelessDeviceImportTaskList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or @null@ if there are
-- no additional results.
listWirelessDeviceImportTasksResponse_nextToken :: Lens.Lens' ListWirelessDeviceImportTasksResponse (Prelude.Maybe Prelude.Text)
listWirelessDeviceImportTasksResponse_nextToken = Lens.lens (\ListWirelessDeviceImportTasksResponse' {nextToken} -> nextToken) (\s@ListWirelessDeviceImportTasksResponse' {} a -> s {nextToken = a} :: ListWirelessDeviceImportTasksResponse)

-- | List of import tasks and summary information of onboarding status of
-- devices in each import task.
listWirelessDeviceImportTasksResponse_wirelessDeviceImportTaskList :: Lens.Lens' ListWirelessDeviceImportTasksResponse (Prelude.Maybe [WirelessDeviceImportTask])
listWirelessDeviceImportTasksResponse_wirelessDeviceImportTaskList = Lens.lens (\ListWirelessDeviceImportTasksResponse' {wirelessDeviceImportTaskList} -> wirelessDeviceImportTaskList) (\s@ListWirelessDeviceImportTasksResponse' {} a -> s {wirelessDeviceImportTaskList = a} :: ListWirelessDeviceImportTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWirelessDeviceImportTasksResponse_httpStatus :: Lens.Lens' ListWirelessDeviceImportTasksResponse Prelude.Int
listWirelessDeviceImportTasksResponse_httpStatus = Lens.lens (\ListWirelessDeviceImportTasksResponse' {httpStatus} -> httpStatus) (\s@ListWirelessDeviceImportTasksResponse' {} a -> s {httpStatus = a} :: ListWirelessDeviceImportTasksResponse)

instance
  Prelude.NFData
    ListWirelessDeviceImportTasksResponse
  where
  rnf ListWirelessDeviceImportTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf wirelessDeviceImportTaskList
      `Prelude.seq` Prelude.rnf httpStatus
