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
-- Module      : Amazonka.Rum.ListAppMonitors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon CloudWatch RUM app monitors in the account.
--
-- This operation returns paginated results.
module Amazonka.Rum.ListAppMonitors
  ( -- * Creating a Request
    ListAppMonitors (..),
    newListAppMonitors,

    -- * Request Lenses
    listAppMonitors_maxResults,
    listAppMonitors_nextToken,

    -- * Destructuring the Response
    ListAppMonitorsResponse (..),
    newListAppMonitorsResponse,

    -- * Response Lenses
    listAppMonitorsResponse_appMonitorSummaries,
    listAppMonitorsResponse_nextToken,
    listAppMonitorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newListAppMonitors' smart constructor.
data ListAppMonitors = ListAppMonitors'
  { -- | The maximum number of results to return in one operation. The default is
    -- 50. The maximum that you can specify is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use the token returned by the previous operation to request the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppMonitors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppMonitors_maxResults' - The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
--
-- 'nextToken', 'listAppMonitors_nextToken' - Use the token returned by the previous operation to request the next
-- page of results.
newListAppMonitors ::
  ListAppMonitors
newListAppMonitors =
  ListAppMonitors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
listAppMonitors_maxResults :: Lens.Lens' ListAppMonitors (Prelude.Maybe Prelude.Natural)
listAppMonitors_maxResults = Lens.lens (\ListAppMonitors' {maxResults} -> maxResults) (\s@ListAppMonitors' {} a -> s {maxResults = a} :: ListAppMonitors)

-- | Use the token returned by the previous operation to request the next
-- page of results.
listAppMonitors_nextToken :: Lens.Lens' ListAppMonitors (Prelude.Maybe Prelude.Text)
listAppMonitors_nextToken = Lens.lens (\ListAppMonitors' {nextToken} -> nextToken) (\s@ListAppMonitors' {} a -> s {nextToken = a} :: ListAppMonitors)

instance Core.AWSPager ListAppMonitors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppMonitorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppMonitorsResponse_appMonitorSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAppMonitors_nextToken
          Lens..~ rs
          Lens.^? listAppMonitorsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAppMonitors where
  type
    AWSResponse ListAppMonitors =
      ListAppMonitorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppMonitorsResponse'
            Prelude.<$> ( x Data..?> "AppMonitorSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppMonitors where
  hashWithSalt _salt ListAppMonitors' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAppMonitors where
  rnf ListAppMonitors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAppMonitors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppMonitors where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListAppMonitors where
  toPath = Prelude.const "/appmonitors"

instance Data.ToQuery ListAppMonitors where
  toQuery ListAppMonitors' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAppMonitorsResponse' smart constructor.
data ListAppMonitorsResponse = ListAppMonitorsResponse'
  { -- | An array of structures that contain information about the returned app
    -- monitors.
    appMonitorSummaries :: Prelude.Maybe [AppMonitorSummary],
    -- | A token that you can use in a subsequent operation to retrieve the next
    -- set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppMonitorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appMonitorSummaries', 'listAppMonitorsResponse_appMonitorSummaries' - An array of structures that contain information about the returned app
-- monitors.
--
-- 'nextToken', 'listAppMonitorsResponse_nextToken' - A token that you can use in a subsequent operation to retrieve the next
-- set of results.
--
-- 'httpStatus', 'listAppMonitorsResponse_httpStatus' - The response's http status code.
newListAppMonitorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppMonitorsResponse
newListAppMonitorsResponse pHttpStatus_ =
  ListAppMonitorsResponse'
    { appMonitorSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that contain information about the returned app
-- monitors.
listAppMonitorsResponse_appMonitorSummaries :: Lens.Lens' ListAppMonitorsResponse (Prelude.Maybe [AppMonitorSummary])
listAppMonitorsResponse_appMonitorSummaries = Lens.lens (\ListAppMonitorsResponse' {appMonitorSummaries} -> appMonitorSummaries) (\s@ListAppMonitorsResponse' {} a -> s {appMonitorSummaries = a} :: ListAppMonitorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that you can use in a subsequent operation to retrieve the next
-- set of results.
listAppMonitorsResponse_nextToken :: Lens.Lens' ListAppMonitorsResponse (Prelude.Maybe Prelude.Text)
listAppMonitorsResponse_nextToken = Lens.lens (\ListAppMonitorsResponse' {nextToken} -> nextToken) (\s@ListAppMonitorsResponse' {} a -> s {nextToken = a} :: ListAppMonitorsResponse)

-- | The response's http status code.
listAppMonitorsResponse_httpStatus :: Lens.Lens' ListAppMonitorsResponse Prelude.Int
listAppMonitorsResponse_httpStatus = Lens.lens (\ListAppMonitorsResponse' {httpStatus} -> httpStatus) (\s@ListAppMonitorsResponse' {} a -> s {httpStatus = a} :: ListAppMonitorsResponse)

instance Prelude.NFData ListAppMonitorsResponse where
  rnf ListAppMonitorsResponse' {..} =
    Prelude.rnf appMonitorSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
