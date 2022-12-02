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
-- Module      : Amazonka.IoTEvents.ListAlarmModelVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of an alarm model. The operation returns only the
-- metadata associated with each alarm model version.
module Amazonka.IoTEvents.ListAlarmModelVersions
  ( -- * Creating a Request
    ListAlarmModelVersions (..),
    newListAlarmModelVersions,

    -- * Request Lenses
    listAlarmModelVersions_nextToken,
    listAlarmModelVersions_maxResults,
    listAlarmModelVersions_alarmModelName,

    -- * Destructuring the Response
    ListAlarmModelVersionsResponse (..),
    newListAlarmModelVersionsResponse,

    -- * Response Lenses
    listAlarmModelVersionsResponse_nextToken,
    listAlarmModelVersionsResponse_alarmModelVersionSummaries,
    listAlarmModelVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlarmModelVersions' smart constructor.
data ListAlarmModelVersions = ListAlarmModelVersions'
  { -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmModelVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlarmModelVersions_nextToken' - The token that you can use to return the next set of results.
--
-- 'maxResults', 'listAlarmModelVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'alarmModelName', 'listAlarmModelVersions_alarmModelName' - The name of the alarm model.
newListAlarmModelVersions ::
  -- | 'alarmModelName'
  Prelude.Text ->
  ListAlarmModelVersions
newListAlarmModelVersions pAlarmModelName_ =
  ListAlarmModelVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      alarmModelName = pAlarmModelName_
    }

-- | The token that you can use to return the next set of results.
listAlarmModelVersions_nextToken :: Lens.Lens' ListAlarmModelVersions (Prelude.Maybe Prelude.Text)
listAlarmModelVersions_nextToken = Lens.lens (\ListAlarmModelVersions' {nextToken} -> nextToken) (\s@ListAlarmModelVersions' {} a -> s {nextToken = a} :: ListAlarmModelVersions)

-- | The maximum number of results to be returned per request.
listAlarmModelVersions_maxResults :: Lens.Lens' ListAlarmModelVersions (Prelude.Maybe Prelude.Natural)
listAlarmModelVersions_maxResults = Lens.lens (\ListAlarmModelVersions' {maxResults} -> maxResults) (\s@ListAlarmModelVersions' {} a -> s {maxResults = a} :: ListAlarmModelVersions)

-- | The name of the alarm model.
listAlarmModelVersions_alarmModelName :: Lens.Lens' ListAlarmModelVersions Prelude.Text
listAlarmModelVersions_alarmModelName = Lens.lens (\ListAlarmModelVersions' {alarmModelName} -> alarmModelName) (\s@ListAlarmModelVersions' {} a -> s {alarmModelName = a} :: ListAlarmModelVersions)

instance Core.AWSRequest ListAlarmModelVersions where
  type
    AWSResponse ListAlarmModelVersions =
      ListAlarmModelVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlarmModelVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "alarmModelVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlarmModelVersions where
  hashWithSalt _salt ListAlarmModelVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData ListAlarmModelVersions where
  rnf ListAlarmModelVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf alarmModelName

instance Data.ToHeaders ListAlarmModelVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAlarmModelVersions where
  toPath ListAlarmModelVersions' {..} =
    Prelude.mconcat
      [ "/alarm-models/",
        Data.toBS alarmModelName,
        "/versions"
      ]

instance Data.ToQuery ListAlarmModelVersions where
  toQuery ListAlarmModelVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListAlarmModelVersionsResponse' smart constructor.
data ListAlarmModelVersionsResponse = ListAlarmModelVersionsResponse'
  { -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each alarm model version.
    alarmModelVersionSummaries :: Prelude.Maybe [AlarmModelVersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmModelVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlarmModelVersionsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'alarmModelVersionSummaries', 'listAlarmModelVersionsResponse_alarmModelVersionSummaries' - A list that summarizes each alarm model version.
--
-- 'httpStatus', 'listAlarmModelVersionsResponse_httpStatus' - The response's http status code.
newListAlarmModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlarmModelVersionsResponse
newListAlarmModelVersionsResponse pHttpStatus_ =
  ListAlarmModelVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      alarmModelVersionSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listAlarmModelVersionsResponse_nextToken :: Lens.Lens' ListAlarmModelVersionsResponse (Prelude.Maybe Prelude.Text)
listAlarmModelVersionsResponse_nextToken = Lens.lens (\ListAlarmModelVersionsResponse' {nextToken} -> nextToken) (\s@ListAlarmModelVersionsResponse' {} a -> s {nextToken = a} :: ListAlarmModelVersionsResponse)

-- | A list that summarizes each alarm model version.
listAlarmModelVersionsResponse_alarmModelVersionSummaries :: Lens.Lens' ListAlarmModelVersionsResponse (Prelude.Maybe [AlarmModelVersionSummary])
listAlarmModelVersionsResponse_alarmModelVersionSummaries = Lens.lens (\ListAlarmModelVersionsResponse' {alarmModelVersionSummaries} -> alarmModelVersionSummaries) (\s@ListAlarmModelVersionsResponse' {} a -> s {alarmModelVersionSummaries = a} :: ListAlarmModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAlarmModelVersionsResponse_httpStatus :: Lens.Lens' ListAlarmModelVersionsResponse Prelude.Int
listAlarmModelVersionsResponse_httpStatus = Lens.lens (\ListAlarmModelVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAlarmModelVersionsResponse' {} a -> s {httpStatus = a} :: ListAlarmModelVersionsResponse)

instance
  Prelude.NFData
    ListAlarmModelVersionsResponse
  where
  rnf ListAlarmModelVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf alarmModelVersionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
