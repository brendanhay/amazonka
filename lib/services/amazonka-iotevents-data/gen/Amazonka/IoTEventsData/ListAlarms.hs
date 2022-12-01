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
-- Module      : Amazonka.IoTEventsData.ListAlarms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more alarms. The operation returns only the metadata
-- associated with each alarm.
module Amazonka.IoTEventsData.ListAlarms
  ( -- * Creating a Request
    ListAlarms (..),
    newListAlarms,

    -- * Request Lenses
    listAlarms_nextToken,
    listAlarms_maxResults,
    listAlarms_alarmModelName,

    -- * Destructuring the Response
    ListAlarmsResponse (..),
    newListAlarmsResponse,

    -- * Response Lenses
    listAlarmsResponse_nextToken,
    listAlarmsResponse_alarmSummaries,
    listAlarmsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlarms' smart constructor.
data ListAlarms = ListAlarms'
  { -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlarms_nextToken' - The token that you can use to return the next set of results.
--
-- 'maxResults', 'listAlarms_maxResults' - The maximum number of results to be returned per request.
--
-- 'alarmModelName', 'listAlarms_alarmModelName' - The name of the alarm model.
newListAlarms ::
  -- | 'alarmModelName'
  Prelude.Text ->
  ListAlarms
newListAlarms pAlarmModelName_ =
  ListAlarms'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      alarmModelName = pAlarmModelName_
    }

-- | The token that you can use to return the next set of results.
listAlarms_nextToken :: Lens.Lens' ListAlarms (Prelude.Maybe Prelude.Text)
listAlarms_nextToken = Lens.lens (\ListAlarms' {nextToken} -> nextToken) (\s@ListAlarms' {} a -> s {nextToken = a} :: ListAlarms)

-- | The maximum number of results to be returned per request.
listAlarms_maxResults :: Lens.Lens' ListAlarms (Prelude.Maybe Prelude.Natural)
listAlarms_maxResults = Lens.lens (\ListAlarms' {maxResults} -> maxResults) (\s@ListAlarms' {} a -> s {maxResults = a} :: ListAlarms)

-- | The name of the alarm model.
listAlarms_alarmModelName :: Lens.Lens' ListAlarms Prelude.Text
listAlarms_alarmModelName = Lens.lens (\ListAlarms' {alarmModelName} -> alarmModelName) (\s@ListAlarms' {} a -> s {alarmModelName = a} :: ListAlarms)

instance Core.AWSRequest ListAlarms where
  type AWSResponse ListAlarms = ListAlarmsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlarmsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "alarmSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlarms where
  hashWithSalt _salt ListAlarms' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData ListAlarms where
  rnf ListAlarms' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf alarmModelName

instance Core.ToHeaders ListAlarms where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAlarms where
  toPath ListAlarms' {..} =
    Prelude.mconcat
      ["/alarms/", Core.toBS alarmModelName]

instance Core.ToQuery ListAlarms where
  toQuery ListAlarms' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAlarmsResponse' smart constructor.
data ListAlarmsResponse = ListAlarmsResponse'
  { -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each alarm.
    alarmSummaries :: Prelude.Maybe [AlarmSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlarmsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'alarmSummaries', 'listAlarmsResponse_alarmSummaries' - A list that summarizes each alarm.
--
-- 'httpStatus', 'listAlarmsResponse_httpStatus' - The response's http status code.
newListAlarmsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlarmsResponse
newListAlarmsResponse pHttpStatus_ =
  ListAlarmsResponse'
    { nextToken = Prelude.Nothing,
      alarmSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listAlarmsResponse_nextToken :: Lens.Lens' ListAlarmsResponse (Prelude.Maybe Prelude.Text)
listAlarmsResponse_nextToken = Lens.lens (\ListAlarmsResponse' {nextToken} -> nextToken) (\s@ListAlarmsResponse' {} a -> s {nextToken = a} :: ListAlarmsResponse)

-- | A list that summarizes each alarm.
listAlarmsResponse_alarmSummaries :: Lens.Lens' ListAlarmsResponse (Prelude.Maybe [AlarmSummary])
listAlarmsResponse_alarmSummaries = Lens.lens (\ListAlarmsResponse' {alarmSummaries} -> alarmSummaries) (\s@ListAlarmsResponse' {} a -> s {alarmSummaries = a} :: ListAlarmsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAlarmsResponse_httpStatus :: Lens.Lens' ListAlarmsResponse Prelude.Int
listAlarmsResponse_httpStatus = Lens.lens (\ListAlarmsResponse' {httpStatus} -> httpStatus) (\s@ListAlarmsResponse' {} a -> s {httpStatus = a} :: ListAlarmsResponse)

instance Prelude.NFData ListAlarmsResponse where
  rnf ListAlarmsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf alarmSummaries
      `Prelude.seq` Prelude.rnf httpStatus
