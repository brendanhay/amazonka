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
-- Module      : Network.AWS.IoTEvents.ListAlarmModelVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of an alarm model. The operation returns only the
-- metadata associated with each alarm model version.
module Network.AWS.IoTEvents.ListAlarmModelVersions
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
    listAlarmModelVersionsResponse_alarmModelVersionSummaries,
    listAlarmModelVersionsResponse_nextToken,
    listAlarmModelVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlarmModelVersionsResponse'
            Prelude.<$> ( x Core..?> "alarmModelVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlarmModelVersions

instance Prelude.NFData ListAlarmModelVersions

instance Core.ToHeaders ListAlarmModelVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAlarmModelVersions where
  toPath ListAlarmModelVersions' {..} =
    Prelude.mconcat
      [ "/alarm-models/",
        Core.toBS alarmModelName,
        "/versions"
      ]

instance Core.ToQuery ListAlarmModelVersions where
  toQuery ListAlarmModelVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAlarmModelVersionsResponse' smart constructor.
data ListAlarmModelVersionsResponse = ListAlarmModelVersionsResponse'
  { -- | A list that summarizes each alarm model version.
    alarmModelVersionSummaries :: Prelude.Maybe [AlarmModelVersionSummary],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'alarmModelVersionSummaries', 'listAlarmModelVersionsResponse_alarmModelVersionSummaries' - A list that summarizes each alarm model version.
--
-- 'nextToken', 'listAlarmModelVersionsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'listAlarmModelVersionsResponse_httpStatus' - The response's http status code.
newListAlarmModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlarmModelVersionsResponse
newListAlarmModelVersionsResponse pHttpStatus_ =
  ListAlarmModelVersionsResponse'
    { alarmModelVersionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each alarm model version.
listAlarmModelVersionsResponse_alarmModelVersionSummaries :: Lens.Lens' ListAlarmModelVersionsResponse (Prelude.Maybe [AlarmModelVersionSummary])
listAlarmModelVersionsResponse_alarmModelVersionSummaries = Lens.lens (\ListAlarmModelVersionsResponse' {alarmModelVersionSummaries} -> alarmModelVersionSummaries) (\s@ListAlarmModelVersionsResponse' {} a -> s {alarmModelVersionSummaries = a} :: ListAlarmModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listAlarmModelVersionsResponse_nextToken :: Lens.Lens' ListAlarmModelVersionsResponse (Prelude.Maybe Prelude.Text)
listAlarmModelVersionsResponse_nextToken = Lens.lens (\ListAlarmModelVersionsResponse' {nextToken} -> nextToken) (\s@ListAlarmModelVersionsResponse' {} a -> s {nextToken = a} :: ListAlarmModelVersionsResponse)

-- | The response's http status code.
listAlarmModelVersionsResponse_httpStatus :: Lens.Lens' ListAlarmModelVersionsResponse Prelude.Int
listAlarmModelVersionsResponse_httpStatus = Lens.lens (\ListAlarmModelVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAlarmModelVersionsResponse' {} a -> s {httpStatus = a} :: ListAlarmModelVersionsResponse)

instance
  Prelude.NFData
    ListAlarmModelVersionsResponse
