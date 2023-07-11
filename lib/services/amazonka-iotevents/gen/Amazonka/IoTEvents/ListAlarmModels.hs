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
-- Module      : Amazonka.IoTEvents.ListAlarmModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the alarm models that you created. The operation returns only the
-- metadata associated with each alarm model.
module Amazonka.IoTEvents.ListAlarmModels
  ( -- * Creating a Request
    ListAlarmModels (..),
    newListAlarmModels,

    -- * Request Lenses
    listAlarmModels_maxResults,
    listAlarmModels_nextToken,

    -- * Destructuring the Response
    ListAlarmModelsResponse (..),
    newListAlarmModelsResponse,

    -- * Response Lenses
    listAlarmModelsResponse_alarmModelSummaries,
    listAlarmModelsResponse_nextToken,
    listAlarmModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlarmModels' smart constructor.
data ListAlarmModels = ListAlarmModels'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAlarmModels_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listAlarmModels_nextToken' - The token that you can use to return the next set of results.
newListAlarmModels ::
  ListAlarmModels
newListAlarmModels =
  ListAlarmModels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listAlarmModels_maxResults :: Lens.Lens' ListAlarmModels (Prelude.Maybe Prelude.Natural)
listAlarmModels_maxResults = Lens.lens (\ListAlarmModels' {maxResults} -> maxResults) (\s@ListAlarmModels' {} a -> s {maxResults = a} :: ListAlarmModels)

-- | The token that you can use to return the next set of results.
listAlarmModels_nextToken :: Lens.Lens' ListAlarmModels (Prelude.Maybe Prelude.Text)
listAlarmModels_nextToken = Lens.lens (\ListAlarmModels' {nextToken} -> nextToken) (\s@ListAlarmModels' {} a -> s {nextToken = a} :: ListAlarmModels)

instance Core.AWSRequest ListAlarmModels where
  type
    AWSResponse ListAlarmModels =
      ListAlarmModelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlarmModelsResponse'
            Prelude.<$> ( x
                            Data..?> "alarmModelSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlarmModels where
  hashWithSalt _salt ListAlarmModels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAlarmModels where
  rnf ListAlarmModels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAlarmModels where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAlarmModels where
  toPath = Prelude.const "/alarm-models"

instance Data.ToQuery ListAlarmModels where
  toQuery ListAlarmModels' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAlarmModelsResponse' smart constructor.
data ListAlarmModelsResponse = ListAlarmModelsResponse'
  { -- | A list that summarizes each alarm model.
    alarmModelSummaries :: Prelude.Maybe [AlarmModelSummary],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelSummaries', 'listAlarmModelsResponse_alarmModelSummaries' - A list that summarizes each alarm model.
--
-- 'nextToken', 'listAlarmModelsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'listAlarmModelsResponse_httpStatus' - The response's http status code.
newListAlarmModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlarmModelsResponse
newListAlarmModelsResponse pHttpStatus_ =
  ListAlarmModelsResponse'
    { alarmModelSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each alarm model.
listAlarmModelsResponse_alarmModelSummaries :: Lens.Lens' ListAlarmModelsResponse (Prelude.Maybe [AlarmModelSummary])
listAlarmModelsResponse_alarmModelSummaries = Lens.lens (\ListAlarmModelsResponse' {alarmModelSummaries} -> alarmModelSummaries) (\s@ListAlarmModelsResponse' {} a -> s {alarmModelSummaries = a} :: ListAlarmModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listAlarmModelsResponse_nextToken :: Lens.Lens' ListAlarmModelsResponse (Prelude.Maybe Prelude.Text)
listAlarmModelsResponse_nextToken = Lens.lens (\ListAlarmModelsResponse' {nextToken} -> nextToken) (\s@ListAlarmModelsResponse' {} a -> s {nextToken = a} :: ListAlarmModelsResponse)

-- | The response's http status code.
listAlarmModelsResponse_httpStatus :: Lens.Lens' ListAlarmModelsResponse Prelude.Int
listAlarmModelsResponse_httpStatus = Lens.lens (\ListAlarmModelsResponse' {httpStatus} -> httpStatus) (\s@ListAlarmModelsResponse' {} a -> s {httpStatus = a} :: ListAlarmModelsResponse)

instance Prelude.NFData ListAlarmModelsResponse where
  rnf ListAlarmModelsResponse' {..} =
    Prelude.rnf alarmModelSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
