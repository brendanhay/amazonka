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
-- Module      : Amazonka.LookoutEquipment.ListInferenceEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all inference events that have been found for the specified
-- inference scheduler.
module Amazonka.LookoutEquipment.ListInferenceEvents
  ( -- * Creating a Request
    ListInferenceEvents (..),
    newListInferenceEvents,

    -- * Request Lenses
    listInferenceEvents_maxResults,
    listInferenceEvents_nextToken,
    listInferenceEvents_inferenceSchedulerName,
    listInferenceEvents_intervalStartTime,
    listInferenceEvents_intervalEndTime,

    -- * Destructuring the Response
    ListInferenceEventsResponse (..),
    newListInferenceEventsResponse,

    -- * Response Lenses
    listInferenceEventsResponse_inferenceEventSummaries,
    listInferenceEventsResponse_nextToken,
    listInferenceEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInferenceEvents' smart constructor.
data ListInferenceEvents = ListInferenceEvents'
  { -- | Specifies the maximum number of inference events to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque pagination token indicating where to continue the listing of
    -- inference events.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference scheduler for the inference events listed.
    inferenceSchedulerName :: Prelude.Text,
    -- | Lookout for Equipment will return all the inference events with an end
    -- time equal to or greater than the start time given.
    intervalStartTime :: Data.POSIX,
    -- | Returns all the inference events with an end start time equal to or
    -- greater than less than the end time given
    intervalEndTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInferenceEvents_maxResults' - Specifies the maximum number of inference events to list.
--
-- 'nextToken', 'listInferenceEvents_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference events.
--
-- 'inferenceSchedulerName', 'listInferenceEvents_inferenceSchedulerName' - The name of the inference scheduler for the inference events listed.
--
-- 'intervalStartTime', 'listInferenceEvents_intervalStartTime' - Lookout for Equipment will return all the inference events with an end
-- time equal to or greater than the start time given.
--
-- 'intervalEndTime', 'listInferenceEvents_intervalEndTime' - Returns all the inference events with an end start time equal to or
-- greater than less than the end time given
newListInferenceEvents ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  -- | 'intervalStartTime'
  Prelude.UTCTime ->
  -- | 'intervalEndTime'
  Prelude.UTCTime ->
  ListInferenceEvents
newListInferenceEvents
  pInferenceSchedulerName_
  pIntervalStartTime_
  pIntervalEndTime_ =
    ListInferenceEvents'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        inferenceSchedulerName = pInferenceSchedulerName_,
        intervalStartTime =
          Data._Time Lens.# pIntervalStartTime_,
        intervalEndTime =
          Data._Time Lens.# pIntervalEndTime_
      }

-- | Specifies the maximum number of inference events to list.
listInferenceEvents_maxResults :: Lens.Lens' ListInferenceEvents (Prelude.Maybe Prelude.Natural)
listInferenceEvents_maxResults = Lens.lens (\ListInferenceEvents' {maxResults} -> maxResults) (\s@ListInferenceEvents' {} a -> s {maxResults = a} :: ListInferenceEvents)

-- | An opaque pagination token indicating where to continue the listing of
-- inference events.
listInferenceEvents_nextToken :: Lens.Lens' ListInferenceEvents (Prelude.Maybe Prelude.Text)
listInferenceEvents_nextToken = Lens.lens (\ListInferenceEvents' {nextToken} -> nextToken) (\s@ListInferenceEvents' {} a -> s {nextToken = a} :: ListInferenceEvents)

-- | The name of the inference scheduler for the inference events listed.
listInferenceEvents_inferenceSchedulerName :: Lens.Lens' ListInferenceEvents Prelude.Text
listInferenceEvents_inferenceSchedulerName = Lens.lens (\ListInferenceEvents' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@ListInferenceEvents' {} a -> s {inferenceSchedulerName = a} :: ListInferenceEvents)

-- | Lookout for Equipment will return all the inference events with an end
-- time equal to or greater than the start time given.
listInferenceEvents_intervalStartTime :: Lens.Lens' ListInferenceEvents Prelude.UTCTime
listInferenceEvents_intervalStartTime = Lens.lens (\ListInferenceEvents' {intervalStartTime} -> intervalStartTime) (\s@ListInferenceEvents' {} a -> s {intervalStartTime = a} :: ListInferenceEvents) Prelude.. Data._Time

-- | Returns all the inference events with an end start time equal to or
-- greater than less than the end time given
listInferenceEvents_intervalEndTime :: Lens.Lens' ListInferenceEvents Prelude.UTCTime
listInferenceEvents_intervalEndTime = Lens.lens (\ListInferenceEvents' {intervalEndTime} -> intervalEndTime) (\s@ListInferenceEvents' {} a -> s {intervalEndTime = a} :: ListInferenceEvents) Prelude.. Data._Time

instance Core.AWSRequest ListInferenceEvents where
  type
    AWSResponse ListInferenceEvents =
      ListInferenceEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceEventsResponse'
            Prelude.<$> ( x
                            Data..?> "InferenceEventSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInferenceEvents where
  hashWithSalt _salt ListInferenceEvents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` inferenceSchedulerName
      `Prelude.hashWithSalt` intervalStartTime
      `Prelude.hashWithSalt` intervalEndTime

instance Prelude.NFData ListInferenceEvents where
  rnf ListInferenceEvents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf intervalStartTime
      `Prelude.seq` Prelude.rnf intervalEndTime

instance Data.ToHeaders ListInferenceEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListInferenceEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInferenceEvents where
  toJSON ListInferenceEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "InferenceSchedulerName"
                  Data..= inferenceSchedulerName
              ),
            Prelude.Just
              ("IntervalStartTime" Data..= intervalStartTime),
            Prelude.Just
              ("IntervalEndTime" Data..= intervalEndTime)
          ]
      )

instance Data.ToPath ListInferenceEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInferenceEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceEventsResponse' smart constructor.
data ListInferenceEventsResponse = ListInferenceEventsResponse'
  { -- | Provides an array of information about the individual inference events
    -- returned from the @ListInferenceEvents@ operation, including scheduler
    -- used, event start time, event end time, diagnostics, and so on.
    inferenceEventSummaries :: Prelude.Maybe [InferenceEventSummary],
    -- | An opaque pagination token indicating where to continue the listing of
    -- inference executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceEventSummaries', 'listInferenceEventsResponse_inferenceEventSummaries' - Provides an array of information about the individual inference events
-- returned from the @ListInferenceEvents@ operation, including scheduler
-- used, event start time, event end time, diagnostics, and so on.
--
-- 'nextToken', 'listInferenceEventsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference executions.
--
-- 'httpStatus', 'listInferenceEventsResponse_httpStatus' - The response's http status code.
newListInferenceEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceEventsResponse
newListInferenceEventsResponse pHttpStatus_ =
  ListInferenceEventsResponse'
    { inferenceEventSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an array of information about the individual inference events
-- returned from the @ListInferenceEvents@ operation, including scheduler
-- used, event start time, event end time, diagnostics, and so on.
listInferenceEventsResponse_inferenceEventSummaries :: Lens.Lens' ListInferenceEventsResponse (Prelude.Maybe [InferenceEventSummary])
listInferenceEventsResponse_inferenceEventSummaries = Lens.lens (\ListInferenceEventsResponse' {inferenceEventSummaries} -> inferenceEventSummaries) (\s@ListInferenceEventsResponse' {} a -> s {inferenceEventSummaries = a} :: ListInferenceEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque pagination token indicating where to continue the listing of
-- inference executions.
listInferenceEventsResponse_nextToken :: Lens.Lens' ListInferenceEventsResponse (Prelude.Maybe Prelude.Text)
listInferenceEventsResponse_nextToken = Lens.lens (\ListInferenceEventsResponse' {nextToken} -> nextToken) (\s@ListInferenceEventsResponse' {} a -> s {nextToken = a} :: ListInferenceEventsResponse)

-- | The response's http status code.
listInferenceEventsResponse_httpStatus :: Lens.Lens' ListInferenceEventsResponse Prelude.Int
listInferenceEventsResponse_httpStatus = Lens.lens (\ListInferenceEventsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceEventsResponse' {} a -> s {httpStatus = a} :: ListInferenceEventsResponse)

instance Prelude.NFData ListInferenceEventsResponse where
  rnf ListInferenceEventsResponse' {..} =
    Prelude.rnf inferenceEventSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
