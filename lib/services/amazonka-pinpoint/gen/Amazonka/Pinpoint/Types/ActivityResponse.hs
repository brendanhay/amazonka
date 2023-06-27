{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.ActivityResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ActivityResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an activity that was performed by a campaign.
--
-- /See:/ 'newActivityResponse' smart constructor.
data ActivityResponse = ActivityResponse'
  { -- | The actual time, in ISO 8601 format, when the activity was marked
    -- CANCELLED or COMPLETED.
    end :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that contains metrics relating to the campaign execution
    -- for this campaign activity. For information about the structure and
    -- contents of the results, see
    -- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
    -- in the /Amazon Pinpoint Developer Guide/.
    executionMetrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether the activity succeeded. Possible values are SUCCESS
    -- and FAIL.
    result :: Prelude.Maybe Prelude.Text,
    -- | The scheduled start time, in ISO 8601 format, for the activity.
    scheduledStart :: Prelude.Maybe Prelude.Text,
    -- | The actual start time, in ISO 8601 format, of the activity.
    start :: Prelude.Maybe Prelude.Text,
    -- | The current status of the activity. Possible values are: PENDING,
    -- INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
    state :: Prelude.Maybe Prelude.Text,
    -- | The total number of endpoints that the campaign successfully delivered
    -- messages to.
    successfulEndpointCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of time zones that were completed.
    timezonesCompletedCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of unique time zones that are in the segment for the
    -- campaign.
    timezonesTotalCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of endpoints that the campaign attempted to deliver
    -- messages to.
    totalEndpointCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the campaign treatment that the activity
    -- applies to. A treatment is a variation of a campaign that\'s used for
    -- A\/B testing of a campaign.
    treatmentId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the campaign that the activity applies to.
    campaignId :: Prelude.Text,
    -- | The unique identifier for the activity.
    id :: Prelude.Text,
    -- | The unique identifier for the application that the campaign applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'activityResponse_end' - The actual time, in ISO 8601 format, when the activity was marked
-- CANCELLED or COMPLETED.
--
-- 'executionMetrics', 'activityResponse_executionMetrics' - A JSON object that contains metrics relating to the campaign execution
-- for this campaign activity. For information about the structure and
-- contents of the results, see
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- 'result', 'activityResponse_result' - Specifies whether the activity succeeded. Possible values are SUCCESS
-- and FAIL.
--
-- 'scheduledStart', 'activityResponse_scheduledStart' - The scheduled start time, in ISO 8601 format, for the activity.
--
-- 'start', 'activityResponse_start' - The actual start time, in ISO 8601 format, of the activity.
--
-- 'state', 'activityResponse_state' - The current status of the activity. Possible values are: PENDING,
-- INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
--
-- 'successfulEndpointCount', 'activityResponse_successfulEndpointCount' - The total number of endpoints that the campaign successfully delivered
-- messages to.
--
-- 'timezonesCompletedCount', 'activityResponse_timezonesCompletedCount' - The total number of time zones that were completed.
--
-- 'timezonesTotalCount', 'activityResponse_timezonesTotalCount' - The total number of unique time zones that are in the segment for the
-- campaign.
--
-- 'totalEndpointCount', 'activityResponse_totalEndpointCount' - The total number of endpoints that the campaign attempted to deliver
-- messages to.
--
-- 'treatmentId', 'activityResponse_treatmentId' - The unique identifier for the campaign treatment that the activity
-- applies to. A treatment is a variation of a campaign that\'s used for
-- A\/B testing of a campaign.
--
-- 'campaignId', 'activityResponse_campaignId' - The unique identifier for the campaign that the activity applies to.
--
-- 'id', 'activityResponse_id' - The unique identifier for the activity.
--
-- 'applicationId', 'activityResponse_applicationId' - The unique identifier for the application that the campaign applies to.
newActivityResponse ::
  -- | 'campaignId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  ActivityResponse
newActivityResponse pCampaignId_ pId_ pApplicationId_ =
  ActivityResponse'
    { end = Prelude.Nothing,
      executionMetrics = Prelude.Nothing,
      result = Prelude.Nothing,
      scheduledStart = Prelude.Nothing,
      start = Prelude.Nothing,
      state = Prelude.Nothing,
      successfulEndpointCount = Prelude.Nothing,
      timezonesCompletedCount = Prelude.Nothing,
      timezonesTotalCount = Prelude.Nothing,
      totalEndpointCount = Prelude.Nothing,
      treatmentId = Prelude.Nothing,
      campaignId = pCampaignId_,
      id = pId_,
      applicationId = pApplicationId_
    }

-- | The actual time, in ISO 8601 format, when the activity was marked
-- CANCELLED or COMPLETED.
activityResponse_end :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_end = Lens.lens (\ActivityResponse' {end} -> end) (\s@ActivityResponse' {} a -> s {end = a} :: ActivityResponse)

-- | A JSON object that contains metrics relating to the campaign execution
-- for this campaign activity. For information about the structure and
-- contents of the results, see
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
activityResponse_executionMetrics :: Lens.Lens' ActivityResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
activityResponse_executionMetrics = Lens.lens (\ActivityResponse' {executionMetrics} -> executionMetrics) (\s@ActivityResponse' {} a -> s {executionMetrics = a} :: ActivityResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the activity succeeded. Possible values are SUCCESS
-- and FAIL.
activityResponse_result :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_result = Lens.lens (\ActivityResponse' {result} -> result) (\s@ActivityResponse' {} a -> s {result = a} :: ActivityResponse)

-- | The scheduled start time, in ISO 8601 format, for the activity.
activityResponse_scheduledStart :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_scheduledStart = Lens.lens (\ActivityResponse' {scheduledStart} -> scheduledStart) (\s@ActivityResponse' {} a -> s {scheduledStart = a} :: ActivityResponse)

-- | The actual start time, in ISO 8601 format, of the activity.
activityResponse_start :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_start = Lens.lens (\ActivityResponse' {start} -> start) (\s@ActivityResponse' {} a -> s {start = a} :: ActivityResponse)

-- | The current status of the activity. Possible values are: PENDING,
-- INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
activityResponse_state :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_state = Lens.lens (\ActivityResponse' {state} -> state) (\s@ActivityResponse' {} a -> s {state = a} :: ActivityResponse)

-- | The total number of endpoints that the campaign successfully delivered
-- messages to.
activityResponse_successfulEndpointCount :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Int)
activityResponse_successfulEndpointCount = Lens.lens (\ActivityResponse' {successfulEndpointCount} -> successfulEndpointCount) (\s@ActivityResponse' {} a -> s {successfulEndpointCount = a} :: ActivityResponse)

-- | The total number of time zones that were completed.
activityResponse_timezonesCompletedCount :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Int)
activityResponse_timezonesCompletedCount = Lens.lens (\ActivityResponse' {timezonesCompletedCount} -> timezonesCompletedCount) (\s@ActivityResponse' {} a -> s {timezonesCompletedCount = a} :: ActivityResponse)

-- | The total number of unique time zones that are in the segment for the
-- campaign.
activityResponse_timezonesTotalCount :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Int)
activityResponse_timezonesTotalCount = Lens.lens (\ActivityResponse' {timezonesTotalCount} -> timezonesTotalCount) (\s@ActivityResponse' {} a -> s {timezonesTotalCount = a} :: ActivityResponse)

-- | The total number of endpoints that the campaign attempted to deliver
-- messages to.
activityResponse_totalEndpointCount :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Int)
activityResponse_totalEndpointCount = Lens.lens (\ActivityResponse' {totalEndpointCount} -> totalEndpointCount) (\s@ActivityResponse' {} a -> s {totalEndpointCount = a} :: ActivityResponse)

-- | The unique identifier for the campaign treatment that the activity
-- applies to. A treatment is a variation of a campaign that\'s used for
-- A\/B testing of a campaign.
activityResponse_treatmentId :: Lens.Lens' ActivityResponse (Prelude.Maybe Prelude.Text)
activityResponse_treatmentId = Lens.lens (\ActivityResponse' {treatmentId} -> treatmentId) (\s@ActivityResponse' {} a -> s {treatmentId = a} :: ActivityResponse)

-- | The unique identifier for the campaign that the activity applies to.
activityResponse_campaignId :: Lens.Lens' ActivityResponse Prelude.Text
activityResponse_campaignId = Lens.lens (\ActivityResponse' {campaignId} -> campaignId) (\s@ActivityResponse' {} a -> s {campaignId = a} :: ActivityResponse)

-- | The unique identifier for the activity.
activityResponse_id :: Lens.Lens' ActivityResponse Prelude.Text
activityResponse_id = Lens.lens (\ActivityResponse' {id} -> id) (\s@ActivityResponse' {} a -> s {id = a} :: ActivityResponse)

-- | The unique identifier for the application that the campaign applies to.
activityResponse_applicationId :: Lens.Lens' ActivityResponse Prelude.Text
activityResponse_applicationId = Lens.lens (\ActivityResponse' {applicationId} -> applicationId) (\s@ActivityResponse' {} a -> s {applicationId = a} :: ActivityResponse)

instance Data.FromJSON ActivityResponse where
  parseJSON =
    Data.withObject
      "ActivityResponse"
      ( \x ->
          ActivityResponse'
            Prelude.<$> (x Data..:? "End")
            Prelude.<*> ( x
                            Data..:? "ExecutionMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Result")
            Prelude.<*> (x Data..:? "ScheduledStart")
            Prelude.<*> (x Data..:? "Start")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "SuccessfulEndpointCount")
            Prelude.<*> (x Data..:? "TimezonesCompletedCount")
            Prelude.<*> (x Data..:? "TimezonesTotalCount")
            Prelude.<*> (x Data..:? "TotalEndpointCount")
            Prelude.<*> (x Data..:? "TreatmentId")
            Prelude.<*> (x Data..: "CampaignId")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance Prelude.Hashable ActivityResponse where
  hashWithSalt _salt ActivityResponse' {..} =
    _salt
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` executionMetrics
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` scheduledStart
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` successfulEndpointCount
      `Prelude.hashWithSalt` timezonesCompletedCount
      `Prelude.hashWithSalt` timezonesTotalCount
      `Prelude.hashWithSalt` totalEndpointCount
      `Prelude.hashWithSalt` treatmentId
      `Prelude.hashWithSalt` campaignId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ActivityResponse where
  rnf ActivityResponse' {..} =
    Prelude.rnf end
      `Prelude.seq` Prelude.rnf executionMetrics
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf scheduledStart
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf successfulEndpointCount
      `Prelude.seq` Prelude.rnf timezonesCompletedCount
      `Prelude.seq` Prelude.rnf timezonesTotalCount
      `Prelude.seq` Prelude.rnf totalEndpointCount
      `Prelude.seq` Prelude.rnf treatmentId
      `Prelude.seq` Prelude.rnf campaignId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf applicationId
