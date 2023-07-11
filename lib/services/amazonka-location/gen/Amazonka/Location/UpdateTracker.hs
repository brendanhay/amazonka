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
-- Module      : Amazonka.Location.UpdateTracker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties of a given tracker resource.
module Amazonka.Location.UpdateTracker
  ( -- * Creating a Request
    UpdateTracker (..),
    newUpdateTracker,

    -- * Request Lenses
    updateTracker_description,
    updateTracker_positionFiltering,
    updateTracker_pricingPlan,
    updateTracker_pricingPlanDataSource,
    updateTracker_trackerName,

    -- * Destructuring the Response
    UpdateTrackerResponse (..),
    newUpdateTrackerResponse,

    -- * Response Lenses
    updateTrackerResponse_httpStatus,
    updateTrackerResponse_trackerArn,
    updateTrackerResponse_trackerName,
    updateTrackerResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTracker' smart constructor.
data UpdateTracker = UpdateTracker'
  { -- | Updates the description for the tracker resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | Updates the position filtering for the tracker resource.
    --
    -- Valid values:
    --
    -- -   @TimeBased@ - Location updates are evaluated against linked geofence
    --     collections, but not every location update is stored. If your update
    --     frequency is more often than 30 seconds, only one update per 30
    --     seconds is stored for each unique device ID.
    --
    -- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
    --     location updates are ignored. Location updates within this distance
    --     are neither evaluated against linked geofence collections, nor
    --     stored. This helps control costs by reducing the number of geofence
    --     evaluations and historical device positions to paginate through.
    --     Distance-based filtering can also reduce the effects of GPS noise
    --     when displaying device trajectories on a map.
    --
    -- -   @AccuracyBased@ - If the device has moved less than the measured
    --     accuracy, location updates are ignored. For example, if two
    --     consecutive updates from a device have a horizontal accuracy of 5 m
    --     and 10 m, the second update is ignored if the device has moved less
    --     than 15 m. Ignored location updates are neither evaluated against
    --     linked geofence collections, nor stored. This helps educe the
    --     effects of GPS noise when displaying device trajectories on a map,
    --     and can help control costs by reducing the number of geofence
    --     evaluations.
    positionFiltering :: Prelude.Maybe PositionFiltering,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | This parameter is no longer used.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the tracker resource to update.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateTracker_description' - Updates the description for the tracker resource.
--
-- 'positionFiltering', 'updateTracker_positionFiltering' - Updates the position filtering for the tracker resource.
--
-- Valid values:
--
-- -   @TimeBased@ - Location updates are evaluated against linked geofence
--     collections, but not every location update is stored. If your update
--     frequency is more often than 30 seconds, only one update per 30
--     seconds is stored for each unique device ID.
--
-- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
--     location updates are ignored. Location updates within this distance
--     are neither evaluated against linked geofence collections, nor
--     stored. This helps control costs by reducing the number of geofence
--     evaluations and historical device positions to paginate through.
--     Distance-based filtering can also reduce the effects of GPS noise
--     when displaying device trajectories on a map.
--
-- -   @AccuracyBased@ - If the device has moved less than the measured
--     accuracy, location updates are ignored. For example, if two
--     consecutive updates from a device have a horizontal accuracy of 5 m
--     and 10 m, the second update is ignored if the device has moved less
--     than 15 m. Ignored location updates are neither evaluated against
--     linked geofence collections, nor stored. This helps educe the
--     effects of GPS noise when displaying device trajectories on a map,
--     and can help control costs by reducing the number of geofence
--     evaluations.
--
-- 'pricingPlan', 'updateTracker_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'pricingPlanDataSource', 'updateTracker_pricingPlanDataSource' - This parameter is no longer used.
--
-- 'trackerName', 'updateTracker_trackerName' - The name of the tracker resource to update.
newUpdateTracker ::
  -- | 'trackerName'
  Prelude.Text ->
  UpdateTracker
newUpdateTracker pTrackerName_ =
  UpdateTracker'
    { description = Prelude.Nothing,
      positionFiltering = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      pricingPlanDataSource = Prelude.Nothing,
      trackerName = pTrackerName_
    }

-- | Updates the description for the tracker resource.
updateTracker_description :: Lens.Lens' UpdateTracker (Prelude.Maybe Prelude.Text)
updateTracker_description = Lens.lens (\UpdateTracker' {description} -> description) (\s@UpdateTracker' {} a -> s {description = a} :: UpdateTracker)

-- | Updates the position filtering for the tracker resource.
--
-- Valid values:
--
-- -   @TimeBased@ - Location updates are evaluated against linked geofence
--     collections, but not every location update is stored. If your update
--     frequency is more often than 30 seconds, only one update per 30
--     seconds is stored for each unique device ID.
--
-- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
--     location updates are ignored. Location updates within this distance
--     are neither evaluated against linked geofence collections, nor
--     stored. This helps control costs by reducing the number of geofence
--     evaluations and historical device positions to paginate through.
--     Distance-based filtering can also reduce the effects of GPS noise
--     when displaying device trajectories on a map.
--
-- -   @AccuracyBased@ - If the device has moved less than the measured
--     accuracy, location updates are ignored. For example, if two
--     consecutive updates from a device have a horizontal accuracy of 5 m
--     and 10 m, the second update is ignored if the device has moved less
--     than 15 m. Ignored location updates are neither evaluated against
--     linked geofence collections, nor stored. This helps educe the
--     effects of GPS noise when displaying device trajectories on a map,
--     and can help control costs by reducing the number of geofence
--     evaluations.
updateTracker_positionFiltering :: Lens.Lens' UpdateTracker (Prelude.Maybe PositionFiltering)
updateTracker_positionFiltering = Lens.lens (\UpdateTracker' {positionFiltering} -> positionFiltering) (\s@UpdateTracker' {} a -> s {positionFiltering = a} :: UpdateTracker)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
updateTracker_pricingPlan :: Lens.Lens' UpdateTracker (Prelude.Maybe PricingPlan)
updateTracker_pricingPlan = Lens.lens (\UpdateTracker' {pricingPlan} -> pricingPlan) (\s@UpdateTracker' {} a -> s {pricingPlan = a} :: UpdateTracker)

-- | This parameter is no longer used.
updateTracker_pricingPlanDataSource :: Lens.Lens' UpdateTracker (Prelude.Maybe Prelude.Text)
updateTracker_pricingPlanDataSource = Lens.lens (\UpdateTracker' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@UpdateTracker' {} a -> s {pricingPlanDataSource = a} :: UpdateTracker)

-- | The name of the tracker resource to update.
updateTracker_trackerName :: Lens.Lens' UpdateTracker Prelude.Text
updateTracker_trackerName = Lens.lens (\UpdateTracker' {trackerName} -> trackerName) (\s@UpdateTracker' {} a -> s {trackerName = a} :: UpdateTracker)

instance Core.AWSRequest UpdateTracker where
  type
    AWSResponse UpdateTracker =
      UpdateTrackerResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrackerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TrackerArn")
            Prelude.<*> (x Data..:> "TrackerName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateTracker where
  hashWithSalt _salt UpdateTracker' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` positionFiltering
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` pricingPlanDataSource
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData UpdateTracker where
  rnf UpdateTracker' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf positionFiltering
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf trackerName

instance Data.ToHeaders UpdateTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTracker where
  toJSON UpdateTracker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("PositionFiltering" Data..=)
              Prelude.<$> positionFiltering,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("PricingPlanDataSource" Data..=)
              Prelude.<$> pricingPlanDataSource
          ]
      )

instance Data.ToPath UpdateTracker where
  toPath UpdateTracker' {..} =
    Prelude.mconcat
      ["/tracking/v0/trackers/", Data.toBS trackerName]

instance Data.ToQuery UpdateTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrackerResponse' smart constructor.
data UpdateTrackerResponse = UpdateTrackerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated tracker resource. Used to
    -- specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
    trackerArn :: Prelude.Text,
    -- | The name of the updated tracker resource.
    trackerName :: Prelude.Text,
    -- | The timestamp for when the tracker resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTrackerResponse_httpStatus' - The response's http status code.
--
-- 'trackerArn', 'updateTrackerResponse_trackerArn' - The Amazon Resource Name (ARN) of the updated tracker resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
--
-- 'trackerName', 'updateTrackerResponse_trackerName' - The name of the updated tracker resource.
--
-- 'updateTime', 'updateTrackerResponse_updateTime' - The timestamp for when the tracker resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newUpdateTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trackerArn'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateTrackerResponse
newUpdateTrackerResponse
  pHttpStatus_
  pTrackerArn_
  pTrackerName_
  pUpdateTime_ =
    UpdateTrackerResponse'
      { httpStatus = pHttpStatus_,
        trackerArn = pTrackerArn_,
        trackerName = pTrackerName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateTrackerResponse_httpStatus :: Lens.Lens' UpdateTrackerResponse Prelude.Int
updateTrackerResponse_httpStatus = Lens.lens (\UpdateTrackerResponse' {httpStatus} -> httpStatus) (\s@UpdateTrackerResponse' {} a -> s {httpStatus = a} :: UpdateTrackerResponse)

-- | The Amazon Resource Name (ARN) of the updated tracker resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
updateTrackerResponse_trackerArn :: Lens.Lens' UpdateTrackerResponse Prelude.Text
updateTrackerResponse_trackerArn = Lens.lens (\UpdateTrackerResponse' {trackerArn} -> trackerArn) (\s@UpdateTrackerResponse' {} a -> s {trackerArn = a} :: UpdateTrackerResponse)

-- | The name of the updated tracker resource.
updateTrackerResponse_trackerName :: Lens.Lens' UpdateTrackerResponse Prelude.Text
updateTrackerResponse_trackerName = Lens.lens (\UpdateTrackerResponse' {trackerName} -> trackerName) (\s@UpdateTrackerResponse' {} a -> s {trackerName = a} :: UpdateTrackerResponse)

-- | The timestamp for when the tracker resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateTrackerResponse_updateTime :: Lens.Lens' UpdateTrackerResponse Prelude.UTCTime
updateTrackerResponse_updateTime = Lens.lens (\UpdateTrackerResponse' {updateTime} -> updateTime) (\s@UpdateTrackerResponse' {} a -> s {updateTime = a} :: UpdateTrackerResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateTrackerResponse where
  rnf UpdateTrackerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trackerArn
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf updateTime
