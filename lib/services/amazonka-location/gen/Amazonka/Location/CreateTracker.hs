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
-- Module      : Amazonka.Location.CreateTracker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a tracker resource in your AWS account, which lets you retrieve
-- current and historical location of devices.
module Amazonka.Location.CreateTracker
  ( -- * Creating a Request
    CreateTracker (..),
    newCreateTracker,

    -- * Request Lenses
    createTracker_tags,
    createTracker_description,
    createTracker_pricingPlanDataSource,
    createTracker_pricingPlan,
    createTracker_kmsKeyId,
    createTracker_positionFiltering,
    createTracker_trackerName,

    -- * Destructuring the Response
    CreateTrackerResponse (..),
    newCreateTrackerResponse,

    -- * Response Lenses
    createTrackerResponse_httpStatus,
    createTrackerResponse_createTime,
    createTrackerResponse_trackerArn,
    createTrackerResponse_trackerName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTracker' smart constructor.
data CreateTracker = CreateTracker'
  { -- | Applies one or more tags to the tracker resource. A tag is a key-value
    -- pair helps manage, identify, search, and filter your resources by
    -- labelling them.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource
    --
    -- -   Each resource tag must be unique with a maximum of one value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional description for the tracker resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | A key identifier for an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
    -- Enter a key ID, key ARN, alias name, or alias ARN.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the position filtering for the tracker resource.
    --
    -- Valid values:
    --
    -- -   @TimeBased@ - Location updates are evaluated against linked geofence
    --     collections, but not every location update is stored. If your update
    --     frequency is more often than 30 seconds, only one update per 30
    --     seconds is stored for each unique device ID.
    --
    -- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
    --     location updates are ignored. Location updates within this area are
    --     neither evaluated against linked geofence collections, nor stored.
    --     This helps control costs by reducing the number of geofence
    --     evaluations and historical device positions to paginate through.
    --     Distance-based filtering can also reduce the effects of GPS noise
    --     when displaying device trajectories on a map.
    --
    -- -   @AccuracyBased@ - If the device has moved less than the measured
    --     accuracy, location updates are ignored. For example, if two
    --     consecutive updates from a device have a horizontal accuracy of 5 m
    --     and 10 m, the second update is ignored if the device has moved less
    --     than 15 m. Ignored location updates are neither evaluated against
    --     linked geofence collections, nor stored. This can reduce the effects
    --     of GPS noise when displaying device trajectories on a map, and can
    --     help control your costs by reducing the number of geofence
    --     evaluations.
    --
    -- This field is optional. If not specified, the default value is
    -- @TimeBased@.
    positionFiltering :: Prelude.Maybe PositionFiltering,
    -- | The name for the tracker resource.
    --
    -- Requirements:
    --
    -- -   Contain only alphanumeric characters (A-Z, a-z, 0-9) , hyphens (-),
    --     periods (.), and underscores (_).
    --
    -- -   Must be a unique tracker resource name.
    --
    -- -   No spaces allowed. For example, @ExampleTracker@.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTracker_tags' - Applies one or more tags to the tracker resource. A tag is a key-value
-- pair helps manage, identify, search, and filter your resources by
-- labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
--
-- 'description', 'createTracker_description' - An optional description for the tracker resource.
--
-- 'pricingPlanDataSource', 'createTracker_pricingPlanDataSource' - This parameter is no longer used.
--
-- 'pricingPlan', 'createTracker_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'kmsKeyId', 'createTracker_kmsKeyId' - A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
--
-- 'positionFiltering', 'createTracker_positionFiltering' - Specifies the position filtering for the tracker resource.
--
-- Valid values:
--
-- -   @TimeBased@ - Location updates are evaluated against linked geofence
--     collections, but not every location update is stored. If your update
--     frequency is more often than 30 seconds, only one update per 30
--     seconds is stored for each unique device ID.
--
-- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
--     location updates are ignored. Location updates within this area are
--     neither evaluated against linked geofence collections, nor stored.
--     This helps control costs by reducing the number of geofence
--     evaluations and historical device positions to paginate through.
--     Distance-based filtering can also reduce the effects of GPS noise
--     when displaying device trajectories on a map.
--
-- -   @AccuracyBased@ - If the device has moved less than the measured
--     accuracy, location updates are ignored. For example, if two
--     consecutive updates from a device have a horizontal accuracy of 5 m
--     and 10 m, the second update is ignored if the device has moved less
--     than 15 m. Ignored location updates are neither evaluated against
--     linked geofence collections, nor stored. This can reduce the effects
--     of GPS noise when displaying device trajectories on a map, and can
--     help control your costs by reducing the number of geofence
--     evaluations.
--
-- This field is optional. If not specified, the default value is
-- @TimeBased@.
--
-- 'trackerName', 'createTracker_trackerName' - The name for the tracker resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A-Z, a-z, 0-9) , hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique tracker resource name.
--
-- -   No spaces allowed. For example, @ExampleTracker@.
newCreateTracker ::
  -- | 'trackerName'
  Prelude.Text ->
  CreateTracker
newCreateTracker pTrackerName_ =
  CreateTracker'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      pricingPlanDataSource = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      positionFiltering = Prelude.Nothing,
      trackerName = pTrackerName_
    }

-- | Applies one or more tags to the tracker resource. A tag is a key-value
-- pair helps manage, identify, search, and filter your resources by
-- labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
createTracker_tags :: Lens.Lens' CreateTracker (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTracker_tags = Lens.lens (\CreateTracker' {tags} -> tags) (\s@CreateTracker' {} a -> s {tags = a} :: CreateTracker) Prelude.. Lens.mapping Lens.coerced

-- | An optional description for the tracker resource.
createTracker_description :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_description = Lens.lens (\CreateTracker' {description} -> description) (\s@CreateTracker' {} a -> s {description = a} :: CreateTracker)

-- | This parameter is no longer used.
createTracker_pricingPlanDataSource :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_pricingPlanDataSource = Lens.lens (\CreateTracker' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@CreateTracker' {} a -> s {pricingPlanDataSource = a} :: CreateTracker)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
createTracker_pricingPlan :: Lens.Lens' CreateTracker (Prelude.Maybe PricingPlan)
createTracker_pricingPlan = Lens.lens (\CreateTracker' {pricingPlan} -> pricingPlan) (\s@CreateTracker' {} a -> s {pricingPlan = a} :: CreateTracker)

-- | A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
createTracker_kmsKeyId :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_kmsKeyId = Lens.lens (\CreateTracker' {kmsKeyId} -> kmsKeyId) (\s@CreateTracker' {} a -> s {kmsKeyId = a} :: CreateTracker)

-- | Specifies the position filtering for the tracker resource.
--
-- Valid values:
--
-- -   @TimeBased@ - Location updates are evaluated against linked geofence
--     collections, but not every location update is stored. If your update
--     frequency is more often than 30 seconds, only one update per 30
--     seconds is stored for each unique device ID.
--
-- -   @DistanceBased@ - If the device has moved less than 30 m (98.4 ft),
--     location updates are ignored. Location updates within this area are
--     neither evaluated against linked geofence collections, nor stored.
--     This helps control costs by reducing the number of geofence
--     evaluations and historical device positions to paginate through.
--     Distance-based filtering can also reduce the effects of GPS noise
--     when displaying device trajectories on a map.
--
-- -   @AccuracyBased@ - If the device has moved less than the measured
--     accuracy, location updates are ignored. For example, if two
--     consecutive updates from a device have a horizontal accuracy of 5 m
--     and 10 m, the second update is ignored if the device has moved less
--     than 15 m. Ignored location updates are neither evaluated against
--     linked geofence collections, nor stored. This can reduce the effects
--     of GPS noise when displaying device trajectories on a map, and can
--     help control your costs by reducing the number of geofence
--     evaluations.
--
-- This field is optional. If not specified, the default value is
-- @TimeBased@.
createTracker_positionFiltering :: Lens.Lens' CreateTracker (Prelude.Maybe PositionFiltering)
createTracker_positionFiltering = Lens.lens (\CreateTracker' {positionFiltering} -> positionFiltering) (\s@CreateTracker' {} a -> s {positionFiltering = a} :: CreateTracker)

-- | The name for the tracker resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A-Z, a-z, 0-9) , hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique tracker resource name.
--
-- -   No spaces allowed. For example, @ExampleTracker@.
createTracker_trackerName :: Lens.Lens' CreateTracker Prelude.Text
createTracker_trackerName = Lens.lens (\CreateTracker' {trackerName} -> trackerName) (\s@CreateTracker' {} a -> s {trackerName = a} :: CreateTracker)

instance Core.AWSRequest CreateTracker where
  type
    AWSResponse CreateTracker =
      CreateTrackerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrackerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "TrackerArn")
            Prelude.<*> (x Data..:> "TrackerName")
      )

instance Prelude.Hashable CreateTracker where
  hashWithSalt _salt CreateTracker' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlanDataSource
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` positionFiltering
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData CreateTracker where
  rnf CreateTracker' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf positionFiltering
      `Prelude.seq` Prelude.rnf trackerName

instance Data.ToHeaders CreateTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTracker where
  toJSON CreateTracker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Description" Data..=) Prelude.<$> description,
            ("PricingPlanDataSource" Data..=)
              Prelude.<$> pricingPlanDataSource,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("PositionFiltering" Data..=)
              Prelude.<$> positionFiltering,
            Prelude.Just ("TrackerName" Data..= trackerName)
          ]
      )

instance Data.ToPath CreateTracker where
  toPath = Prelude.const "/tracking/v0/trackers"

instance Data.ToQuery CreateTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrackerResponse' smart constructor.
data CreateTrackerResponse = CreateTrackerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the tracker resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the tracker resource. Used when you
    -- need to specify a resource across all AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
    trackerArn :: Prelude.Text,
    -- | The name of the tracker resource.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTrackerResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'createTrackerResponse_createTime' - The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'trackerArn', 'createTrackerResponse_trackerArn' - The Amazon Resource Name (ARN) for the tracker resource. Used when you
-- need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
--
-- 'trackerName', 'createTrackerResponse_trackerName' - The name of the tracker resource.
newCreateTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'trackerArn'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  CreateTrackerResponse
newCreateTrackerResponse
  pHttpStatus_
  pCreateTime_
  pTrackerArn_
  pTrackerName_ =
    CreateTrackerResponse'
      { httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        trackerArn = pTrackerArn_,
        trackerName = pTrackerName_
      }

-- | The response's http status code.
createTrackerResponse_httpStatus :: Lens.Lens' CreateTrackerResponse Prelude.Int
createTrackerResponse_httpStatus = Lens.lens (\CreateTrackerResponse' {httpStatus} -> httpStatus) (\s@CreateTrackerResponse' {} a -> s {httpStatus = a} :: CreateTrackerResponse)

-- | The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
createTrackerResponse_createTime :: Lens.Lens' CreateTrackerResponse Prelude.UTCTime
createTrackerResponse_createTime = Lens.lens (\CreateTrackerResponse' {createTime} -> createTime) (\s@CreateTrackerResponse' {} a -> s {createTime = a} :: CreateTrackerResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) for the tracker resource. Used when you
-- need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
createTrackerResponse_trackerArn :: Lens.Lens' CreateTrackerResponse Prelude.Text
createTrackerResponse_trackerArn = Lens.lens (\CreateTrackerResponse' {trackerArn} -> trackerArn) (\s@CreateTrackerResponse' {} a -> s {trackerArn = a} :: CreateTrackerResponse)

-- | The name of the tracker resource.
createTrackerResponse_trackerName :: Lens.Lens' CreateTrackerResponse Prelude.Text
createTrackerResponse_trackerName = Lens.lens (\CreateTrackerResponse' {trackerName} -> trackerName) (\s@CreateTrackerResponse' {} a -> s {trackerName = a} :: CreateTrackerResponse)

instance Prelude.NFData CreateTrackerResponse where
  rnf CreateTrackerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf trackerArn
      `Prelude.seq` Prelude.rnf trackerName
