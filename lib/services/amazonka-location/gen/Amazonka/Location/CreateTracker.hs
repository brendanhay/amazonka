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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createTracker_pricingPlanDataSource,
    createTracker_kmsKeyId,
    createTracker_description,
    createTracker_tags,
    createTracker_positionFiltering,
    createTracker_pricingPlan,
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
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTracker' smart constructor.
data CreateTracker = CreateTracker'
  { -- | Specifies the data provider for the tracker resource.
    --
    -- -   Required value for the following pricing plans:
    --     @MobileAssetTracking @| @MobileAssetManagement@
    --
    -- For more information about
    -- <https://aws.amazon.com/location/data-providers/ Data Providers>, and
    -- <https://aws.amazon.com/location/pricing/ Pricing plans>, see the Amazon
    -- Location Service product page.
    --
    -- Amazon Location Service only uses @PricingPlanDataSource@ to calculate
    -- billing for your tracker resource. Your data will not be shared with the
    -- data provider, and will remain in your AWS account or Region unless you
    -- move it.
    --
    -- Valid values: @Esri@ | @Here@
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | A key identifier for an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
    -- Enter a key ID, key ARN, alias name, or alias ARN.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An optional description for the tracker resource.
    description :: Prelude.Maybe Prelude.Text,
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
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    --     location updates are ignored. Location updates within this distance
    --     are neither evaluated against linked geofence collections, nor
    --     stored. This helps control costs by reducing the number of geofence
    --     evaluations and device positions to retrieve. Distance-based
    --     filtering can also reduce the jitter effect when displaying device
    --     trajectory on a map.
    --
    -- This field is optional. If not specified, the default value is
    -- @TimeBased@.
    positionFiltering :: Prelude.Maybe PositionFiltering,
    -- | Specifies the pricing plan for the tracker resource.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: PricingPlan,
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
-- 'pricingPlanDataSource', 'createTracker_pricingPlanDataSource' - Specifies the data provider for the tracker resource.
--
-- -   Required value for the following pricing plans:
--     @MobileAssetTracking @| @MobileAssetManagement@
--
-- For more information about
-- <https://aws.amazon.com/location/data-providers/ Data Providers>, and
-- <https://aws.amazon.com/location/pricing/ Pricing plans>, see the Amazon
-- Location Service product page.
--
-- Amazon Location Service only uses @PricingPlanDataSource@ to calculate
-- billing for your tracker resource. Your data will not be shared with the
-- data provider, and will remain in your AWS account or Region unless you
-- move it.
--
-- Valid values: @Esri@ | @Here@
--
-- 'kmsKeyId', 'createTracker_kmsKeyId' - A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
--
-- 'description', 'createTracker_description' - An optional description for the tracker resource.
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
--     location updates are ignored. Location updates within this distance
--     are neither evaluated against linked geofence collections, nor
--     stored. This helps control costs by reducing the number of geofence
--     evaluations and device positions to retrieve. Distance-based
--     filtering can also reduce the jitter effect when displaying device
--     trajectory on a map.
--
-- This field is optional. If not specified, the default value is
-- @TimeBased@.
--
-- 'pricingPlan', 'createTracker_pricingPlan' - Specifies the pricing plan for the tracker resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
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
  -- | 'pricingPlan'
  PricingPlan ->
  -- | 'trackerName'
  Prelude.Text ->
  CreateTracker
newCreateTracker pPricingPlan_ pTrackerName_ =
  CreateTracker'
    { pricingPlanDataSource =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      positionFiltering = Prelude.Nothing,
      pricingPlan = pPricingPlan_,
      trackerName = pTrackerName_
    }

-- | Specifies the data provider for the tracker resource.
--
-- -   Required value for the following pricing plans:
--     @MobileAssetTracking @| @MobileAssetManagement@
--
-- For more information about
-- <https://aws.amazon.com/location/data-providers/ Data Providers>, and
-- <https://aws.amazon.com/location/pricing/ Pricing plans>, see the Amazon
-- Location Service product page.
--
-- Amazon Location Service only uses @PricingPlanDataSource@ to calculate
-- billing for your tracker resource. Your data will not be shared with the
-- data provider, and will remain in your AWS account or Region unless you
-- move it.
--
-- Valid values: @Esri@ | @Here@
createTracker_pricingPlanDataSource :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_pricingPlanDataSource = Lens.lens (\CreateTracker' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@CreateTracker' {} a -> s {pricingPlanDataSource = a} :: CreateTracker)

-- | A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
createTracker_kmsKeyId :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_kmsKeyId = Lens.lens (\CreateTracker' {kmsKeyId} -> kmsKeyId) (\s@CreateTracker' {} a -> s {kmsKeyId = a} :: CreateTracker)

-- | An optional description for the tracker resource.
createTracker_description :: Lens.Lens' CreateTracker (Prelude.Maybe Prelude.Text)
createTracker_description = Lens.lens (\CreateTracker' {description} -> description) (\s@CreateTracker' {} a -> s {description = a} :: CreateTracker)

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
createTracker_tags :: Lens.Lens' CreateTracker (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTracker_tags = Lens.lens (\CreateTracker' {tags} -> tags) (\s@CreateTracker' {} a -> s {tags = a} :: CreateTracker) Prelude.. Lens.mapping Lens.coerced

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
--     location updates are ignored. Location updates within this distance
--     are neither evaluated against linked geofence collections, nor
--     stored. This helps control costs by reducing the number of geofence
--     evaluations and device positions to retrieve. Distance-based
--     filtering can also reduce the jitter effect when displaying device
--     trajectory on a map.
--
-- This field is optional. If not specified, the default value is
-- @TimeBased@.
createTracker_positionFiltering :: Lens.Lens' CreateTracker (Prelude.Maybe PositionFiltering)
createTracker_positionFiltering = Lens.lens (\CreateTracker' {positionFiltering} -> positionFiltering) (\s@CreateTracker' {} a -> s {positionFiltering = a} :: CreateTracker)

-- | Specifies the pricing plan for the tracker resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
createTracker_pricingPlan :: Lens.Lens' CreateTracker PricingPlan
createTracker_pricingPlan = Lens.lens (\CreateTracker' {pricingPlan} -> pricingPlan) (\s@CreateTracker' {} a -> s {pricingPlan = a} :: CreateTracker)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrackerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CreateTime")
            Prelude.<*> (x Core..:> "TrackerArn")
            Prelude.<*> (x Core..:> "TrackerName")
      )

instance Prelude.Hashable CreateTracker where
  hashWithSalt salt' CreateTracker' {..} =
    salt' `Prelude.hashWithSalt` trackerName
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` positionFiltering
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` pricingPlanDataSource

instance Prelude.NFData CreateTracker where
  rnf CreateTracker' {..} =
    Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf positionFiltering
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Core.ToHeaders CreateTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTracker where
  toJSON CreateTracker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PricingPlanDataSource" Core..=)
              Prelude.<$> pricingPlanDataSource,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            ("PositionFiltering" Core..=)
              Prelude.<$> positionFiltering,
            Prelude.Just ("PricingPlan" Core..= pricingPlan),
            Prelude.Just ("TrackerName" Core..= trackerName)
          ]
      )

instance Core.ToPath CreateTracker where
  toPath = Prelude.const "/tracking/v0/trackers"

instance Core.ToQuery CreateTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrackerResponse' smart constructor.
data CreateTrackerResponse = CreateTrackerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the tracker resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Core.POSIX,
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
        createTime = Core._Time Lens.# pCreateTime_,
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
createTrackerResponse_createTime = Lens.lens (\CreateTrackerResponse' {createTime} -> createTime) (\s@CreateTrackerResponse' {} a -> s {createTime = a} :: CreateTrackerResponse) Prelude.. Core._Time

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
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf trackerArn
      `Prelude.seq` Prelude.rnf createTime
