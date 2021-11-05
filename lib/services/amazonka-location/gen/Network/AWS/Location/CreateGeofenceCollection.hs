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
-- Module      : Network.AWS.Location.CreateGeofenceCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a geofence collection, which manages and stores geofences.
module Network.AWS.Location.CreateGeofenceCollection
  ( -- * Creating a Request
    CreateGeofenceCollection (..),
    newCreateGeofenceCollection,

    -- * Request Lenses
    createGeofenceCollection_pricingPlanDataSource,
    createGeofenceCollection_kmsKeyId,
    createGeofenceCollection_description,
    createGeofenceCollection_tags,
    createGeofenceCollection_collectionName,
    createGeofenceCollection_pricingPlan,

    -- * Destructuring the Response
    CreateGeofenceCollectionResponse (..),
    newCreateGeofenceCollectionResponse,

    -- * Response Lenses
    createGeofenceCollectionResponse_httpStatus,
    createGeofenceCollectionResponse_collectionArn,
    createGeofenceCollectionResponse_collectionName,
    createGeofenceCollectionResponse_createTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGeofenceCollection' smart constructor.
data CreateGeofenceCollection = CreateGeofenceCollection'
  { -- | Specifies the data provider for the geofence collection.
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
    -- billing for your geofence collection. Your data won\'t be shared with
    -- the data provider, and will remain in your AWS account or Region unless
    -- you move it.
    --
    -- Valid Values: @Esri @| @Here@
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | A key identifier for an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
    -- Enter a key ID, key ARN, alias name, or alias ARN.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An optional description for the geofence collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | Applies one or more tags to the geofence collection. A tag is a
    -- key-value pair helps manage, identify, search, and filter your resources
    -- by labelling them.
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
    -- | A custom name for the geofence collection.
    --
    -- Requirements:
    --
    -- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
    --     periods (.), and underscores (_).
    --
    -- -   Must be a unique geofence collection name.
    --
    -- -   No spaces allowed. For example, @ExampleGeofenceCollection@.
    collectionName :: Prelude.Text,
    -- | Specifies the pricing plan for the geofence collection.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- the
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
    pricingPlan :: PricingPlan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGeofenceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlanDataSource', 'createGeofenceCollection_pricingPlanDataSource' - Specifies the data provider for the geofence collection.
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
-- billing for your geofence collection. Your data won\'t be shared with
-- the data provider, and will remain in your AWS account or Region unless
-- you move it.
--
-- Valid Values: @Esri @| @Here@
--
-- 'kmsKeyId', 'createGeofenceCollection_kmsKeyId' - A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
--
-- 'description', 'createGeofenceCollection_description' - An optional description for the geofence collection.
--
-- 'tags', 'createGeofenceCollection_tags' - Applies one or more tags to the geofence collection. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
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
-- 'collectionName', 'createGeofenceCollection_collectionName' - A custom name for the geofence collection.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique geofence collection name.
--
-- -   No spaces allowed. For example, @ExampleGeofenceCollection@.
--
-- 'pricingPlan', 'createGeofenceCollection_pricingPlan' - Specifies the pricing plan for the geofence collection.
--
-- For additional details and restrictions on each pricing plan option, see
-- the
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
newCreateGeofenceCollection ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'pricingPlan'
  PricingPlan ->
  CreateGeofenceCollection
newCreateGeofenceCollection
  pCollectionName_
  pPricingPlan_ =
    CreateGeofenceCollection'
      { pricingPlanDataSource =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        collectionName = pCollectionName_,
        pricingPlan = pPricingPlan_
      }

-- | Specifies the data provider for the geofence collection.
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
-- billing for your geofence collection. Your data won\'t be shared with
-- the data provider, and will remain in your AWS account or Region unless
-- you move it.
--
-- Valid Values: @Esri @| @Here@
createGeofenceCollection_pricingPlanDataSource :: Lens.Lens' CreateGeofenceCollection (Prelude.Maybe Prelude.Text)
createGeofenceCollection_pricingPlanDataSource = Lens.lens (\CreateGeofenceCollection' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@CreateGeofenceCollection' {} a -> s {pricingPlanDataSource = a} :: CreateGeofenceCollection)

-- | A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>.
-- Enter a key ID, key ARN, alias name, or alias ARN.
createGeofenceCollection_kmsKeyId :: Lens.Lens' CreateGeofenceCollection (Prelude.Maybe Prelude.Text)
createGeofenceCollection_kmsKeyId = Lens.lens (\CreateGeofenceCollection' {kmsKeyId} -> kmsKeyId) (\s@CreateGeofenceCollection' {} a -> s {kmsKeyId = a} :: CreateGeofenceCollection)

-- | An optional description for the geofence collection.
createGeofenceCollection_description :: Lens.Lens' CreateGeofenceCollection (Prelude.Maybe Prelude.Text)
createGeofenceCollection_description = Lens.lens (\CreateGeofenceCollection' {description} -> description) (\s@CreateGeofenceCollection' {} a -> s {description = a} :: CreateGeofenceCollection)

-- | Applies one or more tags to the geofence collection. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
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
createGeofenceCollection_tags :: Lens.Lens' CreateGeofenceCollection (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGeofenceCollection_tags = Lens.lens (\CreateGeofenceCollection' {tags} -> tags) (\s@CreateGeofenceCollection' {} a -> s {tags = a} :: CreateGeofenceCollection) Prelude.. Lens.mapping Lens.coerced

-- | A custom name for the geofence collection.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique geofence collection name.
--
-- -   No spaces allowed. For example, @ExampleGeofenceCollection@.
createGeofenceCollection_collectionName :: Lens.Lens' CreateGeofenceCollection Prelude.Text
createGeofenceCollection_collectionName = Lens.lens (\CreateGeofenceCollection' {collectionName} -> collectionName) (\s@CreateGeofenceCollection' {} a -> s {collectionName = a} :: CreateGeofenceCollection)

-- | Specifies the pricing plan for the geofence collection.
--
-- For additional details and restrictions on each pricing plan option, see
-- the
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
createGeofenceCollection_pricingPlan :: Lens.Lens' CreateGeofenceCollection PricingPlan
createGeofenceCollection_pricingPlan = Lens.lens (\CreateGeofenceCollection' {pricingPlan} -> pricingPlan) (\s@CreateGeofenceCollection' {} a -> s {pricingPlan = a} :: CreateGeofenceCollection)

instance Core.AWSRequest CreateGeofenceCollection where
  type
    AWSResponse CreateGeofenceCollection =
      CreateGeofenceCollectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGeofenceCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CollectionArn")
            Prelude.<*> (x Core..:> "CollectionName")
            Prelude.<*> (x Core..:> "CreateTime")
      )

instance Prelude.Hashable CreateGeofenceCollection

instance Prelude.NFData CreateGeofenceCollection

instance Core.ToHeaders CreateGeofenceCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateGeofenceCollection where
  toJSON CreateGeofenceCollection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PricingPlanDataSource" Core..=)
              Prelude.<$> pricingPlanDataSource,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("CollectionName" Core..= collectionName),
            Prelude.Just ("PricingPlan" Core..= pricingPlan)
          ]
      )

instance Core.ToPath CreateGeofenceCollection where
  toPath = Prelude.const "/geofencing/v0/collections"

instance Core.ToQuery CreateGeofenceCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGeofenceCollectionResponse' smart constructor.
data CreateGeofenceCollectionResponse = CreateGeofenceCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the geofence collection resource.
    -- Used when you need to specify a resource across all AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
    collectionArn :: Prelude.Text,
    -- | The name for the geofence collection.
    collectionName :: Prelude.Text,
    -- | The timestamp for when the geofence collection was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGeofenceCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createGeofenceCollectionResponse_httpStatus' - The response's http status code.
--
-- 'collectionArn', 'createGeofenceCollectionResponse_collectionArn' - The Amazon Resource Name (ARN) for the geofence collection resource.
-- Used when you need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
--
-- 'collectionName', 'createGeofenceCollectionResponse_collectionName' - The name for the geofence collection.
--
-- 'createTime', 'createGeofenceCollectionResponse_createTime' - The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newCreateGeofenceCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'collectionArn'
  Prelude.Text ->
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  CreateGeofenceCollectionResponse
newCreateGeofenceCollectionResponse
  pHttpStatus_
  pCollectionArn_
  pCollectionName_
  pCreateTime_ =
    CreateGeofenceCollectionResponse'
      { httpStatus =
          pHttpStatus_,
        collectionArn = pCollectionArn_,
        collectionName = pCollectionName_,
        createTime =
          Core._Time Lens.# pCreateTime_
      }

-- | The response's http status code.
createGeofenceCollectionResponse_httpStatus :: Lens.Lens' CreateGeofenceCollectionResponse Prelude.Int
createGeofenceCollectionResponse_httpStatus = Lens.lens (\CreateGeofenceCollectionResponse' {httpStatus} -> httpStatus) (\s@CreateGeofenceCollectionResponse' {} a -> s {httpStatus = a} :: CreateGeofenceCollectionResponse)

-- | The Amazon Resource Name (ARN) for the geofence collection resource.
-- Used when you need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
createGeofenceCollectionResponse_collectionArn :: Lens.Lens' CreateGeofenceCollectionResponse Prelude.Text
createGeofenceCollectionResponse_collectionArn = Lens.lens (\CreateGeofenceCollectionResponse' {collectionArn} -> collectionArn) (\s@CreateGeofenceCollectionResponse' {} a -> s {collectionArn = a} :: CreateGeofenceCollectionResponse)

-- | The name for the geofence collection.
createGeofenceCollectionResponse_collectionName :: Lens.Lens' CreateGeofenceCollectionResponse Prelude.Text
createGeofenceCollectionResponse_collectionName = Lens.lens (\CreateGeofenceCollectionResponse' {collectionName} -> collectionName) (\s@CreateGeofenceCollectionResponse' {} a -> s {collectionName = a} :: CreateGeofenceCollectionResponse)

-- | The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
createGeofenceCollectionResponse_createTime :: Lens.Lens' CreateGeofenceCollectionResponse Prelude.UTCTime
createGeofenceCollectionResponse_createTime = Lens.lens (\CreateGeofenceCollectionResponse' {createTime} -> createTime) (\s@CreateGeofenceCollectionResponse' {} a -> s {createTime = a} :: CreateGeofenceCollectionResponse) Prelude.. Core._Time

instance
  Prelude.NFData
    CreateGeofenceCollectionResponse
