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
-- Module      : Amazonka.Location.UpdateGeofenceCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties of a given geofence collection.
module Amazonka.Location.UpdateGeofenceCollection
  ( -- * Creating a Request
    UpdateGeofenceCollection (..),
    newUpdateGeofenceCollection,

    -- * Request Lenses
    updateGeofenceCollection_description,
    updateGeofenceCollection_pricingPlan,
    updateGeofenceCollection_pricingPlanDataSource,
    updateGeofenceCollection_collectionName,

    -- * Destructuring the Response
    UpdateGeofenceCollectionResponse (..),
    newUpdateGeofenceCollectionResponse,

    -- * Response Lenses
    updateGeofenceCollectionResponse_httpStatus,
    updateGeofenceCollectionResponse_collectionArn,
    updateGeofenceCollectionResponse_collectionName,
    updateGeofenceCollectionResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGeofenceCollection' smart constructor.
data UpdateGeofenceCollection = UpdateGeofenceCollection'
  { -- | Updates the description for the geofence collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | This parameter is no longer used.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the geofence collection to update.
    collectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGeofenceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateGeofenceCollection_description' - Updates the description for the geofence collection.
--
-- 'pricingPlan', 'updateGeofenceCollection_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'pricingPlanDataSource', 'updateGeofenceCollection_pricingPlanDataSource' - This parameter is no longer used.
--
-- 'collectionName', 'updateGeofenceCollection_collectionName' - The name of the geofence collection to update.
newUpdateGeofenceCollection ::
  -- | 'collectionName'
  Prelude.Text ->
  UpdateGeofenceCollection
newUpdateGeofenceCollection pCollectionName_ =
  UpdateGeofenceCollection'
    { description =
        Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      pricingPlanDataSource = Prelude.Nothing,
      collectionName = pCollectionName_
    }

-- | Updates the description for the geofence collection.
updateGeofenceCollection_description :: Lens.Lens' UpdateGeofenceCollection (Prelude.Maybe Prelude.Text)
updateGeofenceCollection_description = Lens.lens (\UpdateGeofenceCollection' {description} -> description) (\s@UpdateGeofenceCollection' {} a -> s {description = a} :: UpdateGeofenceCollection)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
updateGeofenceCollection_pricingPlan :: Lens.Lens' UpdateGeofenceCollection (Prelude.Maybe PricingPlan)
updateGeofenceCollection_pricingPlan = Lens.lens (\UpdateGeofenceCollection' {pricingPlan} -> pricingPlan) (\s@UpdateGeofenceCollection' {} a -> s {pricingPlan = a} :: UpdateGeofenceCollection)

-- | This parameter is no longer used.
updateGeofenceCollection_pricingPlanDataSource :: Lens.Lens' UpdateGeofenceCollection (Prelude.Maybe Prelude.Text)
updateGeofenceCollection_pricingPlanDataSource = Lens.lens (\UpdateGeofenceCollection' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@UpdateGeofenceCollection' {} a -> s {pricingPlanDataSource = a} :: UpdateGeofenceCollection)

-- | The name of the geofence collection to update.
updateGeofenceCollection_collectionName :: Lens.Lens' UpdateGeofenceCollection Prelude.Text
updateGeofenceCollection_collectionName = Lens.lens (\UpdateGeofenceCollection' {collectionName} -> collectionName) (\s@UpdateGeofenceCollection' {} a -> s {collectionName = a} :: UpdateGeofenceCollection)

instance Core.AWSRequest UpdateGeofenceCollection where
  type
    AWSResponse UpdateGeofenceCollection =
      UpdateGeofenceCollectionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGeofenceCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CollectionArn")
            Prelude.<*> (x Data..:> "CollectionName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateGeofenceCollection where
  hashWithSalt _salt UpdateGeofenceCollection' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` pricingPlanDataSource
      `Prelude.hashWithSalt` collectionName

instance Prelude.NFData UpdateGeofenceCollection where
  rnf UpdateGeofenceCollection' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf collectionName

instance Data.ToHeaders UpdateGeofenceCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGeofenceCollection where
  toJSON UpdateGeofenceCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("PricingPlanDataSource" Data..=)
              Prelude.<$> pricingPlanDataSource
          ]
      )

instance Data.ToPath UpdateGeofenceCollection where
  toPath UpdateGeofenceCollection' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName
      ]

instance Data.ToQuery UpdateGeofenceCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGeofenceCollectionResponse' smart constructor.
data UpdateGeofenceCollectionResponse = UpdateGeofenceCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated geofence collection. Used
    -- to specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
    collectionArn :: Prelude.Text,
    -- | The name of the updated geofence collection.
    collectionName :: Prelude.Text,
    -- | The time when the geofence collection was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGeofenceCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGeofenceCollectionResponse_httpStatus' - The response's http status code.
--
-- 'collectionArn', 'updateGeofenceCollectionResponse_collectionArn' - The Amazon Resource Name (ARN) of the updated geofence collection. Used
-- to specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
--
-- 'collectionName', 'updateGeofenceCollectionResponse_collectionName' - The name of the updated geofence collection.
--
-- 'updateTime', 'updateGeofenceCollectionResponse_updateTime' - The time when the geofence collection was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newUpdateGeofenceCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'collectionArn'
  Prelude.Text ->
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateGeofenceCollectionResponse
newUpdateGeofenceCollectionResponse
  pHttpStatus_
  pCollectionArn_
  pCollectionName_
  pUpdateTime_ =
    UpdateGeofenceCollectionResponse'
      { httpStatus =
          pHttpStatus_,
        collectionArn = pCollectionArn_,
        collectionName = pCollectionName_,
        updateTime =
          Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateGeofenceCollectionResponse_httpStatus :: Lens.Lens' UpdateGeofenceCollectionResponse Prelude.Int
updateGeofenceCollectionResponse_httpStatus = Lens.lens (\UpdateGeofenceCollectionResponse' {httpStatus} -> httpStatus) (\s@UpdateGeofenceCollectionResponse' {} a -> s {httpStatus = a} :: UpdateGeofenceCollectionResponse)

-- | The Amazon Resource Name (ARN) of the updated geofence collection. Used
-- to specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:geofence-collection\/ExampleGeofenceCollection@
updateGeofenceCollectionResponse_collectionArn :: Lens.Lens' UpdateGeofenceCollectionResponse Prelude.Text
updateGeofenceCollectionResponse_collectionArn = Lens.lens (\UpdateGeofenceCollectionResponse' {collectionArn} -> collectionArn) (\s@UpdateGeofenceCollectionResponse' {} a -> s {collectionArn = a} :: UpdateGeofenceCollectionResponse)

-- | The name of the updated geofence collection.
updateGeofenceCollectionResponse_collectionName :: Lens.Lens' UpdateGeofenceCollectionResponse Prelude.Text
updateGeofenceCollectionResponse_collectionName = Lens.lens (\UpdateGeofenceCollectionResponse' {collectionName} -> collectionName) (\s@UpdateGeofenceCollectionResponse' {} a -> s {collectionName = a} :: UpdateGeofenceCollectionResponse)

-- | The time when the geofence collection was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
updateGeofenceCollectionResponse_updateTime :: Lens.Lens' UpdateGeofenceCollectionResponse Prelude.UTCTime
updateGeofenceCollectionResponse_updateTime = Lens.lens (\UpdateGeofenceCollectionResponse' {updateTime} -> updateTime) (\s@UpdateGeofenceCollectionResponse' {} a -> s {updateTime = a} :: UpdateGeofenceCollectionResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    UpdateGeofenceCollectionResponse
  where
  rnf UpdateGeofenceCollectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf collectionArn
      `Prelude.seq` Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf updateTime
