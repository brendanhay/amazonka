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
-- Module      : Amazonka.Location.Types.ListGeofenceCollectionsResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListGeofenceCollectionsResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | Contains the geofence collection details.
--
-- /See:/ 'newListGeofenceCollectionsResponseEntry' smart constructor.
data ListGeofenceCollectionsResponseEntry = ListGeofenceCollectionsResponseEntry'
  { -- | The specified data provider for the geofence collection.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the geofence collection.
    collectionName :: Prelude.Text,
    -- | The timestamp for when the geofence collection was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Core.POSIX,
    -- | The description for the geofence collection
    description :: Prelude.Text,
    -- | The pricing plan for the specified geofence collection.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- the
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
    pricingPlan :: PricingPlan,
    -- | Specifies a timestamp for when the resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofenceCollectionsResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlanDataSource', 'listGeofenceCollectionsResponseEntry_pricingPlanDataSource' - The specified data provider for the geofence collection.
--
-- 'collectionName', 'listGeofenceCollectionsResponseEntry_collectionName' - The name of the geofence collection.
--
-- 'createTime', 'listGeofenceCollectionsResponseEntry_createTime' - The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'description', 'listGeofenceCollectionsResponseEntry_description' - The description for the geofence collection
--
-- 'pricingPlan', 'listGeofenceCollectionsResponseEntry_pricingPlan' - The pricing plan for the specified geofence collection.
--
-- For additional details and restrictions on each pricing plan option, see
-- the
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
--
-- 'updateTime', 'listGeofenceCollectionsResponseEntry_updateTime' - Specifies a timestamp for when the resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newListGeofenceCollectionsResponseEntry ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'pricingPlan'
  PricingPlan ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListGeofenceCollectionsResponseEntry
newListGeofenceCollectionsResponseEntry
  pCollectionName_
  pCreateTime_
  pDescription_
  pPricingPlan_
  pUpdateTime_ =
    ListGeofenceCollectionsResponseEntry'
      { pricingPlanDataSource =
          Prelude.Nothing,
        collectionName = pCollectionName_,
        createTime =
          Core._Time Lens.# pCreateTime_,
        description = pDescription_,
        pricingPlan = pPricingPlan_,
        updateTime =
          Core._Time Lens.# pUpdateTime_
      }

-- | The specified data provider for the geofence collection.
listGeofenceCollectionsResponseEntry_pricingPlanDataSource :: Lens.Lens' ListGeofenceCollectionsResponseEntry (Prelude.Maybe Prelude.Text)
listGeofenceCollectionsResponseEntry_pricingPlanDataSource = Lens.lens (\ListGeofenceCollectionsResponseEntry' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {pricingPlanDataSource = a} :: ListGeofenceCollectionsResponseEntry)

-- | The name of the geofence collection.
listGeofenceCollectionsResponseEntry_collectionName :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.Text
listGeofenceCollectionsResponseEntry_collectionName = Lens.lens (\ListGeofenceCollectionsResponseEntry' {collectionName} -> collectionName) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {collectionName = a} :: ListGeofenceCollectionsResponseEntry)

-- | The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceCollectionsResponseEntry_createTime :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.UTCTime
listGeofenceCollectionsResponseEntry_createTime = Lens.lens (\ListGeofenceCollectionsResponseEntry' {createTime} -> createTime) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {createTime = a} :: ListGeofenceCollectionsResponseEntry) Prelude.. Core._Time

-- | The description for the geofence collection
listGeofenceCollectionsResponseEntry_description :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.Text
listGeofenceCollectionsResponseEntry_description = Lens.lens (\ListGeofenceCollectionsResponseEntry' {description} -> description) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {description = a} :: ListGeofenceCollectionsResponseEntry)

-- | The pricing plan for the specified geofence collection.
--
-- For additional details and restrictions on each pricing plan option, see
-- the
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing page>.
listGeofenceCollectionsResponseEntry_pricingPlan :: Lens.Lens' ListGeofenceCollectionsResponseEntry PricingPlan
listGeofenceCollectionsResponseEntry_pricingPlan = Lens.lens (\ListGeofenceCollectionsResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {pricingPlan = a} :: ListGeofenceCollectionsResponseEntry)

-- | Specifies a timestamp for when the resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceCollectionsResponseEntry_updateTime :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.UTCTime
listGeofenceCollectionsResponseEntry_updateTime = Lens.lens (\ListGeofenceCollectionsResponseEntry' {updateTime} -> updateTime) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {updateTime = a} :: ListGeofenceCollectionsResponseEntry) Prelude.. Core._Time

instance
  Core.FromJSON
    ListGeofenceCollectionsResponseEntry
  where
  parseJSON =
    Core.withObject
      "ListGeofenceCollectionsResponseEntry"
      ( \x ->
          ListGeofenceCollectionsResponseEntry'
            Prelude.<$> (x Core..:? "PricingPlanDataSource")
            Prelude.<*> (x Core..: "CollectionName")
            Prelude.<*> (x Core..: "CreateTime")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "PricingPlan")
            Prelude.<*> (x Core..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListGeofenceCollectionsResponseEntry

instance
  Prelude.NFData
    ListGeofenceCollectionsResponseEntry
