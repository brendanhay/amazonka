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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListGeofenceCollectionsResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | Contains the geofence collection details.
--
-- /See:/ 'newListGeofenceCollectionsResponseEntry' smart constructor.
data ListGeofenceCollectionsResponseEntry = ListGeofenceCollectionsResponseEntry'
  { -- | No longer used. Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | No longer used. Always returns an empty string.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the geofence collection.
    collectionName :: Prelude.Text,
    -- | The timestamp for when the geofence collection was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Data.ISO8601,
    -- | The description for the geofence collection
    description :: Prelude.Text,
    -- | Specifies a timestamp for when the resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
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
-- 'pricingPlan', 'listGeofenceCollectionsResponseEntry_pricingPlan' - No longer used. Always returns @RequestBasedUsage@.
--
-- 'pricingPlanDataSource', 'listGeofenceCollectionsResponseEntry_pricingPlanDataSource' - No longer used. Always returns an empty string.
--
-- 'collectionName', 'listGeofenceCollectionsResponseEntry_collectionName' - The name of the geofence collection.
--
-- 'createTime', 'listGeofenceCollectionsResponseEntry_createTime' - The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'description', 'listGeofenceCollectionsResponseEntry_description' - The description for the geofence collection
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
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListGeofenceCollectionsResponseEntry
newListGeofenceCollectionsResponseEntry
  pCollectionName_
  pCreateTime_
  pDescription_
  pUpdateTime_ =
    ListGeofenceCollectionsResponseEntry'
      { pricingPlan =
          Prelude.Nothing,
        pricingPlanDataSource =
          Prelude.Nothing,
        collectionName = pCollectionName_,
        createTime =
          Data._Time Lens.# pCreateTime_,
        description = pDescription_,
        updateTime =
          Data._Time Lens.# pUpdateTime_
      }

-- | No longer used. Always returns @RequestBasedUsage@.
listGeofenceCollectionsResponseEntry_pricingPlan :: Lens.Lens' ListGeofenceCollectionsResponseEntry (Prelude.Maybe PricingPlan)
listGeofenceCollectionsResponseEntry_pricingPlan = Lens.lens (\ListGeofenceCollectionsResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {pricingPlan = a} :: ListGeofenceCollectionsResponseEntry)

-- | No longer used. Always returns an empty string.
listGeofenceCollectionsResponseEntry_pricingPlanDataSource :: Lens.Lens' ListGeofenceCollectionsResponseEntry (Prelude.Maybe Prelude.Text)
listGeofenceCollectionsResponseEntry_pricingPlanDataSource = Lens.lens (\ListGeofenceCollectionsResponseEntry' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {pricingPlanDataSource = a} :: ListGeofenceCollectionsResponseEntry)

-- | The name of the geofence collection.
listGeofenceCollectionsResponseEntry_collectionName :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.Text
listGeofenceCollectionsResponseEntry_collectionName = Lens.lens (\ListGeofenceCollectionsResponseEntry' {collectionName} -> collectionName) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {collectionName = a} :: ListGeofenceCollectionsResponseEntry)

-- | The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceCollectionsResponseEntry_createTime :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.UTCTime
listGeofenceCollectionsResponseEntry_createTime = Lens.lens (\ListGeofenceCollectionsResponseEntry' {createTime} -> createTime) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {createTime = a} :: ListGeofenceCollectionsResponseEntry) Prelude.. Data._Time

-- | The description for the geofence collection
listGeofenceCollectionsResponseEntry_description :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.Text
listGeofenceCollectionsResponseEntry_description = Lens.lens (\ListGeofenceCollectionsResponseEntry' {description} -> description) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {description = a} :: ListGeofenceCollectionsResponseEntry)

-- | Specifies a timestamp for when the resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceCollectionsResponseEntry_updateTime :: Lens.Lens' ListGeofenceCollectionsResponseEntry Prelude.UTCTime
listGeofenceCollectionsResponseEntry_updateTime = Lens.lens (\ListGeofenceCollectionsResponseEntry' {updateTime} -> updateTime) (\s@ListGeofenceCollectionsResponseEntry' {} a -> s {updateTime = a} :: ListGeofenceCollectionsResponseEntry) Prelude.. Data._Time

instance
  Data.FromJSON
    ListGeofenceCollectionsResponseEntry
  where
  parseJSON =
    Data.withObject
      "ListGeofenceCollectionsResponseEntry"
      ( \x ->
          ListGeofenceCollectionsResponseEntry'
            Prelude.<$> (x Data..:? "PricingPlan")
            Prelude.<*> (x Data..:? "PricingPlanDataSource")
            Prelude.<*> (x Data..: "CollectionName")
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListGeofenceCollectionsResponseEntry
  where
  hashWithSalt
    _salt
    ListGeofenceCollectionsResponseEntry' {..} =
      _salt
        `Prelude.hashWithSalt` pricingPlan
        `Prelude.hashWithSalt` pricingPlanDataSource
        `Prelude.hashWithSalt` collectionName
        `Prelude.hashWithSalt` createTime
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` updateTime

instance
  Prelude.NFData
    ListGeofenceCollectionsResponseEntry
  where
  rnf ListGeofenceCollectionsResponseEntry' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf updateTime
