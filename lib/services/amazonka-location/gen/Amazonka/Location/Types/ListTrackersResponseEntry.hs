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
-- Module      : Amazonka.Location.Types.ListTrackersResponseEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListTrackersResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | Contains the tracker resource details.
--
-- /See:/ 'newListTrackersResponseEntry' smart constructor.
data ListTrackersResponseEntry = ListTrackersResponseEntry'
  { -- | Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | No longer used. Always returns an empty string.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when the tracker resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The description for the tracker resource.
    description :: Prelude.Text,
    -- | The name of the tracker resource.
    trackerName :: Prelude.Text,
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrackersResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'listTrackersResponseEntry_pricingPlan' - Always returns @RequestBasedUsage@.
--
-- 'pricingPlanDataSource', 'listTrackersResponseEntry_pricingPlanDataSource' - No longer used. Always returns an empty string.
--
-- 'createTime', 'listTrackersResponseEntry_createTime' - The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'description', 'listTrackersResponseEntry_description' - The description for the tracker resource.
--
-- 'trackerName', 'listTrackersResponseEntry_trackerName' - The name of the tracker resource.
--
-- 'updateTime', 'listTrackersResponseEntry_updateTime' - The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListTrackersResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListTrackersResponseEntry
newListTrackersResponseEntry
  pCreateTime_
  pDescription_
  pTrackerName_
  pUpdateTime_ =
    ListTrackersResponseEntry'
      { pricingPlan =
          Prelude.Nothing,
        pricingPlanDataSource = Prelude.Nothing,
        createTime = Data._Time Lens.# pCreateTime_,
        description = pDescription_,
        trackerName = pTrackerName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | Always returns @RequestBasedUsage@.
listTrackersResponseEntry_pricingPlan :: Lens.Lens' ListTrackersResponseEntry (Prelude.Maybe PricingPlan)
listTrackersResponseEntry_pricingPlan = Lens.lens (\ListTrackersResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListTrackersResponseEntry' {} a -> s {pricingPlan = a} :: ListTrackersResponseEntry)

-- | No longer used. Always returns an empty string.
listTrackersResponseEntry_pricingPlanDataSource :: Lens.Lens' ListTrackersResponseEntry (Prelude.Maybe Prelude.Text)
listTrackersResponseEntry_pricingPlanDataSource = Lens.lens (\ListTrackersResponseEntry' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@ListTrackersResponseEntry' {} a -> s {pricingPlanDataSource = a} :: ListTrackersResponseEntry)

-- | The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listTrackersResponseEntry_createTime :: Lens.Lens' ListTrackersResponseEntry Prelude.UTCTime
listTrackersResponseEntry_createTime = Lens.lens (\ListTrackersResponseEntry' {createTime} -> createTime) (\s@ListTrackersResponseEntry' {} a -> s {createTime = a} :: ListTrackersResponseEntry) Prelude.. Data._Time

-- | The description for the tracker resource.
listTrackersResponseEntry_description :: Lens.Lens' ListTrackersResponseEntry Prelude.Text
listTrackersResponseEntry_description = Lens.lens (\ListTrackersResponseEntry' {description} -> description) (\s@ListTrackersResponseEntry' {} a -> s {description = a} :: ListTrackersResponseEntry)

-- | The name of the tracker resource.
listTrackersResponseEntry_trackerName :: Lens.Lens' ListTrackersResponseEntry Prelude.Text
listTrackersResponseEntry_trackerName = Lens.lens (\ListTrackersResponseEntry' {trackerName} -> trackerName) (\s@ListTrackersResponseEntry' {} a -> s {trackerName = a} :: ListTrackersResponseEntry)

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listTrackersResponseEntry_updateTime :: Lens.Lens' ListTrackersResponseEntry Prelude.UTCTime
listTrackersResponseEntry_updateTime = Lens.lens (\ListTrackersResponseEntry' {updateTime} -> updateTime) (\s@ListTrackersResponseEntry' {} a -> s {updateTime = a} :: ListTrackersResponseEntry) Prelude.. Data._Time

instance Data.FromJSON ListTrackersResponseEntry where
  parseJSON =
    Data.withObject
      "ListTrackersResponseEntry"
      ( \x ->
          ListTrackersResponseEntry'
            Prelude.<$> (x Data..:? "PricingPlan")
            Prelude.<*> (x Data..:? "PricingPlanDataSource")
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "TrackerName")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance Prelude.Hashable ListTrackersResponseEntry where
  hashWithSalt _salt ListTrackersResponseEntry' {..} =
    _salt
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` pricingPlanDataSource
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` trackerName
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ListTrackersResponseEntry where
  rnf ListTrackersResponseEntry' {..} =
    Prelude.rnf pricingPlan `Prelude.seq`
      Prelude.rnf pricingPlanDataSource `Prelude.seq`
        Prelude.rnf createTime `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf trackerName `Prelude.seq`
              Prelude.rnf updateTime
