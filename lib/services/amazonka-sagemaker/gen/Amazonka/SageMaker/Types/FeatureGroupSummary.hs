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
-- Module      : Amazonka.SageMaker.Types.FeatureGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureGroupStatus
import Amazonka.SageMaker.Types.OfflineStoreStatus

-- | The name, Arn, @CreationTime@, @FeatureGroup@ values, @LastUpdatedTime@
-- and @EnableOnlineStorage@ status of a @FeatureGroup@.
--
-- /See:/ 'newFeatureGroupSummary' smart constructor.
data FeatureGroupSummary = FeatureGroupSummary'
  { -- | The status of a FeatureGroup. The status can be any of the following:
    -- @Creating@, @Created@, @CreateFail@, @Deleting@ or @DetailFail@.
    featureGroupStatus :: Prelude.Maybe FeatureGroupStatus,
    -- | Notifies you if replicating data into the @OfflineStore@ has failed.
    -- Returns either: @Active@ or @Blocked@.
    offlineStoreStatus :: Prelude.Maybe OfflineStoreStatus,
    -- | The name of @FeatureGroup@.
    featureGroupName :: Prelude.Text,
    -- | Unique identifier for the @FeatureGroup@.
    featureGroupArn :: Prelude.Text,
    -- | A timestamp indicating the time of creation time of the @FeatureGroup@.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupStatus', 'featureGroupSummary_featureGroupStatus' - The status of a FeatureGroup. The status can be any of the following:
-- @Creating@, @Created@, @CreateFail@, @Deleting@ or @DetailFail@.
--
-- 'offlineStoreStatus', 'featureGroupSummary_offlineStoreStatus' - Notifies you if replicating data into the @OfflineStore@ has failed.
-- Returns either: @Active@ or @Blocked@.
--
-- 'featureGroupName', 'featureGroupSummary_featureGroupName' - The name of @FeatureGroup@.
--
-- 'featureGroupArn', 'featureGroupSummary_featureGroupArn' - Unique identifier for the @FeatureGroup@.
--
-- 'creationTime', 'featureGroupSummary_creationTime' - A timestamp indicating the time of creation time of the @FeatureGroup@.
newFeatureGroupSummary ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'featureGroupArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  FeatureGroupSummary
newFeatureGroupSummary
  pFeatureGroupName_
  pFeatureGroupArn_
  pCreationTime_ =
    FeatureGroupSummary'
      { featureGroupStatus =
          Prelude.Nothing,
        offlineStoreStatus = Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        featureGroupArn = pFeatureGroupArn_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The status of a FeatureGroup. The status can be any of the following:
-- @Creating@, @Created@, @CreateFail@, @Deleting@ or @DetailFail@.
featureGroupSummary_featureGroupStatus :: Lens.Lens' FeatureGroupSummary (Prelude.Maybe FeatureGroupStatus)
featureGroupSummary_featureGroupStatus = Lens.lens (\FeatureGroupSummary' {featureGroupStatus} -> featureGroupStatus) (\s@FeatureGroupSummary' {} a -> s {featureGroupStatus = a} :: FeatureGroupSummary)

-- | Notifies you if replicating data into the @OfflineStore@ has failed.
-- Returns either: @Active@ or @Blocked@.
featureGroupSummary_offlineStoreStatus :: Lens.Lens' FeatureGroupSummary (Prelude.Maybe OfflineStoreStatus)
featureGroupSummary_offlineStoreStatus = Lens.lens (\FeatureGroupSummary' {offlineStoreStatus} -> offlineStoreStatus) (\s@FeatureGroupSummary' {} a -> s {offlineStoreStatus = a} :: FeatureGroupSummary)

-- | The name of @FeatureGroup@.
featureGroupSummary_featureGroupName :: Lens.Lens' FeatureGroupSummary Prelude.Text
featureGroupSummary_featureGroupName = Lens.lens (\FeatureGroupSummary' {featureGroupName} -> featureGroupName) (\s@FeatureGroupSummary' {} a -> s {featureGroupName = a} :: FeatureGroupSummary)

-- | Unique identifier for the @FeatureGroup@.
featureGroupSummary_featureGroupArn :: Lens.Lens' FeatureGroupSummary Prelude.Text
featureGroupSummary_featureGroupArn = Lens.lens (\FeatureGroupSummary' {featureGroupArn} -> featureGroupArn) (\s@FeatureGroupSummary' {} a -> s {featureGroupArn = a} :: FeatureGroupSummary)

-- | A timestamp indicating the time of creation time of the @FeatureGroup@.
featureGroupSummary_creationTime :: Lens.Lens' FeatureGroupSummary Prelude.UTCTime
featureGroupSummary_creationTime = Lens.lens (\FeatureGroupSummary' {creationTime} -> creationTime) (\s@FeatureGroupSummary' {} a -> s {creationTime = a} :: FeatureGroupSummary) Prelude.. Data._Time

instance Data.FromJSON FeatureGroupSummary where
  parseJSON =
    Data.withObject
      "FeatureGroupSummary"
      ( \x ->
          FeatureGroupSummary'
            Prelude.<$> (x Data..:? "FeatureGroupStatus")
            Prelude.<*> (x Data..:? "OfflineStoreStatus")
            Prelude.<*> (x Data..: "FeatureGroupName")
            Prelude.<*> (x Data..: "FeatureGroupArn")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable FeatureGroupSummary where
  hashWithSalt _salt FeatureGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` featureGroupStatus
      `Prelude.hashWithSalt` offlineStoreStatus
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` featureGroupArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData FeatureGroupSummary where
  rnf FeatureGroupSummary' {..} =
    Prelude.rnf featureGroupStatus
      `Prelude.seq` Prelude.rnf offlineStoreStatus
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf featureGroupArn
      `Prelude.seq` Prelude.rnf creationTime
