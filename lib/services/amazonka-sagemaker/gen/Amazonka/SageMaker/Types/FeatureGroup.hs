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
-- Module      : Amazonka.SageMaker.Types.FeatureGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureDefinition
import Amazonka.SageMaker.Types.FeatureGroupStatus
import Amazonka.SageMaker.Types.LastUpdateStatus
import Amazonka.SageMaker.Types.OfflineStoreConfig
import Amazonka.SageMaker.Types.OfflineStoreStatus
import Amazonka.SageMaker.Types.OnlineStoreConfig
import Amazonka.SageMaker.Types.Tag

-- | Amazon SageMaker Feature Store stores features in a collection called
-- Feature Group. A Feature Group can be visualized as a table which has
-- rows, with a unique identifier for each row where each column in the
-- table is a feature. In principle, a Feature Group is composed of
-- features and values per features.
--
-- /See:/ 'newFeatureGroup' smart constructor.
data FeatureGroup = FeatureGroup'
  { -- | The time a @FeatureGroup@ was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A free form description of a @FeatureGroup@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature that stores the @EventTime@ of a Record in a
    -- @FeatureGroup@.
    --
    -- A @EventTime@ is point in time when a new event occurs that corresponds
    -- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
    -- in the @FeatureGroup@ must have a corresponding @EventTime@.
    eventTimeFeatureName :: Prelude.Maybe Prelude.Text,
    -- | The reason that the @FeatureGroup@ failed to be replicated in the
    -- @OfflineStore@. This is failure may be due to a failure to create a
    -- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A list of @Feature@s. Each @Feature@ must include a @FeatureName@ and a
    -- @FeatureType@.
    --
    -- Valid @FeatureType@s are @Integral@, @Fractional@ and @String@.
    --
    -- @FeatureName@s cannot be any of the following: @is_deleted@,
    -- @write_time@, @api_invocation_time@.
    --
    -- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
    featureDefinitions :: Prelude.Maybe (Prelude.NonEmpty FeatureDefinition),
    -- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
    featureGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the @FeatureGroup@.
    featureGroupName :: Prelude.Maybe Prelude.Text,
    -- | A @FeatureGroup@ status.
    featureGroupStatus :: Prelude.Maybe FeatureGroupStatus,
    -- | A timestamp indicating the last time you updated the feature group.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A value that indicates whether the feature group was updated
    -- successfully.
    lastUpdateStatus :: Prelude.Maybe LastUpdateStatus,
    offlineStoreConfig :: Prelude.Maybe OfflineStoreConfig,
    offlineStoreStatus :: Prelude.Maybe OfflineStoreStatus,
    onlineStoreConfig :: Prelude.Maybe OnlineStoreConfig,
    -- | The name of the @Feature@ whose value uniquely identifies a @Record@
    -- defined in the @FeatureGroup@ @FeatureDefinitions@.
    recordIdentifierFeatureName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to create
    -- the feature group.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Tags used to define a @FeatureGroup@.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'featureGroup_creationTime' - The time a @FeatureGroup@ was created.
--
-- 'description', 'featureGroup_description' - A free form description of a @FeatureGroup@.
--
-- 'eventTimeFeatureName', 'featureGroup_eventTimeFeatureName' - The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
--
-- 'failureReason', 'featureGroup_failureReason' - The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
--
-- 'featureDefinitions', 'featureGroup_featureDefinitions' - A list of @Feature@s. Each @Feature@ must include a @FeatureName@ and a
-- @FeatureType@.
--
-- Valid @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- @FeatureName@s cannot be any of the following: @is_deleted@,
-- @write_time@, @api_invocation_time@.
--
-- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
--
-- 'featureGroupArn', 'featureGroup_featureGroupArn' - The Amazon Resource Name (ARN) of a @FeatureGroup@.
--
-- 'featureGroupName', 'featureGroup_featureGroupName' - The name of the @FeatureGroup@.
--
-- 'featureGroupStatus', 'featureGroup_featureGroupStatus' - A @FeatureGroup@ status.
--
-- 'lastModifiedTime', 'featureGroup_lastModifiedTime' - A timestamp indicating the last time you updated the feature group.
--
-- 'lastUpdateStatus', 'featureGroup_lastUpdateStatus' - A value that indicates whether the feature group was updated
-- successfully.
--
-- 'offlineStoreConfig', 'featureGroup_offlineStoreConfig' - Undocumented member.
--
-- 'offlineStoreStatus', 'featureGroup_offlineStoreStatus' - Undocumented member.
--
-- 'onlineStoreConfig', 'featureGroup_onlineStoreConfig' - Undocumented member.
--
-- 'recordIdentifierFeatureName', 'featureGroup_recordIdentifierFeatureName' - The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
--
-- 'roleArn', 'featureGroup_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
--
-- 'tags', 'featureGroup_tags' - Tags used to define a @FeatureGroup@.
newFeatureGroup ::
  FeatureGroup
newFeatureGroup =
  FeatureGroup'
    { creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      eventTimeFeatureName = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      featureDefinitions = Prelude.Nothing,
      featureGroupArn = Prelude.Nothing,
      featureGroupName = Prelude.Nothing,
      featureGroupStatus = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      offlineStoreConfig = Prelude.Nothing,
      offlineStoreStatus = Prelude.Nothing,
      onlineStoreConfig = Prelude.Nothing,
      recordIdentifierFeatureName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time a @FeatureGroup@ was created.
featureGroup_creationTime :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.UTCTime)
featureGroup_creationTime = Lens.lens (\FeatureGroup' {creationTime} -> creationTime) (\s@FeatureGroup' {} a -> s {creationTime = a} :: FeatureGroup) Prelude.. Lens.mapping Data._Time

-- | A free form description of a @FeatureGroup@.
featureGroup_description :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_description = Lens.lens (\FeatureGroup' {description} -> description) (\s@FeatureGroup' {} a -> s {description = a} :: FeatureGroup)

-- | The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
featureGroup_eventTimeFeatureName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_eventTimeFeatureName = Lens.lens (\FeatureGroup' {eventTimeFeatureName} -> eventTimeFeatureName) (\s@FeatureGroup' {} a -> s {eventTimeFeatureName = a} :: FeatureGroup)

-- | The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
featureGroup_failureReason :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_failureReason = Lens.lens (\FeatureGroup' {failureReason} -> failureReason) (\s@FeatureGroup' {} a -> s {failureReason = a} :: FeatureGroup)

-- | A list of @Feature@s. Each @Feature@ must include a @FeatureName@ and a
-- @FeatureType@.
--
-- Valid @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- @FeatureName@s cannot be any of the following: @is_deleted@,
-- @write_time@, @api_invocation_time@.
--
-- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
featureGroup_featureDefinitions :: Lens.Lens' FeatureGroup (Prelude.Maybe (Prelude.NonEmpty FeatureDefinition))
featureGroup_featureDefinitions = Lens.lens (\FeatureGroup' {featureDefinitions} -> featureDefinitions) (\s@FeatureGroup' {} a -> s {featureDefinitions = a} :: FeatureGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
featureGroup_featureGroupArn :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_featureGroupArn = Lens.lens (\FeatureGroup' {featureGroupArn} -> featureGroupArn) (\s@FeatureGroup' {} a -> s {featureGroupArn = a} :: FeatureGroup)

-- | The name of the @FeatureGroup@.
featureGroup_featureGroupName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_featureGroupName = Lens.lens (\FeatureGroup' {featureGroupName} -> featureGroupName) (\s@FeatureGroup' {} a -> s {featureGroupName = a} :: FeatureGroup)

-- | A @FeatureGroup@ status.
featureGroup_featureGroupStatus :: Lens.Lens' FeatureGroup (Prelude.Maybe FeatureGroupStatus)
featureGroup_featureGroupStatus = Lens.lens (\FeatureGroup' {featureGroupStatus} -> featureGroupStatus) (\s@FeatureGroup' {} a -> s {featureGroupStatus = a} :: FeatureGroup)

-- | A timestamp indicating the last time you updated the feature group.
featureGroup_lastModifiedTime :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.UTCTime)
featureGroup_lastModifiedTime = Lens.lens (\FeatureGroup' {lastModifiedTime} -> lastModifiedTime) (\s@FeatureGroup' {} a -> s {lastModifiedTime = a} :: FeatureGroup) Prelude.. Lens.mapping Data._Time

-- | A value that indicates whether the feature group was updated
-- successfully.
featureGroup_lastUpdateStatus :: Lens.Lens' FeatureGroup (Prelude.Maybe LastUpdateStatus)
featureGroup_lastUpdateStatus = Lens.lens (\FeatureGroup' {lastUpdateStatus} -> lastUpdateStatus) (\s@FeatureGroup' {} a -> s {lastUpdateStatus = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_offlineStoreConfig :: Lens.Lens' FeatureGroup (Prelude.Maybe OfflineStoreConfig)
featureGroup_offlineStoreConfig = Lens.lens (\FeatureGroup' {offlineStoreConfig} -> offlineStoreConfig) (\s@FeatureGroup' {} a -> s {offlineStoreConfig = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_offlineStoreStatus :: Lens.Lens' FeatureGroup (Prelude.Maybe OfflineStoreStatus)
featureGroup_offlineStoreStatus = Lens.lens (\FeatureGroup' {offlineStoreStatus} -> offlineStoreStatus) (\s@FeatureGroup' {} a -> s {offlineStoreStatus = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_onlineStoreConfig :: Lens.Lens' FeatureGroup (Prelude.Maybe OnlineStoreConfig)
featureGroup_onlineStoreConfig = Lens.lens (\FeatureGroup' {onlineStoreConfig} -> onlineStoreConfig) (\s@FeatureGroup' {} a -> s {onlineStoreConfig = a} :: FeatureGroup)

-- | The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
featureGroup_recordIdentifierFeatureName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_recordIdentifierFeatureName = Lens.lens (\FeatureGroup' {recordIdentifierFeatureName} -> recordIdentifierFeatureName) (\s@FeatureGroup' {} a -> s {recordIdentifierFeatureName = a} :: FeatureGroup)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
featureGroup_roleArn :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_roleArn = Lens.lens (\FeatureGroup' {roleArn} -> roleArn) (\s@FeatureGroup' {} a -> s {roleArn = a} :: FeatureGroup)

-- | Tags used to define a @FeatureGroup@.
featureGroup_tags :: Lens.Lens' FeatureGroup (Prelude.Maybe [Tag])
featureGroup_tags = Lens.lens (\FeatureGroup' {tags} -> tags) (\s@FeatureGroup' {} a -> s {tags = a} :: FeatureGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FeatureGroup where
  parseJSON =
    Data.withObject
      "FeatureGroup"
      ( \x ->
          FeatureGroup'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EventTimeFeatureName")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "FeatureDefinitions")
            Prelude.<*> (x Data..:? "FeatureGroupArn")
            Prelude.<*> (x Data..:? "FeatureGroupName")
            Prelude.<*> (x Data..:? "FeatureGroupStatus")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LastUpdateStatus")
            Prelude.<*> (x Data..:? "OfflineStoreConfig")
            Prelude.<*> (x Data..:? "OfflineStoreStatus")
            Prelude.<*> (x Data..:? "OnlineStoreConfig")
            Prelude.<*> (x Data..:? "RecordIdentifierFeatureName")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FeatureGroup where
  hashWithSalt _salt FeatureGroup' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventTimeFeatureName
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` featureDefinitions
      `Prelude.hashWithSalt` featureGroupArn
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` featureGroupStatus
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastUpdateStatus
      `Prelude.hashWithSalt` offlineStoreConfig
      `Prelude.hashWithSalt` offlineStoreStatus
      `Prelude.hashWithSalt` onlineStoreConfig
      `Prelude.hashWithSalt` recordIdentifierFeatureName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData FeatureGroup where
  rnf FeatureGroup' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventTimeFeatureName
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf featureDefinitions
      `Prelude.seq` Prelude.rnf featureGroupArn
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf featureGroupStatus
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf offlineStoreConfig
      `Prelude.seq` Prelude.rnf offlineStoreStatus
      `Prelude.seq` Prelude.rnf onlineStoreConfig
      `Prelude.seq` Prelude.rnf recordIdentifierFeatureName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
