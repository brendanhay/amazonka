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
-- Module      : Network.AWS.SageMaker.Types.FeatureGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FeatureGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.FeatureDefinition
import Network.AWS.SageMaker.Types.FeatureGroupStatus
import Network.AWS.SageMaker.Types.OfflineStoreConfig
import Network.AWS.SageMaker.Types.OfflineStoreStatus
import Network.AWS.SageMaker.Types.OnlineStoreConfig
import Network.AWS.SageMaker.Types.Tag

-- | Amazon SageMaker Feature Store stores features in a collection called
-- Feature Group. A Feature Group can be visualized as a table which has
-- rows, with a unique identifier for each row where each column in the
-- table is a feature. In principle, a Feature Group is composed of
-- features and values per features.
--
-- /See:/ 'newFeatureGroup' smart constructor.
data FeatureGroup = FeatureGroup'
  { -- | A @FeatureGroup@ status.
    featureGroupStatus :: Core.Maybe FeatureGroupStatus,
    offlineStoreConfig :: Core.Maybe OfflineStoreConfig,
    -- | The time a @FeatureGroup@ was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to create
    -- the feature group.
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
    featureGroupArn :: Core.Maybe Core.Text,
    -- | The name of the @Feature@ whose value uniquely identifies a @Record@
    -- defined in the @FeatureGroup@ @FeatureDefinitions@.
    recordIdentifierFeatureName :: Core.Maybe Core.Text,
    -- | A list of @Feature@s. Each @Feature@ must include a @FeatureName@ and a
    -- @FeatureType@.
    --
    -- Valid @FeatureType@s are @Integral@, @Fractional@ and @String@.
    --
    -- @FeatureName@s cannot be any of the following: @is_deleted@,
    -- @write_time@, @api_invocation_time@.
    --
    -- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
    featureDefinitions :: Core.Maybe (Core.NonEmpty FeatureDefinition),
    offlineStoreStatus :: Core.Maybe OfflineStoreStatus,
    -- | The name of the feature that stores the @EventTime@ of a Record in a
    -- @FeatureGroup@.
    --
    -- A @EventTime@ is point in time when a new event occurs that corresponds
    -- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
    -- in the @FeatureGroup@ must have a corresponding @EventTime@.
    eventTimeFeatureName :: Core.Maybe Core.Text,
    -- | The name of the @FeatureGroup@.
    featureGroupName :: Core.Maybe Core.Text,
    -- | The reason that the @FeatureGroup@ failed to be replicated in the
    -- @OfflineStore@. This is failure may be due to a failure to create a
    -- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
    failureReason :: Core.Maybe Core.Text,
    -- | Tags used to define a @FeatureGroup@.
    tags :: Core.Maybe [Tag],
    -- | A free form description of a @FeatureGroup@.
    description :: Core.Maybe Core.Text,
    onlineStoreConfig :: Core.Maybe OnlineStoreConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupStatus', 'featureGroup_featureGroupStatus' - A @FeatureGroup@ status.
--
-- 'offlineStoreConfig', 'featureGroup_offlineStoreConfig' - Undocumented member.
--
-- 'creationTime', 'featureGroup_creationTime' - The time a @FeatureGroup@ was created.
--
-- 'roleArn', 'featureGroup_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
--
-- 'featureGroupArn', 'featureGroup_featureGroupArn' - The Amazon Resource Name (ARN) of a @FeatureGroup@.
--
-- 'recordIdentifierFeatureName', 'featureGroup_recordIdentifierFeatureName' - The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
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
-- 'offlineStoreStatus', 'featureGroup_offlineStoreStatus' - Undocumented member.
--
-- 'eventTimeFeatureName', 'featureGroup_eventTimeFeatureName' - The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
--
-- 'featureGroupName', 'featureGroup_featureGroupName' - The name of the @FeatureGroup@.
--
-- 'failureReason', 'featureGroup_failureReason' - The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
--
-- 'tags', 'featureGroup_tags' - Tags used to define a @FeatureGroup@.
--
-- 'description', 'featureGroup_description' - A free form description of a @FeatureGroup@.
--
-- 'onlineStoreConfig', 'featureGroup_onlineStoreConfig' - Undocumented member.
newFeatureGroup ::
  FeatureGroup
newFeatureGroup =
  FeatureGroup'
    { featureGroupStatus = Core.Nothing,
      offlineStoreConfig = Core.Nothing,
      creationTime = Core.Nothing,
      roleArn = Core.Nothing,
      featureGroupArn = Core.Nothing,
      recordIdentifierFeatureName = Core.Nothing,
      featureDefinitions = Core.Nothing,
      offlineStoreStatus = Core.Nothing,
      eventTimeFeatureName = Core.Nothing,
      featureGroupName = Core.Nothing,
      failureReason = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      onlineStoreConfig = Core.Nothing
    }

-- | A @FeatureGroup@ status.
featureGroup_featureGroupStatus :: Lens.Lens' FeatureGroup (Core.Maybe FeatureGroupStatus)
featureGroup_featureGroupStatus = Lens.lens (\FeatureGroup' {featureGroupStatus} -> featureGroupStatus) (\s@FeatureGroup' {} a -> s {featureGroupStatus = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_offlineStoreConfig :: Lens.Lens' FeatureGroup (Core.Maybe OfflineStoreConfig)
featureGroup_offlineStoreConfig = Lens.lens (\FeatureGroup' {offlineStoreConfig} -> offlineStoreConfig) (\s@FeatureGroup' {} a -> s {offlineStoreConfig = a} :: FeatureGroup)

-- | The time a @FeatureGroup@ was created.
featureGroup_creationTime :: Lens.Lens' FeatureGroup (Core.Maybe Core.UTCTime)
featureGroup_creationTime = Lens.lens (\FeatureGroup' {creationTime} -> creationTime) (\s@FeatureGroup' {} a -> s {creationTime = a} :: FeatureGroup) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
featureGroup_roleArn :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_roleArn = Lens.lens (\FeatureGroup' {roleArn} -> roleArn) (\s@FeatureGroup' {} a -> s {roleArn = a} :: FeatureGroup)

-- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
featureGroup_featureGroupArn :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_featureGroupArn = Lens.lens (\FeatureGroup' {featureGroupArn} -> featureGroupArn) (\s@FeatureGroup' {} a -> s {featureGroupArn = a} :: FeatureGroup)

-- | The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
featureGroup_recordIdentifierFeatureName :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_recordIdentifierFeatureName = Lens.lens (\FeatureGroup' {recordIdentifierFeatureName} -> recordIdentifierFeatureName) (\s@FeatureGroup' {} a -> s {recordIdentifierFeatureName = a} :: FeatureGroup)

-- | A list of @Feature@s. Each @Feature@ must include a @FeatureName@ and a
-- @FeatureType@.
--
-- Valid @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- @FeatureName@s cannot be any of the following: @is_deleted@,
-- @write_time@, @api_invocation_time@.
--
-- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
featureGroup_featureDefinitions :: Lens.Lens' FeatureGroup (Core.Maybe (Core.NonEmpty FeatureDefinition))
featureGroup_featureDefinitions = Lens.lens (\FeatureGroup' {featureDefinitions} -> featureDefinitions) (\s@FeatureGroup' {} a -> s {featureDefinitions = a} :: FeatureGroup) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
featureGroup_offlineStoreStatus :: Lens.Lens' FeatureGroup (Core.Maybe OfflineStoreStatus)
featureGroup_offlineStoreStatus = Lens.lens (\FeatureGroup' {offlineStoreStatus} -> offlineStoreStatus) (\s@FeatureGroup' {} a -> s {offlineStoreStatus = a} :: FeatureGroup)

-- | The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
featureGroup_eventTimeFeatureName :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_eventTimeFeatureName = Lens.lens (\FeatureGroup' {eventTimeFeatureName} -> eventTimeFeatureName) (\s@FeatureGroup' {} a -> s {eventTimeFeatureName = a} :: FeatureGroup)

-- | The name of the @FeatureGroup@.
featureGroup_featureGroupName :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_featureGroupName = Lens.lens (\FeatureGroup' {featureGroupName} -> featureGroupName) (\s@FeatureGroup' {} a -> s {featureGroupName = a} :: FeatureGroup)

-- | The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
featureGroup_failureReason :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_failureReason = Lens.lens (\FeatureGroup' {failureReason} -> failureReason) (\s@FeatureGroup' {} a -> s {failureReason = a} :: FeatureGroup)

-- | Tags used to define a @FeatureGroup@.
featureGroup_tags :: Lens.Lens' FeatureGroup (Core.Maybe [Tag])
featureGroup_tags = Lens.lens (\FeatureGroup' {tags} -> tags) (\s@FeatureGroup' {} a -> s {tags = a} :: FeatureGroup) Core.. Lens.mapping Lens._Coerce

-- | A free form description of a @FeatureGroup@.
featureGroup_description :: Lens.Lens' FeatureGroup (Core.Maybe Core.Text)
featureGroup_description = Lens.lens (\FeatureGroup' {description} -> description) (\s@FeatureGroup' {} a -> s {description = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_onlineStoreConfig :: Lens.Lens' FeatureGroup (Core.Maybe OnlineStoreConfig)
featureGroup_onlineStoreConfig = Lens.lens (\FeatureGroup' {onlineStoreConfig} -> onlineStoreConfig) (\s@FeatureGroup' {} a -> s {onlineStoreConfig = a} :: FeatureGroup)

instance Core.FromJSON FeatureGroup where
  parseJSON =
    Core.withObject
      "FeatureGroup"
      ( \x ->
          FeatureGroup'
            Core.<$> (x Core..:? "FeatureGroupStatus")
            Core.<*> (x Core..:? "OfflineStoreConfig")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "FeatureGroupArn")
            Core.<*> (x Core..:? "RecordIdentifierFeatureName")
            Core.<*> (x Core..:? "FeatureDefinitions")
            Core.<*> (x Core..:? "OfflineStoreStatus")
            Core.<*> (x Core..:? "EventTimeFeatureName")
            Core.<*> (x Core..:? "FeatureGroupName")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "OnlineStoreConfig")
      )

instance Core.Hashable FeatureGroup

instance Core.NFData FeatureGroup
