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
import qualified Network.AWS.Prelude as Prelude
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
  { -- | The time a @FeatureGroup@ was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    offlineStoreConfig :: Prelude.Maybe OfflineStoreConfig,
    -- | The reason that the @FeatureGroup@ failed to be replicated in the
    -- @OfflineStore@. This is failure may be due to a failure to create a
    -- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A @FeatureGroup@ status.
    featureGroupStatus :: Prelude.Maybe FeatureGroupStatus,
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
    offlineStoreStatus :: Prelude.Maybe OfflineStoreStatus,
    onlineStoreConfig :: Prelude.Maybe OnlineStoreConfig,
    -- | The name of the feature that stores the @EventTime@ of a Record in a
    -- @FeatureGroup@.
    --
    -- A @EventTime@ is point in time when a new event occurs that corresponds
    -- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
    -- in the @FeatureGroup@ must have a corresponding @EventTime@.
    eventTimeFeatureName :: Prelude.Maybe Prelude.Text,
    -- | The name of the @Feature@ whose value uniquely identifies a @Record@
    -- defined in the @FeatureGroup@ @FeatureDefinitions@.
    recordIdentifierFeatureName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
    featureGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the @FeatureGroup@.
    featureGroupName :: Prelude.Maybe Prelude.Text,
    -- | A free form description of a @FeatureGroup@.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags used to define a @FeatureGroup@.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to create
    -- the feature group.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'offlineStoreConfig', 'featureGroup_offlineStoreConfig' - Undocumented member.
--
-- 'failureReason', 'featureGroup_failureReason' - The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
--
-- 'featureGroupStatus', 'featureGroup_featureGroupStatus' - A @FeatureGroup@ status.
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
-- 'onlineStoreConfig', 'featureGroup_onlineStoreConfig' - Undocumented member.
--
-- 'eventTimeFeatureName', 'featureGroup_eventTimeFeatureName' - The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
--
-- 'recordIdentifierFeatureName', 'featureGroup_recordIdentifierFeatureName' - The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
--
-- 'featureGroupArn', 'featureGroup_featureGroupArn' - The Amazon Resource Name (ARN) of a @FeatureGroup@.
--
-- 'featureGroupName', 'featureGroup_featureGroupName' - The name of the @FeatureGroup@.
--
-- 'description', 'featureGroup_description' - A free form description of a @FeatureGroup@.
--
-- 'tags', 'featureGroup_tags' - Tags used to define a @FeatureGroup@.
--
-- 'roleArn', 'featureGroup_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
newFeatureGroup ::
  FeatureGroup
newFeatureGroup =
  FeatureGroup'
    { creationTime = Prelude.Nothing,
      offlineStoreConfig = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      featureGroupStatus = Prelude.Nothing,
      featureDefinitions = Prelude.Nothing,
      offlineStoreStatus = Prelude.Nothing,
      onlineStoreConfig = Prelude.Nothing,
      eventTimeFeatureName = Prelude.Nothing,
      recordIdentifierFeatureName = Prelude.Nothing,
      featureGroupArn = Prelude.Nothing,
      featureGroupName = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time a @FeatureGroup@ was created.
featureGroup_creationTime :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.UTCTime)
featureGroup_creationTime = Lens.lens (\FeatureGroup' {creationTime} -> creationTime) (\s@FeatureGroup' {} a -> s {creationTime = a} :: FeatureGroup) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
featureGroup_offlineStoreConfig :: Lens.Lens' FeatureGroup (Prelude.Maybe OfflineStoreConfig)
featureGroup_offlineStoreConfig = Lens.lens (\FeatureGroup' {offlineStoreConfig} -> offlineStoreConfig) (\s@FeatureGroup' {} a -> s {offlineStoreConfig = a} :: FeatureGroup)

-- | The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure may be due to a failure to create a
-- @FeatureGroup@ in or delete a @FeatureGroup@ from the @OfflineStore@.
featureGroup_failureReason :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_failureReason = Lens.lens (\FeatureGroup' {failureReason} -> failureReason) (\s@FeatureGroup' {} a -> s {failureReason = a} :: FeatureGroup)

-- | A @FeatureGroup@ status.
featureGroup_featureGroupStatus :: Lens.Lens' FeatureGroup (Prelude.Maybe FeatureGroupStatus)
featureGroup_featureGroupStatus = Lens.lens (\FeatureGroup' {featureGroupStatus} -> featureGroupStatus) (\s@FeatureGroup' {} a -> s {featureGroupStatus = a} :: FeatureGroup)

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

-- | Undocumented member.
featureGroup_offlineStoreStatus :: Lens.Lens' FeatureGroup (Prelude.Maybe OfflineStoreStatus)
featureGroup_offlineStoreStatus = Lens.lens (\FeatureGroup' {offlineStoreStatus} -> offlineStoreStatus) (\s@FeatureGroup' {} a -> s {offlineStoreStatus = a} :: FeatureGroup)

-- | Undocumented member.
featureGroup_onlineStoreConfig :: Lens.Lens' FeatureGroup (Prelude.Maybe OnlineStoreConfig)
featureGroup_onlineStoreConfig = Lens.lens (\FeatureGroup' {onlineStoreConfig} -> onlineStoreConfig) (\s@FeatureGroup' {} a -> s {onlineStoreConfig = a} :: FeatureGroup)

-- | The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- A @EventTime@ is point in time when a new event occurs that corresponds
-- to the creation or update of a @Record@ in @FeatureGroup@. All @Records@
-- in the @FeatureGroup@ must have a corresponding @EventTime@.
featureGroup_eventTimeFeatureName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_eventTimeFeatureName = Lens.lens (\FeatureGroup' {eventTimeFeatureName} -> eventTimeFeatureName) (\s@FeatureGroup' {} a -> s {eventTimeFeatureName = a} :: FeatureGroup)

-- | The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureGroup@ @FeatureDefinitions@.
featureGroup_recordIdentifierFeatureName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_recordIdentifierFeatureName = Lens.lens (\FeatureGroup' {recordIdentifierFeatureName} -> recordIdentifierFeatureName) (\s@FeatureGroup' {} a -> s {recordIdentifierFeatureName = a} :: FeatureGroup)

-- | The Amazon Resource Name (ARN) of a @FeatureGroup@.
featureGroup_featureGroupArn :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_featureGroupArn = Lens.lens (\FeatureGroup' {featureGroupArn} -> featureGroupArn) (\s@FeatureGroup' {} a -> s {featureGroupArn = a} :: FeatureGroup)

-- | The name of the @FeatureGroup@.
featureGroup_featureGroupName :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_featureGroupName = Lens.lens (\FeatureGroup' {featureGroupName} -> featureGroupName) (\s@FeatureGroup' {} a -> s {featureGroupName = a} :: FeatureGroup)

-- | A free form description of a @FeatureGroup@.
featureGroup_description :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_description = Lens.lens (\FeatureGroup' {description} -> description) (\s@FeatureGroup' {} a -> s {description = a} :: FeatureGroup)

-- | Tags used to define a @FeatureGroup@.
featureGroup_tags :: Lens.Lens' FeatureGroup (Prelude.Maybe [Tag])
featureGroup_tags = Lens.lens (\FeatureGroup' {tags} -> tags) (\s@FeatureGroup' {} a -> s {tags = a} :: FeatureGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM execution role used to create
-- the feature group.
featureGroup_roleArn :: Lens.Lens' FeatureGroup (Prelude.Maybe Prelude.Text)
featureGroup_roleArn = Lens.lens (\FeatureGroup' {roleArn} -> roleArn) (\s@FeatureGroup' {} a -> s {roleArn = a} :: FeatureGroup)

instance Core.FromJSON FeatureGroup where
  parseJSON =
    Core.withObject
      "FeatureGroup"
      ( \x ->
          FeatureGroup'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "OfflineStoreConfig")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "FeatureGroupStatus")
            Prelude.<*> (x Core..:? "FeatureDefinitions")
            Prelude.<*> (x Core..:? "OfflineStoreStatus")
            Prelude.<*> (x Core..:? "OnlineStoreConfig")
            Prelude.<*> (x Core..:? "EventTimeFeatureName")
            Prelude.<*> (x Core..:? "RecordIdentifierFeatureName")
            Prelude.<*> (x Core..:? "FeatureGroupArn")
            Prelude.<*> (x Core..:? "FeatureGroupName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable FeatureGroup

instance Prelude.NFData FeatureGroup
