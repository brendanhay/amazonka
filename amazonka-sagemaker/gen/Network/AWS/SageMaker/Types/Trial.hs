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
-- Module      : Network.AWS.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Trial where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial as returned by the Search API.
--
-- /See:/ 'newTrial' smart constructor.
data Trial = Trial'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Core.Text,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the trial was created.
    creationTime :: Core.Maybe Core.POSIX,
    source :: Core.Maybe TrialSource,
    -- | A list of the components associated with the trial. For each component,
    -- a summary of the component\'s properties is included.
    trialComponentSummaries :: Core.Maybe [TrialComponentSimpleSummary],
    -- | The list of tags that are associated with the trial. You can use Search
    -- API to search on the tags.
    tags :: Core.Maybe [Tag],
    -- | Who last modified the trial.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the trial.
    trialName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Trial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'trial_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'metadataProperties', 'trial_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'trial_creationTime' - When the trial was created.
--
-- 'source', 'trial_source' - Undocumented member.
--
-- 'trialComponentSummaries', 'trial_trialComponentSummaries' - A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
--
-- 'tags', 'trial_tags' - The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
--
-- 'lastModifiedTime', 'trial_lastModifiedTime' - Who last modified the trial.
--
-- 'experimentName', 'trial_experimentName' - The name of the experiment the trial is part of.
--
-- 'createdBy', 'trial_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'trial_lastModifiedBy' - Undocumented member.
--
-- 'displayName', 'trial_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'trial_trialName' - The name of the trial.
newTrial ::
  Trial
newTrial =
  Trial'
    { trialArn = Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      source = Core.Nothing,
      trialComponentSummaries = Core.Nothing,
      tags = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      experimentName = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing,
      trialName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the trial.
trial_trialArn :: Lens.Lens' Trial (Core.Maybe Core.Text)
trial_trialArn = Lens.lens (\Trial' {trialArn} -> trialArn) (\s@Trial' {} a -> s {trialArn = a} :: Trial)

-- | Undocumented member.
trial_metadataProperties :: Lens.Lens' Trial (Core.Maybe MetadataProperties)
trial_metadataProperties = Lens.lens (\Trial' {metadataProperties} -> metadataProperties) (\s@Trial' {} a -> s {metadataProperties = a} :: Trial)

-- | When the trial was created.
trial_creationTime :: Lens.Lens' Trial (Core.Maybe Core.UTCTime)
trial_creationTime = Lens.lens (\Trial' {creationTime} -> creationTime) (\s@Trial' {} a -> s {creationTime = a} :: Trial) Core.. Lens.mapping Core._Time

-- | Undocumented member.
trial_source :: Lens.Lens' Trial (Core.Maybe TrialSource)
trial_source = Lens.lens (\Trial' {source} -> source) (\s@Trial' {} a -> s {source = a} :: Trial)

-- | A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
trial_trialComponentSummaries :: Lens.Lens' Trial (Core.Maybe [TrialComponentSimpleSummary])
trial_trialComponentSummaries = Lens.lens (\Trial' {trialComponentSummaries} -> trialComponentSummaries) (\s@Trial' {} a -> s {trialComponentSummaries = a} :: Trial) Core.. Lens.mapping Lens._Coerce

-- | The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
trial_tags :: Lens.Lens' Trial (Core.Maybe [Tag])
trial_tags = Lens.lens (\Trial' {tags} -> tags) (\s@Trial' {} a -> s {tags = a} :: Trial) Core.. Lens.mapping Lens._Coerce

-- | Who last modified the trial.
trial_lastModifiedTime :: Lens.Lens' Trial (Core.Maybe Core.UTCTime)
trial_lastModifiedTime = Lens.lens (\Trial' {lastModifiedTime} -> lastModifiedTime) (\s@Trial' {} a -> s {lastModifiedTime = a} :: Trial) Core.. Lens.mapping Core._Time

-- | The name of the experiment the trial is part of.
trial_experimentName :: Lens.Lens' Trial (Core.Maybe Core.Text)
trial_experimentName = Lens.lens (\Trial' {experimentName} -> experimentName) (\s@Trial' {} a -> s {experimentName = a} :: Trial)

-- | Undocumented member.
trial_createdBy :: Lens.Lens' Trial (Core.Maybe UserContext)
trial_createdBy = Lens.lens (\Trial' {createdBy} -> createdBy) (\s@Trial' {} a -> s {createdBy = a} :: Trial)

-- | Undocumented member.
trial_lastModifiedBy :: Lens.Lens' Trial (Core.Maybe UserContext)
trial_lastModifiedBy = Lens.lens (\Trial' {lastModifiedBy} -> lastModifiedBy) (\s@Trial' {} a -> s {lastModifiedBy = a} :: Trial)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trial_displayName :: Lens.Lens' Trial (Core.Maybe Core.Text)
trial_displayName = Lens.lens (\Trial' {displayName} -> displayName) (\s@Trial' {} a -> s {displayName = a} :: Trial)

-- | The name of the trial.
trial_trialName :: Lens.Lens' Trial (Core.Maybe Core.Text)
trial_trialName = Lens.lens (\Trial' {trialName} -> trialName) (\s@Trial' {} a -> s {trialName = a} :: Trial)

instance Core.FromJSON Trial where
  parseJSON =
    Core.withObject
      "Trial"
      ( \x ->
          Trial'
            Core.<$> (x Core..:? "TrialArn")
            Core.<*> (x Core..:? "MetadataProperties")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Source")
            Core.<*> ( x Core..:? "TrialComponentSummaries"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ExperimentName")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "TrialName")
      )

instance Core.Hashable Trial

instance Core.NFData Trial
