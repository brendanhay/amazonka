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
-- Module      : Network.AWS.SageMaker.Types.Experiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Experiment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of an experiment as returned by the Search API.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Core.Text,
    -- | When the experiment was created.
    creationTime :: Core.Maybe Core.POSIX,
    source :: Core.Maybe ExperimentSource,
    -- | The list of tags that are associated with the experiment. You can use
    -- Search API to search on the tags.
    tags :: Core.Maybe [Tag],
    -- | When the experiment was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Core.Maybe Core.Text,
    -- | The description of the experiment.
    description :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Experiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'experiment_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'creationTime', 'experiment_creationTime' - When the experiment was created.
--
-- 'source', 'experiment_source' - Undocumented member.
--
-- 'tags', 'experiment_tags' - The list of tags that are associated with the experiment. You can use
-- Search API to search on the tags.
--
-- 'lastModifiedTime', 'experiment_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentName', 'experiment_experimentName' - The name of the experiment.
--
-- 'description', 'experiment_description' - The description of the experiment.
--
-- 'createdBy', 'experiment_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'experiment_lastModifiedBy' - Undocumented member.
--
-- 'displayName', 'experiment_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
newExperiment ::
  Experiment
newExperiment =
  Experiment'
    { experimentArn = Core.Nothing,
      creationTime = Core.Nothing,
      source = Core.Nothing,
      tags = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      experimentName = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the experiment.
experiment_experimentArn :: Lens.Lens' Experiment (Core.Maybe Core.Text)
experiment_experimentArn = Lens.lens (\Experiment' {experimentArn} -> experimentArn) (\s@Experiment' {} a -> s {experimentArn = a} :: Experiment)

-- | When the experiment was created.
experiment_creationTime :: Lens.Lens' Experiment (Core.Maybe Core.UTCTime)
experiment_creationTime = Lens.lens (\Experiment' {creationTime} -> creationTime) (\s@Experiment' {} a -> s {creationTime = a} :: Experiment) Core.. Lens.mapping Core._Time

-- | Undocumented member.
experiment_source :: Lens.Lens' Experiment (Core.Maybe ExperimentSource)
experiment_source = Lens.lens (\Experiment' {source} -> source) (\s@Experiment' {} a -> s {source = a} :: Experiment)

-- | The list of tags that are associated with the experiment. You can use
-- Search API to search on the tags.
experiment_tags :: Lens.Lens' Experiment (Core.Maybe [Tag])
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Core.. Lens.mapping Lens._Coerce

-- | When the experiment was last modified.
experiment_lastModifiedTime :: Lens.Lens' Experiment (Core.Maybe Core.UTCTime)
experiment_lastModifiedTime = Lens.lens (\Experiment' {lastModifiedTime} -> lastModifiedTime) (\s@Experiment' {} a -> s {lastModifiedTime = a} :: Experiment) Core.. Lens.mapping Core._Time

-- | The name of the experiment.
experiment_experimentName :: Lens.Lens' Experiment (Core.Maybe Core.Text)
experiment_experimentName = Lens.lens (\Experiment' {experimentName} -> experimentName) (\s@Experiment' {} a -> s {experimentName = a} :: Experiment)

-- | The description of the experiment.
experiment_description :: Lens.Lens' Experiment (Core.Maybe Core.Text)
experiment_description = Lens.lens (\Experiment' {description} -> description) (\s@Experiment' {} a -> s {description = a} :: Experiment)

-- | Undocumented member.
experiment_createdBy :: Lens.Lens' Experiment (Core.Maybe UserContext)
experiment_createdBy = Lens.lens (\Experiment' {createdBy} -> createdBy) (\s@Experiment' {} a -> s {createdBy = a} :: Experiment)

-- | Undocumented member.
experiment_lastModifiedBy :: Lens.Lens' Experiment (Core.Maybe UserContext)
experiment_lastModifiedBy = Lens.lens (\Experiment' {lastModifiedBy} -> lastModifiedBy) (\s@Experiment' {} a -> s {lastModifiedBy = a} :: Experiment)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
experiment_displayName :: Lens.Lens' Experiment (Core.Maybe Core.Text)
experiment_displayName = Lens.lens (\Experiment' {displayName} -> displayName) (\s@Experiment' {} a -> s {displayName = a} :: Experiment)

instance Core.FromJSON Experiment where
  parseJSON =
    Core.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Core.<$> (x Core..:? "ExperimentArn")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ExperimentName")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "DisplayName")
      )

instance Core.Hashable Experiment

instance Core.NFData Experiment
