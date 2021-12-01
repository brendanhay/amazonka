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
-- Module      : Amazonka.SageMaker.Types.Experiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Experiment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ExperimentSource
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | The properties of an experiment as returned by the Search API.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | When the experiment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Who created the experiment.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the experiment was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    source :: Prelude.Maybe ExperimentSource,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of tags that are associated with the experiment. You can use
    -- Search API to search on the tags.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Experiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'experiment_creationTime' - When the experiment was created.
--
-- 'createdBy', 'experiment_createdBy' - Who created the experiment.
--
-- 'lastModifiedTime', 'experiment_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentName', 'experiment_experimentName' - The name of the experiment.
--
-- 'experimentArn', 'experiment_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'source', 'experiment_source' - Undocumented member.
--
-- 'displayName', 'experiment_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
--
-- 'lastModifiedBy', 'experiment_lastModifiedBy' - Undocumented member.
--
-- 'description', 'experiment_description' - The description of the experiment.
--
-- 'tags', 'experiment_tags' - The list of tags that are associated with the experiment. You can use
-- Search API to search on the tags.
newExperiment ::
  Experiment
newExperiment =
  Experiment'
    { creationTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      experimentArn = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | When the experiment was created.
experiment_creationTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_creationTime = Lens.lens (\Experiment' {creationTime} -> creationTime) (\s@Experiment' {} a -> s {creationTime = a} :: Experiment) Prelude.. Lens.mapping Core._Time

-- | Who created the experiment.
experiment_createdBy :: Lens.Lens' Experiment (Prelude.Maybe UserContext)
experiment_createdBy = Lens.lens (\Experiment' {createdBy} -> createdBy) (\s@Experiment' {} a -> s {createdBy = a} :: Experiment)

-- | When the experiment was last modified.
experiment_lastModifiedTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_lastModifiedTime = Lens.lens (\Experiment' {lastModifiedTime} -> lastModifiedTime) (\s@Experiment' {} a -> s {lastModifiedTime = a} :: Experiment) Prelude.. Lens.mapping Core._Time

-- | The name of the experiment.
experiment_experimentName :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentName = Lens.lens (\Experiment' {experimentName} -> experimentName) (\s@Experiment' {} a -> s {experimentName = a} :: Experiment)

-- | The Amazon Resource Name (ARN) of the experiment.
experiment_experimentArn :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentArn = Lens.lens (\Experiment' {experimentArn} -> experimentArn) (\s@Experiment' {} a -> s {experimentArn = a} :: Experiment)

-- | Undocumented member.
experiment_source :: Lens.Lens' Experiment (Prelude.Maybe ExperimentSource)
experiment_source = Lens.lens (\Experiment' {source} -> source) (\s@Experiment' {} a -> s {source = a} :: Experiment)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
experiment_displayName :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_displayName = Lens.lens (\Experiment' {displayName} -> displayName) (\s@Experiment' {} a -> s {displayName = a} :: Experiment)

-- | Undocumented member.
experiment_lastModifiedBy :: Lens.Lens' Experiment (Prelude.Maybe UserContext)
experiment_lastModifiedBy = Lens.lens (\Experiment' {lastModifiedBy} -> lastModifiedBy) (\s@Experiment' {} a -> s {lastModifiedBy = a} :: Experiment)

-- | The description of the experiment.
experiment_description :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_description = Lens.lens (\Experiment' {description} -> description) (\s@Experiment' {} a -> s {description = a} :: Experiment)

-- | The list of tags that are associated with the experiment. You can use
-- Search API to search on the tags.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe [Tag])
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Experiment where
  parseJSON =
    Core.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ExperimentName")
            Prelude.<*> (x Core..:? "ExperimentArn")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Experiment where
  hashWithSalt salt' Experiment' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` experimentArn
      `Prelude.hashWithSalt` experimentName
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Experiment where
  rnf Experiment' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf experimentArn
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf createdBy
