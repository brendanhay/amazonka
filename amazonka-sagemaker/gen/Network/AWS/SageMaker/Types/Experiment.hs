{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of an experiment as returned by the Search API.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | When the experiment was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    source :: Prelude.Maybe ExperimentSource,
    -- | The list of tags that are associated with the experiment. You can use
    -- Search API to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | When the experiment was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { experimentArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      source = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      description = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the experiment.
experiment_experimentArn :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentArn = Lens.lens (\Experiment' {experimentArn} -> experimentArn) (\s@Experiment' {} a -> s {experimentArn = a} :: Experiment)

-- | When the experiment was created.
experiment_creationTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_creationTime = Lens.lens (\Experiment' {creationTime} -> creationTime) (\s@Experiment' {} a -> s {creationTime = a} :: Experiment) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
experiment_source :: Lens.Lens' Experiment (Prelude.Maybe ExperimentSource)
experiment_source = Lens.lens (\Experiment' {source} -> source) (\s@Experiment' {} a -> s {source = a} :: Experiment)

-- | The list of tags that are associated with the experiment. You can use
-- Search API to search on the tags.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe [Tag])
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Prelude._Coerce

-- | When the experiment was last modified.
experiment_lastModifiedTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_lastModifiedTime = Lens.lens (\Experiment' {lastModifiedTime} -> lastModifiedTime) (\s@Experiment' {} a -> s {lastModifiedTime = a} :: Experiment) Prelude.. Lens.mapping Prelude._Time

-- | The name of the experiment.
experiment_experimentName :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentName = Lens.lens (\Experiment' {experimentName} -> experimentName) (\s@Experiment' {} a -> s {experimentName = a} :: Experiment)

-- | The description of the experiment.
experiment_description :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_description = Lens.lens (\Experiment' {description} -> description) (\s@Experiment' {} a -> s {description = a} :: Experiment)

-- | Undocumented member.
experiment_createdBy :: Lens.Lens' Experiment (Prelude.Maybe UserContext)
experiment_createdBy = Lens.lens (\Experiment' {createdBy} -> createdBy) (\s@Experiment' {} a -> s {createdBy = a} :: Experiment)

-- | Undocumented member.
experiment_lastModifiedBy :: Lens.Lens' Experiment (Prelude.Maybe UserContext)
experiment_lastModifiedBy = Lens.lens (\Experiment' {lastModifiedBy} -> lastModifiedBy) (\s@Experiment' {} a -> s {lastModifiedBy = a} :: Experiment)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
experiment_displayName :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_displayName = Lens.lens (\Experiment' {displayName} -> displayName) (\s@Experiment' {} a -> s {displayName = a} :: Experiment)

instance Prelude.FromJSON Experiment where
  parseJSON =
    Prelude.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Prelude..:? "ExperimentArn")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "ExperimentName")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..:? "DisplayName")
      )

instance Prelude.Hashable Experiment

instance Prelude.NFData Experiment
