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
-- Module      : Amazonka.Evidently.Types.ProjectSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ProjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ProjectStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains configuration information about an Evidently
-- project.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The number of experiments currently in the project.
    activeExperimentCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of ongoing launches currently in the project.
    activeLaunchCount :: Prelude.Maybe Prelude.Integer,
    -- | The description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of experiments currently in the project.
    experimentCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of features currently in the project.
    featureCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of launches currently in the project, including launches that
    -- are ongoing, completed, and not started yet.
    launchCount :: Prelude.Maybe Prelude.Integer,
    -- | The list of tag keys and values associated with this project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name or ARN of the project.
    arn :: Prelude.Text,
    -- | The date and time that the project is created.
    createdTime :: Data.POSIX,
    -- | The date and time that the project was most recently updated.
    lastUpdatedTime :: Data.POSIX,
    -- | The name of the project.
    name :: Prelude.Text,
    -- | The current state of the project.
    status :: ProjectStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeExperimentCount', 'projectSummary_activeExperimentCount' - The number of experiments currently in the project.
--
-- 'activeLaunchCount', 'projectSummary_activeLaunchCount' - The number of ongoing launches currently in the project.
--
-- 'description', 'projectSummary_description' - The description of the project.
--
-- 'experimentCount', 'projectSummary_experimentCount' - The number of experiments currently in the project.
--
-- 'featureCount', 'projectSummary_featureCount' - The number of features currently in the project.
--
-- 'launchCount', 'projectSummary_launchCount' - The number of launches currently in the project, including launches that
-- are ongoing, completed, and not started yet.
--
-- 'tags', 'projectSummary_tags' - The list of tag keys and values associated with this project.
--
-- 'arn', 'projectSummary_arn' - The name or ARN of the project.
--
-- 'createdTime', 'projectSummary_createdTime' - The date and time that the project is created.
--
-- 'lastUpdatedTime', 'projectSummary_lastUpdatedTime' - The date and time that the project was most recently updated.
--
-- 'name', 'projectSummary_name' - The name of the project.
--
-- 'status', 'projectSummary_status' - The current state of the project.
newProjectSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ProjectStatus ->
  ProjectSummary
newProjectSummary
  pArn_
  pCreatedTime_
  pLastUpdatedTime_
  pName_
  pStatus_ =
    ProjectSummary'
      { activeExperimentCount =
          Prelude.Nothing,
        activeLaunchCount = Prelude.Nothing,
        description = Prelude.Nothing,
        experimentCount = Prelude.Nothing,
        featureCount = Prelude.Nothing,
        launchCount = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        status = pStatus_
      }

-- | The number of experiments currently in the project.
projectSummary_activeExperimentCount :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Integer)
projectSummary_activeExperimentCount = Lens.lens (\ProjectSummary' {activeExperimentCount} -> activeExperimentCount) (\s@ProjectSummary' {} a -> s {activeExperimentCount = a} :: ProjectSummary)

-- | The number of ongoing launches currently in the project.
projectSummary_activeLaunchCount :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Integer)
projectSummary_activeLaunchCount = Lens.lens (\ProjectSummary' {activeLaunchCount} -> activeLaunchCount) (\s@ProjectSummary' {} a -> s {activeLaunchCount = a} :: ProjectSummary)

-- | The description of the project.
projectSummary_description :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_description = Lens.lens (\ProjectSummary' {description} -> description) (\s@ProjectSummary' {} a -> s {description = a} :: ProjectSummary)

-- | The number of experiments currently in the project.
projectSummary_experimentCount :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Integer)
projectSummary_experimentCount = Lens.lens (\ProjectSummary' {experimentCount} -> experimentCount) (\s@ProjectSummary' {} a -> s {experimentCount = a} :: ProjectSummary)

-- | The number of features currently in the project.
projectSummary_featureCount :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Integer)
projectSummary_featureCount = Lens.lens (\ProjectSummary' {featureCount} -> featureCount) (\s@ProjectSummary' {} a -> s {featureCount = a} :: ProjectSummary)

-- | The number of launches currently in the project, including launches that
-- are ongoing, completed, and not started yet.
projectSummary_launchCount :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Integer)
projectSummary_launchCount = Lens.lens (\ProjectSummary' {launchCount} -> launchCount) (\s@ProjectSummary' {} a -> s {launchCount = a} :: ProjectSummary)

-- | The list of tag keys and values associated with this project.
projectSummary_tags :: Lens.Lens' ProjectSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
projectSummary_tags = Lens.lens (\ProjectSummary' {tags} -> tags) (\s@ProjectSummary' {} a -> s {tags = a} :: ProjectSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the project.
projectSummary_arn :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_arn = Lens.lens (\ProjectSummary' {arn} -> arn) (\s@ProjectSummary' {} a -> s {arn = a} :: ProjectSummary)

-- | The date and time that the project is created.
projectSummary_createdTime :: Lens.Lens' ProjectSummary Prelude.UTCTime
projectSummary_createdTime = Lens.lens (\ProjectSummary' {createdTime} -> createdTime) (\s@ProjectSummary' {} a -> s {createdTime = a} :: ProjectSummary) Prelude.. Data._Time

-- | The date and time that the project was most recently updated.
projectSummary_lastUpdatedTime :: Lens.Lens' ProjectSummary Prelude.UTCTime
projectSummary_lastUpdatedTime = Lens.lens (\ProjectSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ProjectSummary' {} a -> s {lastUpdatedTime = a} :: ProjectSummary) Prelude.. Data._Time

-- | The name of the project.
projectSummary_name :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_name = Lens.lens (\ProjectSummary' {name} -> name) (\s@ProjectSummary' {} a -> s {name = a} :: ProjectSummary)

-- | The current state of the project.
projectSummary_status :: Lens.Lens' ProjectSummary ProjectStatus
projectSummary_status = Lens.lens (\ProjectSummary' {status} -> status) (\s@ProjectSummary' {} a -> s {status = a} :: ProjectSummary)

instance Data.FromJSON ProjectSummary where
  parseJSON =
    Data.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Data..:? "activeExperimentCount")
            Prelude.<*> (x Data..:? "activeLaunchCount")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "experimentCount")
            Prelude.<*> (x Data..:? "featureCount")
            Prelude.<*> (x Data..:? "launchCount")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "lastUpdatedTime")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ProjectSummary where
  hashWithSalt _salt ProjectSummary' {..} =
    _salt `Prelude.hashWithSalt` activeExperimentCount
      `Prelude.hashWithSalt` activeLaunchCount
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` experimentCount
      `Prelude.hashWithSalt` featureCount
      `Prelude.hashWithSalt` launchCount
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData ProjectSummary where
  rnf ProjectSummary' {..} =
    Prelude.rnf activeExperimentCount
      `Prelude.seq` Prelude.rnf activeLaunchCount
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf experimentCount
      `Prelude.seq` Prelude.rnf featureCount
      `Prelude.seq` Prelude.rnf launchCount
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
