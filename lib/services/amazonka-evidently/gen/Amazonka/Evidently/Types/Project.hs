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
-- Module      : Amazonka.Evidently.Types.Project
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ProjectAppConfigResource
import Amazonka.Evidently.Types.ProjectDataDelivery
import Amazonka.Evidently.Types.ProjectStatus
import qualified Amazonka.Prelude as Prelude

-- | This structure defines a project, which is the logical object in
-- Evidently that can contain features, launches, and experiments. Use
-- projects to group similar features together.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The number of ongoing experiments currently in the project.
    activeExperimentCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of ongoing launches currently in the project.
    activeLaunchCount :: Prelude.Maybe Prelude.Integer,
    -- | This structure defines the configuration of how your application
    -- integrates with AppConfig to run client-side evaluation.
    appConfigResource :: Prelude.Maybe ProjectAppConfigResource,
    -- | A structure that contains information about where Evidently is to store
    -- evaluation events for longer term storage.
    dataDelivery :: Prelude.Maybe ProjectDataDelivery,
    -- | The user-entered description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of experiments currently in the project. This includes all
    -- experiments that have been created and not deleted, whether they are
    -- ongoing or not.
    experimentCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of features currently in the project.
    featureCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of launches currently in the project. This includes all
    -- launches that have been created and not deleted, whether they are
    -- ongoing or not.
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
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeExperimentCount', 'project_activeExperimentCount' - The number of ongoing experiments currently in the project.
--
-- 'activeLaunchCount', 'project_activeLaunchCount' - The number of ongoing launches currently in the project.
--
-- 'appConfigResource', 'project_appConfigResource' - This structure defines the configuration of how your application
-- integrates with AppConfig to run client-side evaluation.
--
-- 'dataDelivery', 'project_dataDelivery' - A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage.
--
-- 'description', 'project_description' - The user-entered description of the project.
--
-- 'experimentCount', 'project_experimentCount' - The number of experiments currently in the project. This includes all
-- experiments that have been created and not deleted, whether they are
-- ongoing or not.
--
-- 'featureCount', 'project_featureCount' - The number of features currently in the project.
--
-- 'launchCount', 'project_launchCount' - The number of launches currently in the project. This includes all
-- launches that have been created and not deleted, whether they are
-- ongoing or not.
--
-- 'tags', 'project_tags' - The list of tag keys and values associated with this project.
--
-- 'arn', 'project_arn' - The name or ARN of the project.
--
-- 'createdTime', 'project_createdTime' - The date and time that the project is created.
--
-- 'lastUpdatedTime', 'project_lastUpdatedTime' - The date and time that the project was most recently updated.
--
-- 'name', 'project_name' - The name of the project.
--
-- 'status', 'project_status' - The current state of the project.
newProject ::
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
  Project
newProject
  pArn_
  pCreatedTime_
  pLastUpdatedTime_
  pName_
  pStatus_ =
    Project'
      { activeExperimentCount = Prelude.Nothing,
        activeLaunchCount = Prelude.Nothing,
        appConfigResource = Prelude.Nothing,
        dataDelivery = Prelude.Nothing,
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

-- | The number of ongoing experiments currently in the project.
project_activeExperimentCount :: Lens.Lens' Project (Prelude.Maybe Prelude.Integer)
project_activeExperimentCount = Lens.lens (\Project' {activeExperimentCount} -> activeExperimentCount) (\s@Project' {} a -> s {activeExperimentCount = a} :: Project)

-- | The number of ongoing launches currently in the project.
project_activeLaunchCount :: Lens.Lens' Project (Prelude.Maybe Prelude.Integer)
project_activeLaunchCount = Lens.lens (\Project' {activeLaunchCount} -> activeLaunchCount) (\s@Project' {} a -> s {activeLaunchCount = a} :: Project)

-- | This structure defines the configuration of how your application
-- integrates with AppConfig to run client-side evaluation.
project_appConfigResource :: Lens.Lens' Project (Prelude.Maybe ProjectAppConfigResource)
project_appConfigResource = Lens.lens (\Project' {appConfigResource} -> appConfigResource) (\s@Project' {} a -> s {appConfigResource = a} :: Project)

-- | A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage.
project_dataDelivery :: Lens.Lens' Project (Prelude.Maybe ProjectDataDelivery)
project_dataDelivery = Lens.lens (\Project' {dataDelivery} -> dataDelivery) (\s@Project' {} a -> s {dataDelivery = a} :: Project)

-- | The user-entered description of the project.
project_description :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_description = Lens.lens (\Project' {description} -> description) (\s@Project' {} a -> s {description = a} :: Project)

-- | The number of experiments currently in the project. This includes all
-- experiments that have been created and not deleted, whether they are
-- ongoing or not.
project_experimentCount :: Lens.Lens' Project (Prelude.Maybe Prelude.Integer)
project_experimentCount = Lens.lens (\Project' {experimentCount} -> experimentCount) (\s@Project' {} a -> s {experimentCount = a} :: Project)

-- | The number of features currently in the project.
project_featureCount :: Lens.Lens' Project (Prelude.Maybe Prelude.Integer)
project_featureCount = Lens.lens (\Project' {featureCount} -> featureCount) (\s@Project' {} a -> s {featureCount = a} :: Project)

-- | The number of launches currently in the project. This includes all
-- launches that have been created and not deleted, whether they are
-- ongoing or not.
project_launchCount :: Lens.Lens' Project (Prelude.Maybe Prelude.Integer)
project_launchCount = Lens.lens (\Project' {launchCount} -> launchCount) (\s@Project' {} a -> s {launchCount = a} :: Project)

-- | The list of tag keys and values associated with this project.
project_tags :: Lens.Lens' Project (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
project_tags = Lens.lens (\Project' {tags} -> tags) (\s@Project' {} a -> s {tags = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the project.
project_arn :: Lens.Lens' Project Prelude.Text
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | The date and time that the project is created.
project_createdTime :: Lens.Lens' Project Prelude.UTCTime
project_createdTime = Lens.lens (\Project' {createdTime} -> createdTime) (\s@Project' {} a -> s {createdTime = a} :: Project) Prelude.. Data._Time

-- | The date and time that the project was most recently updated.
project_lastUpdatedTime :: Lens.Lens' Project Prelude.UTCTime
project_lastUpdatedTime = Lens.lens (\Project' {lastUpdatedTime} -> lastUpdatedTime) (\s@Project' {} a -> s {lastUpdatedTime = a} :: Project) Prelude.. Data._Time

-- | The name of the project.
project_name :: Lens.Lens' Project Prelude.Text
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | The current state of the project.
project_status :: Lens.Lens' Project ProjectStatus
project_status = Lens.lens (\Project' {status} -> status) (\s@Project' {} a -> s {status = a} :: Project)

instance Data.FromJSON Project where
  parseJSON =
    Data.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Data..:? "activeExperimentCount")
            Prelude.<*> (x Data..:? "activeLaunchCount")
            Prelude.<*> (x Data..:? "appConfigResource")
            Prelude.<*> (x Data..:? "dataDelivery")
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

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt `Prelude.hashWithSalt` activeExperimentCount
      `Prelude.hashWithSalt` activeLaunchCount
      `Prelude.hashWithSalt` appConfigResource
      `Prelude.hashWithSalt` dataDelivery
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

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf activeExperimentCount
      `Prelude.seq` Prelude.rnf activeLaunchCount
      `Prelude.seq` Prelude.rnf appConfigResource
      `Prelude.seq` Prelude.rnf dataDelivery
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
