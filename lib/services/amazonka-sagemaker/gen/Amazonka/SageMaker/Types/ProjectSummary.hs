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
-- Module      : Amazonka.SageMaker.Types.ProjectSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProjectStatus

-- | Information about a project.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The description of the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Text,
    -- | The ID of the project.
    projectId :: Prelude.Text,
    -- | The time that the project was created.
    creationTime :: Data.POSIX,
    -- | The status of the project.
    projectStatus :: ProjectStatus
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
-- 'projectDescription', 'projectSummary_projectDescription' - The description of the project.
--
-- 'projectName', 'projectSummary_projectName' - The name of the project.
--
-- 'projectArn', 'projectSummary_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectId', 'projectSummary_projectId' - The ID of the project.
--
-- 'creationTime', 'projectSummary_creationTime' - The time that the project was created.
--
-- 'projectStatus', 'projectSummary_projectStatus' - The status of the project.
newProjectSummary ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'projectId'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'projectStatus'
  ProjectStatus ->
  ProjectSummary
newProjectSummary
  pProjectName_
  pProjectArn_
  pProjectId_
  pCreationTime_
  pProjectStatus_ =
    ProjectSummary'
      { projectDescription =
          Prelude.Nothing,
        projectName = pProjectName_,
        projectArn = pProjectArn_,
        projectId = pProjectId_,
        creationTime = Data._Time Lens.# pCreationTime_,
        projectStatus = pProjectStatus_
      }

-- | The description of the project.
projectSummary_projectDescription :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_projectDescription = Lens.lens (\ProjectSummary' {projectDescription} -> projectDescription) (\s@ProjectSummary' {} a -> s {projectDescription = a} :: ProjectSummary)

-- | The name of the project.
projectSummary_projectName :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_projectName = Lens.lens (\ProjectSummary' {projectName} -> projectName) (\s@ProjectSummary' {} a -> s {projectName = a} :: ProjectSummary)

-- | The Amazon Resource Name (ARN) of the project.
projectSummary_projectArn :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_projectArn = Lens.lens (\ProjectSummary' {projectArn} -> projectArn) (\s@ProjectSummary' {} a -> s {projectArn = a} :: ProjectSummary)

-- | The ID of the project.
projectSummary_projectId :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_projectId = Lens.lens (\ProjectSummary' {projectId} -> projectId) (\s@ProjectSummary' {} a -> s {projectId = a} :: ProjectSummary)

-- | The time that the project was created.
projectSummary_creationTime :: Lens.Lens' ProjectSummary Prelude.UTCTime
projectSummary_creationTime = Lens.lens (\ProjectSummary' {creationTime} -> creationTime) (\s@ProjectSummary' {} a -> s {creationTime = a} :: ProjectSummary) Prelude.. Data._Time

-- | The status of the project.
projectSummary_projectStatus :: Lens.Lens' ProjectSummary ProjectStatus
projectSummary_projectStatus = Lens.lens (\ProjectSummary' {projectStatus} -> projectStatus) (\s@ProjectSummary' {} a -> s {projectStatus = a} :: ProjectSummary)

instance Data.FromJSON ProjectSummary where
  parseJSON =
    Data.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Data..:? "ProjectDescription")
            Prelude.<*> (x Data..: "ProjectName")
            Prelude.<*> (x Data..: "ProjectArn")
            Prelude.<*> (x Data..: "ProjectId")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "ProjectStatus")
      )

instance Prelude.Hashable ProjectSummary where
  hashWithSalt _salt ProjectSummary' {..} =
    _salt
      `Prelude.hashWithSalt` projectDescription
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` projectId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` projectStatus

instance Prelude.NFData ProjectSummary where
  rnf ProjectSummary' {..} =
    Prelude.rnf projectDescription `Prelude.seq`
      Prelude.rnf projectName `Prelude.seq`
        Prelude.rnf projectArn `Prelude.seq`
          Prelude.rnf projectId `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf projectStatus
