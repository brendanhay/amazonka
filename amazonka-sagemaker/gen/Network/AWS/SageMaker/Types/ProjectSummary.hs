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
-- Module      : Network.AWS.SageMaker.Types.ProjectSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProjectSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProjectStatus

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
    creationTime :: Prelude.POSIX,
    -- | The status of the project.
    projectStatus :: ProjectStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        creationTime = Prelude._Time Lens.# pCreationTime_,
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
projectSummary_creationTime = Lens.lens (\ProjectSummary' {creationTime} -> creationTime) (\s@ProjectSummary' {} a -> s {creationTime = a} :: ProjectSummary) Prelude.. Prelude._Time

-- | The status of the project.
projectSummary_projectStatus :: Lens.Lens' ProjectSummary ProjectStatus
projectSummary_projectStatus = Lens.lens (\ProjectSummary' {projectStatus} -> projectStatus) (\s@ProjectSummary' {} a -> s {projectStatus = a} :: ProjectSummary)

instance Prelude.FromJSON ProjectSummary where
  parseJSON =
    Prelude.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Prelude..:? "ProjectDescription")
            Prelude.<*> (x Prelude..: "ProjectName")
            Prelude.<*> (x Prelude..: "ProjectArn")
            Prelude.<*> (x Prelude..: "ProjectId")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "ProjectStatus")
      )

instance Prelude.Hashable ProjectSummary

instance Prelude.NFData ProjectSummary
