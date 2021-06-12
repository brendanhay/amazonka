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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProjectStatus

-- | Information about a project.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The description of the project.
    projectDescription :: Core.Maybe Core.Text,
    -- | The name of the project.
    projectName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Core.Text,
    -- | The ID of the project.
    projectId :: Core.Text,
    -- | The time that the project was created.
    creationTime :: Core.POSIX,
    -- | The status of the project.
    projectStatus :: ProjectStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'projectArn'
  Core.Text ->
  -- | 'projectId'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
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
      { projectDescription = Core.Nothing,
        projectName = pProjectName_,
        projectArn = pProjectArn_,
        projectId = pProjectId_,
        creationTime = Core._Time Lens.# pCreationTime_,
        projectStatus = pProjectStatus_
      }

-- | The description of the project.
projectSummary_projectDescription :: Lens.Lens' ProjectSummary (Core.Maybe Core.Text)
projectSummary_projectDescription = Lens.lens (\ProjectSummary' {projectDescription} -> projectDescription) (\s@ProjectSummary' {} a -> s {projectDescription = a} :: ProjectSummary)

-- | The name of the project.
projectSummary_projectName :: Lens.Lens' ProjectSummary Core.Text
projectSummary_projectName = Lens.lens (\ProjectSummary' {projectName} -> projectName) (\s@ProjectSummary' {} a -> s {projectName = a} :: ProjectSummary)

-- | The Amazon Resource Name (ARN) of the project.
projectSummary_projectArn :: Lens.Lens' ProjectSummary Core.Text
projectSummary_projectArn = Lens.lens (\ProjectSummary' {projectArn} -> projectArn) (\s@ProjectSummary' {} a -> s {projectArn = a} :: ProjectSummary)

-- | The ID of the project.
projectSummary_projectId :: Lens.Lens' ProjectSummary Core.Text
projectSummary_projectId = Lens.lens (\ProjectSummary' {projectId} -> projectId) (\s@ProjectSummary' {} a -> s {projectId = a} :: ProjectSummary)

-- | The time that the project was created.
projectSummary_creationTime :: Lens.Lens' ProjectSummary Core.UTCTime
projectSummary_creationTime = Lens.lens (\ProjectSummary' {creationTime} -> creationTime) (\s@ProjectSummary' {} a -> s {creationTime = a} :: ProjectSummary) Core.. Core._Time

-- | The status of the project.
projectSummary_projectStatus :: Lens.Lens' ProjectSummary ProjectStatus
projectSummary_projectStatus = Lens.lens (\ProjectSummary' {projectStatus} -> projectStatus) (\s@ProjectSummary' {} a -> s {projectStatus = a} :: ProjectSummary)

instance Core.FromJSON ProjectSummary where
  parseJSON =
    Core.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Core.<$> (x Core..:? "ProjectDescription")
            Core.<*> (x Core..: "ProjectName")
            Core.<*> (x Core..: "ProjectArn")
            Core.<*> (x Core..: "ProjectId")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ProjectStatus")
      )

instance Core.Hashable ProjectSummary

instance Core.NFData ProjectSummary
