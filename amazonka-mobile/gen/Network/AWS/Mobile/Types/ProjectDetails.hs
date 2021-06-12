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
-- Module      : Network.AWS.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.Resource

-- | Detailed information about an AWS Mobile Hub project.
--
-- /See:/ 'newProjectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { -- | Date the project was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | Date of the last modification of the project.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    projectId :: Core.Maybe Core.Text,
    state :: Core.Maybe ProjectState,
    name :: Core.Maybe Core.Text,
    -- | Website URL for this project in the AWS Mobile Hub console.
    consoleUrl :: Core.Maybe Core.Text,
    resources :: Core.Maybe [Resource],
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProjectDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'projectDetails_createdDate' - Date the project was created.
--
-- 'lastUpdatedDate', 'projectDetails_lastUpdatedDate' - Date of the last modification of the project.
--
-- 'projectId', 'projectDetails_projectId' - Undocumented member.
--
-- 'state', 'projectDetails_state' - Undocumented member.
--
-- 'name', 'projectDetails_name' - Undocumented member.
--
-- 'consoleUrl', 'projectDetails_consoleUrl' - Website URL for this project in the AWS Mobile Hub console.
--
-- 'resources', 'projectDetails_resources' - Undocumented member.
--
-- 'region', 'projectDetails_region' - Undocumented member.
newProjectDetails ::
  ProjectDetails
newProjectDetails =
  ProjectDetails'
    { createdDate = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      projectId = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      consoleUrl = Core.Nothing,
      resources = Core.Nothing,
      region = Core.Nothing
    }

-- | Date the project was created.
projectDetails_createdDate :: Lens.Lens' ProjectDetails (Core.Maybe Core.UTCTime)
projectDetails_createdDate = Lens.lens (\ProjectDetails' {createdDate} -> createdDate) (\s@ProjectDetails' {} a -> s {createdDate = a} :: ProjectDetails) Core.. Lens.mapping Core._Time

-- | Date of the last modification of the project.
projectDetails_lastUpdatedDate :: Lens.Lens' ProjectDetails (Core.Maybe Core.UTCTime)
projectDetails_lastUpdatedDate = Lens.lens (\ProjectDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@ProjectDetails' {} a -> s {lastUpdatedDate = a} :: ProjectDetails) Core.. Lens.mapping Core._Time

-- | Undocumented member.
projectDetails_projectId :: Lens.Lens' ProjectDetails (Core.Maybe Core.Text)
projectDetails_projectId = Lens.lens (\ProjectDetails' {projectId} -> projectId) (\s@ProjectDetails' {} a -> s {projectId = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_state :: Lens.Lens' ProjectDetails (Core.Maybe ProjectState)
projectDetails_state = Lens.lens (\ProjectDetails' {state} -> state) (\s@ProjectDetails' {} a -> s {state = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_name :: Lens.Lens' ProjectDetails (Core.Maybe Core.Text)
projectDetails_name = Lens.lens (\ProjectDetails' {name} -> name) (\s@ProjectDetails' {} a -> s {name = a} :: ProjectDetails)

-- | Website URL for this project in the AWS Mobile Hub console.
projectDetails_consoleUrl :: Lens.Lens' ProjectDetails (Core.Maybe Core.Text)
projectDetails_consoleUrl = Lens.lens (\ProjectDetails' {consoleUrl} -> consoleUrl) (\s@ProjectDetails' {} a -> s {consoleUrl = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_resources :: Lens.Lens' ProjectDetails (Core.Maybe [Resource])
projectDetails_resources = Lens.lens (\ProjectDetails' {resources} -> resources) (\s@ProjectDetails' {} a -> s {resources = a} :: ProjectDetails) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
projectDetails_region :: Lens.Lens' ProjectDetails (Core.Maybe Core.Text)
projectDetails_region = Lens.lens (\ProjectDetails' {region} -> region) (\s@ProjectDetails' {} a -> s {region = a} :: ProjectDetails)

instance Core.FromJSON ProjectDetails where
  parseJSON =
    Core.withObject
      "ProjectDetails"
      ( \x ->
          ProjectDetails'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "projectId")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "consoleUrl")
            Core.<*> (x Core..:? "resources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "region")
      )

instance Core.Hashable ProjectDetails

instance Core.NFData ProjectDetails
