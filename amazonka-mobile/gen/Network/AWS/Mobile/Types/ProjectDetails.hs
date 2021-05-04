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
-- Module      : Network.AWS.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectDetails where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.Resource
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about an AWS Mobile Hub project.
--
-- /See:/ 'newProjectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { -- | Date the project was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | Date of the last modification of the project.
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    projectId :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ProjectState,
    name :: Prelude.Maybe Prelude.Text,
    -- | Website URL for this project in the AWS Mobile Hub console.
    consoleUrl :: Prelude.Maybe Prelude.Text,
    resources :: Prelude.Maybe [Resource],
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdDate = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      projectId = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      consoleUrl = Prelude.Nothing,
      resources = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | Date the project was created.
projectDetails_createdDate :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.UTCTime)
projectDetails_createdDate = Lens.lens (\ProjectDetails' {createdDate} -> createdDate) (\s@ProjectDetails' {} a -> s {createdDate = a} :: ProjectDetails) Prelude.. Lens.mapping Prelude._Time

-- | Date of the last modification of the project.
projectDetails_lastUpdatedDate :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.UTCTime)
projectDetails_lastUpdatedDate = Lens.lens (\ProjectDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@ProjectDetails' {} a -> s {lastUpdatedDate = a} :: ProjectDetails) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
projectDetails_projectId :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_projectId = Lens.lens (\ProjectDetails' {projectId} -> projectId) (\s@ProjectDetails' {} a -> s {projectId = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_state :: Lens.Lens' ProjectDetails (Prelude.Maybe ProjectState)
projectDetails_state = Lens.lens (\ProjectDetails' {state} -> state) (\s@ProjectDetails' {} a -> s {state = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_name :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_name = Lens.lens (\ProjectDetails' {name} -> name) (\s@ProjectDetails' {} a -> s {name = a} :: ProjectDetails)

-- | Website URL for this project in the AWS Mobile Hub console.
projectDetails_consoleUrl :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_consoleUrl = Lens.lens (\ProjectDetails' {consoleUrl} -> consoleUrl) (\s@ProjectDetails' {} a -> s {consoleUrl = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_resources :: Lens.Lens' ProjectDetails (Prelude.Maybe [Resource])
projectDetails_resources = Lens.lens (\ProjectDetails' {resources} -> resources) (\s@ProjectDetails' {} a -> s {resources = a} :: ProjectDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
projectDetails_region :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_region = Lens.lens (\ProjectDetails' {region} -> region) (\s@ProjectDetails' {} a -> s {region = a} :: ProjectDetails)

instance Prelude.FromJSON ProjectDetails where
  parseJSON =
    Prelude.withObject
      "ProjectDetails"
      ( \x ->
          ProjectDetails'
            Prelude.<$> (x Prelude..:? "createdDate")
            Prelude.<*> (x Prelude..:? "lastUpdatedDate")
            Prelude.<*> (x Prelude..:? "projectId")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "consoleUrl")
            Prelude.<*> ( x Prelude..:? "resources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "region")
      )

instance Prelude.Hashable ProjectDetails

instance Prelude.NFData ProjectDetails
