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
-- Module      : Amazonka.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types.ProjectDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types.ProjectState
import Amazonka.Mobile.Types.Resource
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about an AWS Mobile Hub project.
--
-- /See:/ 'newProjectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { -- | Website URL for this project in the AWS Mobile Hub console.
    consoleUrl :: Prelude.Maybe Prelude.Text,
    -- | Date the project was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | Date of the last modification of the project.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    name :: Prelude.Maybe Prelude.Text,
    projectId :: Prelude.Maybe Prelude.Text,
    region :: Prelude.Maybe Prelude.Text,
    resources :: Prelude.Maybe [Resource],
    state :: Prelude.Maybe ProjectState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consoleUrl', 'projectDetails_consoleUrl' - Website URL for this project in the AWS Mobile Hub console.
--
-- 'createdDate', 'projectDetails_createdDate' - Date the project was created.
--
-- 'lastUpdatedDate', 'projectDetails_lastUpdatedDate' - Date of the last modification of the project.
--
-- 'name', 'projectDetails_name' - Undocumented member.
--
-- 'projectId', 'projectDetails_projectId' - Undocumented member.
--
-- 'region', 'projectDetails_region' - Undocumented member.
--
-- 'resources', 'projectDetails_resources' - Undocumented member.
--
-- 'state', 'projectDetails_state' - Undocumented member.
newProjectDetails ::
  ProjectDetails
newProjectDetails =
  ProjectDetails'
    { consoleUrl = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      projectId = Prelude.Nothing,
      region = Prelude.Nothing,
      resources = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Website URL for this project in the AWS Mobile Hub console.
projectDetails_consoleUrl :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_consoleUrl = Lens.lens (\ProjectDetails' {consoleUrl} -> consoleUrl) (\s@ProjectDetails' {} a -> s {consoleUrl = a} :: ProjectDetails)

-- | Date the project was created.
projectDetails_createdDate :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.UTCTime)
projectDetails_createdDate = Lens.lens (\ProjectDetails' {createdDate} -> createdDate) (\s@ProjectDetails' {} a -> s {createdDate = a} :: ProjectDetails) Prelude.. Lens.mapping Data._Time

-- | Date of the last modification of the project.
projectDetails_lastUpdatedDate :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.UTCTime)
projectDetails_lastUpdatedDate = Lens.lens (\ProjectDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@ProjectDetails' {} a -> s {lastUpdatedDate = a} :: ProjectDetails) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
projectDetails_name :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_name = Lens.lens (\ProjectDetails' {name} -> name) (\s@ProjectDetails' {} a -> s {name = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_projectId :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_projectId = Lens.lens (\ProjectDetails' {projectId} -> projectId) (\s@ProjectDetails' {} a -> s {projectId = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_region :: Lens.Lens' ProjectDetails (Prelude.Maybe Prelude.Text)
projectDetails_region = Lens.lens (\ProjectDetails' {region} -> region) (\s@ProjectDetails' {} a -> s {region = a} :: ProjectDetails)

-- | Undocumented member.
projectDetails_resources :: Lens.Lens' ProjectDetails (Prelude.Maybe [Resource])
projectDetails_resources = Lens.lens (\ProjectDetails' {resources} -> resources) (\s@ProjectDetails' {} a -> s {resources = a} :: ProjectDetails) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
projectDetails_state :: Lens.Lens' ProjectDetails (Prelude.Maybe ProjectState)
projectDetails_state = Lens.lens (\ProjectDetails' {state} -> state) (\s@ProjectDetails' {} a -> s {state = a} :: ProjectDetails)

instance Data.FromJSON ProjectDetails where
  parseJSON =
    Data.withObject
      "ProjectDetails"
      ( \x ->
          ProjectDetails'
            Prelude.<$> (x Data..:? "consoleUrl")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "lastUpdatedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "projectId")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable ProjectDetails where
  hashWithSalt _salt ProjectDetails' {..} =
    _salt `Prelude.hashWithSalt` consoleUrl
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` projectId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` state

instance Prelude.NFData ProjectDetails where
  rnf ProjectDetails' {..} =
    Prelude.rnf consoleUrl
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf projectId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf state
