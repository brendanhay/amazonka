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
-- Module      : Amazonka.IoT1ClickProjects.Types.ProjectDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.ProjectDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types.PlacementTemplate
import qualified Amazonka.Prelude as Prelude

-- | An object providing detailed information for a particular project
-- associated with an AWS account and region.
--
-- /See:/ 'newProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { -- | The ARN of the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | An object describing the project\'s placement specifications.
    placementTemplate :: Prelude.Maybe PlacementTemplate,
    -- | The tags (metadata key\/value pairs) associated with the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the project for which to obtain information from.
    projectName :: Prelude.Text,
    -- | The date when the project was originally created, in UNIX epoch time
    -- format.
    createdDate :: Data.POSIX,
    -- | The date when the project was last updated, in UNIX epoch time format.
    -- If the project was not updated, then @createdDate@ and @updatedDate@ are
    -- the same.
    updatedDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'projectDescription_arn' - The ARN of the project.
--
-- 'description', 'projectDescription_description' - The description of the project.
--
-- 'placementTemplate', 'projectDescription_placementTemplate' - An object describing the project\'s placement specifications.
--
-- 'tags', 'projectDescription_tags' - The tags (metadata key\/value pairs) associated with the project.
--
-- 'projectName', 'projectDescription_projectName' - The name of the project for which to obtain information from.
--
-- 'createdDate', 'projectDescription_createdDate' - The date when the project was originally created, in UNIX epoch time
-- format.
--
-- 'updatedDate', 'projectDescription_updatedDate' - The date when the project was last updated, in UNIX epoch time format.
-- If the project was not updated, then @createdDate@ and @updatedDate@ are
-- the same.
newProjectDescription ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'updatedDate'
  Prelude.UTCTime ->
  ProjectDescription
newProjectDescription
  pProjectName_
  pCreatedDate_
  pUpdatedDate_ =
    ProjectDescription'
      { arn = Prelude.Nothing,
        description = Prelude.Nothing,
        placementTemplate = Prelude.Nothing,
        tags = Prelude.Nothing,
        projectName = pProjectName_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        updatedDate = Data._Time Lens.# pUpdatedDate_
      }

-- | The ARN of the project.
projectDescription_arn :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_arn = Lens.lens (\ProjectDescription' {arn} -> arn) (\s@ProjectDescription' {} a -> s {arn = a} :: ProjectDescription)

-- | The description of the project.
projectDescription_description :: Lens.Lens' ProjectDescription (Prelude.Maybe Prelude.Text)
projectDescription_description = Lens.lens (\ProjectDescription' {description} -> description) (\s@ProjectDescription' {} a -> s {description = a} :: ProjectDescription)

-- | An object describing the project\'s placement specifications.
projectDescription_placementTemplate :: Lens.Lens' ProjectDescription (Prelude.Maybe PlacementTemplate)
projectDescription_placementTemplate = Lens.lens (\ProjectDescription' {placementTemplate} -> placementTemplate) (\s@ProjectDescription' {} a -> s {placementTemplate = a} :: ProjectDescription)

-- | The tags (metadata key\/value pairs) associated with the project.
projectDescription_tags :: Lens.Lens' ProjectDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
projectDescription_tags = Lens.lens (\ProjectDescription' {tags} -> tags) (\s@ProjectDescription' {} a -> s {tags = a} :: ProjectDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the project for which to obtain information from.
projectDescription_projectName :: Lens.Lens' ProjectDescription Prelude.Text
projectDescription_projectName = Lens.lens (\ProjectDescription' {projectName} -> projectName) (\s@ProjectDescription' {} a -> s {projectName = a} :: ProjectDescription)

-- | The date when the project was originally created, in UNIX epoch time
-- format.
projectDescription_createdDate :: Lens.Lens' ProjectDescription Prelude.UTCTime
projectDescription_createdDate = Lens.lens (\ProjectDescription' {createdDate} -> createdDate) (\s@ProjectDescription' {} a -> s {createdDate = a} :: ProjectDescription) Prelude.. Data._Time

-- | The date when the project was last updated, in UNIX epoch time format.
-- If the project was not updated, then @createdDate@ and @updatedDate@ are
-- the same.
projectDescription_updatedDate :: Lens.Lens' ProjectDescription Prelude.UTCTime
projectDescription_updatedDate = Lens.lens (\ProjectDescription' {updatedDate} -> updatedDate) (\s@ProjectDescription' {} a -> s {updatedDate = a} :: ProjectDescription) Prelude.. Data._Time

instance Data.FromJSON ProjectDescription where
  parseJSON =
    Data.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "placementTemplate")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "projectName")
            Prelude.<*> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "updatedDate")
      )

instance Prelude.Hashable ProjectDescription where
  hashWithSalt _salt ProjectDescription' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` placementTemplate
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` updatedDate

instance Prelude.NFData ProjectDescription where
  rnf ProjectDescription' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf placementTemplate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf updatedDate
