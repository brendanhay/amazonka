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
-- Module      : Amazonka.IoT1ClickProjects.Types.ProjectSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.ProjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object providing summary information for a particular project for an
-- associated AWS account and region.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The tags (metadata key\/value pairs) associated with the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the project being summarized.
    projectName :: Prelude.Text,
    -- | The date when the project was originally created, in UNIX epoch time
    -- format.
    createdDate :: Core.POSIX,
    -- | The date when the project was last updated, in UNIX epoch time format.
    -- If the project was not updated, then @createdDate@ and @updatedDate@ are
    -- the same.
    updatedDate :: Core.POSIX
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
-- 'tags', 'projectSummary_tags' - The tags (metadata key\/value pairs) associated with the project.
--
-- 'arn', 'projectSummary_arn' - The ARN of the project.
--
-- 'projectName', 'projectSummary_projectName' - The name of the project being summarized.
--
-- 'createdDate', 'projectSummary_createdDate' - The date when the project was originally created, in UNIX epoch time
-- format.
--
-- 'updatedDate', 'projectSummary_updatedDate' - The date when the project was last updated, in UNIX epoch time format.
-- If the project was not updated, then @createdDate@ and @updatedDate@ are
-- the same.
newProjectSummary ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'updatedDate'
  Prelude.UTCTime ->
  ProjectSummary
newProjectSummary
  pProjectName_
  pCreatedDate_
  pUpdatedDate_ =
    ProjectSummary'
      { tags = Prelude.Nothing,
        arn = Prelude.Nothing,
        projectName = pProjectName_,
        createdDate = Core._Time Lens.# pCreatedDate_,
        updatedDate = Core._Time Lens.# pUpdatedDate_
      }

-- | The tags (metadata key\/value pairs) associated with the project.
projectSummary_tags :: Lens.Lens' ProjectSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
projectSummary_tags = Lens.lens (\ProjectSummary' {tags} -> tags) (\s@ProjectSummary' {} a -> s {tags = a} :: ProjectSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the project.
projectSummary_arn :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_arn = Lens.lens (\ProjectSummary' {arn} -> arn) (\s@ProjectSummary' {} a -> s {arn = a} :: ProjectSummary)

-- | The name of the project being summarized.
projectSummary_projectName :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_projectName = Lens.lens (\ProjectSummary' {projectName} -> projectName) (\s@ProjectSummary' {} a -> s {projectName = a} :: ProjectSummary)

-- | The date when the project was originally created, in UNIX epoch time
-- format.
projectSummary_createdDate :: Lens.Lens' ProjectSummary Prelude.UTCTime
projectSummary_createdDate = Lens.lens (\ProjectSummary' {createdDate} -> createdDate) (\s@ProjectSummary' {} a -> s {createdDate = a} :: ProjectSummary) Prelude.. Core._Time

-- | The date when the project was last updated, in UNIX epoch time format.
-- If the project was not updated, then @createdDate@ and @updatedDate@ are
-- the same.
projectSummary_updatedDate :: Lens.Lens' ProjectSummary Prelude.UTCTime
projectSummary_updatedDate = Lens.lens (\ProjectSummary' {updatedDate} -> updatedDate) (\s@ProjectSummary' {} a -> s {updatedDate = a} :: ProjectSummary) Prelude.. Core._Time

instance Core.FromJSON ProjectSummary where
  parseJSON =
    Core.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..: "projectName")
            Prelude.<*> (x Core..: "createdDate")
            Prelude.<*> (x Core..: "updatedDate")
      )

instance Prelude.Hashable ProjectSummary where
  hashWithSalt _salt ProjectSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` updatedDate

instance Prelude.NFData ProjectSummary where
  rnf ProjectSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf updatedDate
