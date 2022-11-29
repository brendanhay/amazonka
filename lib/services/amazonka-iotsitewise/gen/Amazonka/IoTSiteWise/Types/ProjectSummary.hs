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
-- Module      : Amazonka.IoTSiteWise.Types.ProjectSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ProjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains project summary information.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The date the project was created, in Unix epoch time.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date the project was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Core.POSIX,
    -- | The project\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project.
    id :: Prelude.Text,
    -- | The name of the project.
    name :: Prelude.Text
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
-- 'creationDate', 'projectSummary_creationDate' - The date the project was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'projectSummary_lastUpdateDate' - The date the project was last updated, in Unix epoch time.
--
-- 'description', 'projectSummary_description' - The project\'s description.
--
-- 'id', 'projectSummary_id' - The ID of the project.
--
-- 'name', 'projectSummary_name' - The name of the project.
newProjectSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ProjectSummary
newProjectSummary pId_ pName_ =
  ProjectSummary'
    { creationDate = Prelude.Nothing,
      lastUpdateDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_,
      name = pName_
    }

-- | The date the project was created, in Unix epoch time.
projectSummary_creationDate :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.UTCTime)
projectSummary_creationDate = Lens.lens (\ProjectSummary' {creationDate} -> creationDate) (\s@ProjectSummary' {} a -> s {creationDate = a} :: ProjectSummary) Prelude.. Lens.mapping Core._Time

-- | The date the project was last updated, in Unix epoch time.
projectSummary_lastUpdateDate :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.UTCTime)
projectSummary_lastUpdateDate = Lens.lens (\ProjectSummary' {lastUpdateDate} -> lastUpdateDate) (\s@ProjectSummary' {} a -> s {lastUpdateDate = a} :: ProjectSummary) Prelude.. Lens.mapping Core._Time

-- | The project\'s description.
projectSummary_description :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_description = Lens.lens (\ProjectSummary' {description} -> description) (\s@ProjectSummary' {} a -> s {description = a} :: ProjectSummary)

-- | The ID of the project.
projectSummary_id :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_id = Lens.lens (\ProjectSummary' {id} -> id) (\s@ProjectSummary' {} a -> s {id = a} :: ProjectSummary)

-- | The name of the project.
projectSummary_name :: Lens.Lens' ProjectSummary Prelude.Text
projectSummary_name = Lens.lens (\ProjectSummary' {name} -> name) (\s@ProjectSummary' {} a -> s {name = a} :: ProjectSummary)

instance Core.FromJSON ProjectSummary where
  parseJSON =
    Core.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "lastUpdateDate")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable ProjectSummary where
  hashWithSalt _salt ProjectSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ProjectSummary where
  rnf ProjectSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
