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
-- Module      : Amazonka.CodeStar.Types.ProjectSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.ProjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the metadata for a project.
--
-- /See:/ 'newProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project.
    projectId :: Prelude.Maybe Prelude.Text
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
-- 'projectArn', 'projectSummary_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectId', 'projectSummary_projectId' - The ID of the project.
newProjectSummary ::
  ProjectSummary
newProjectSummary =
  ProjectSummary'
    { projectArn = Prelude.Nothing,
      projectId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project.
projectSummary_projectArn :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_projectArn = Lens.lens (\ProjectSummary' {projectArn} -> projectArn) (\s@ProjectSummary' {} a -> s {projectArn = a} :: ProjectSummary)

-- | The ID of the project.
projectSummary_projectId :: Lens.Lens' ProjectSummary (Prelude.Maybe Prelude.Text)
projectSummary_projectId = Lens.lens (\ProjectSummary' {projectId} -> projectId) (\s@ProjectSummary' {} a -> s {projectId = a} :: ProjectSummary)

instance Data.FromJSON ProjectSummary where
  parseJSON =
    Data.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Prelude.<$> (x Data..:? "projectArn")
            Prelude.<*> (x Data..:? "projectId")
      )

instance Prelude.Hashable ProjectSummary where
  hashWithSalt _salt ProjectSummary' {..} =
    _salt
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` projectId

instance Prelude.NFData ProjectSummary where
  rnf ProjectSummary' {..} =
    Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectId
