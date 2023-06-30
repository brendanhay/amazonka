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
-- Module      : Amazonka.CodeBuild.Types.ProjectBadge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectBadge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the build badge for the build project.
--
-- /See:/ 'newProjectBadge' smart constructor.
data ProjectBadge = ProjectBadge'
  { -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The publicly-accessible URL through which you can access the build badge
    -- for your project.
    badgeRequestUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectBadge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'badgeEnabled', 'projectBadge_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
--
-- 'badgeRequestUrl', 'projectBadge_badgeRequestUrl' - The publicly-accessible URL through which you can access the build badge
-- for your project.
newProjectBadge ::
  ProjectBadge
newProjectBadge =
  ProjectBadge'
    { badgeEnabled = Prelude.Nothing,
      badgeRequestUrl = Prelude.Nothing
    }

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
projectBadge_badgeEnabled :: Lens.Lens' ProjectBadge (Prelude.Maybe Prelude.Bool)
projectBadge_badgeEnabled = Lens.lens (\ProjectBadge' {badgeEnabled} -> badgeEnabled) (\s@ProjectBadge' {} a -> s {badgeEnabled = a} :: ProjectBadge)

-- | The publicly-accessible URL through which you can access the build badge
-- for your project.
projectBadge_badgeRequestUrl :: Lens.Lens' ProjectBadge (Prelude.Maybe Prelude.Text)
projectBadge_badgeRequestUrl = Lens.lens (\ProjectBadge' {badgeRequestUrl} -> badgeRequestUrl) (\s@ProjectBadge' {} a -> s {badgeRequestUrl = a} :: ProjectBadge)

instance Data.FromJSON ProjectBadge where
  parseJSON =
    Data.withObject
      "ProjectBadge"
      ( \x ->
          ProjectBadge'
            Prelude.<$> (x Data..:? "badgeEnabled")
            Prelude.<*> (x Data..:? "badgeRequestUrl")
      )

instance Prelude.Hashable ProjectBadge where
  hashWithSalt _salt ProjectBadge' {..} =
    _salt
      `Prelude.hashWithSalt` badgeEnabled
      `Prelude.hashWithSalt` badgeRequestUrl

instance Prelude.NFData ProjectBadge where
  rnf ProjectBadge' {..} =
    Prelude.rnf badgeEnabled
      `Prelude.seq` Prelude.rnf badgeRequestUrl
