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
-- Module      : Network.AWS.CodeBuild.Types.ProjectBadge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectBadge where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the build badge for the build project.
--
-- /See:/ 'newProjectBadge' smart constructor.
data ProjectBadge = ProjectBadge'
  { -- | The publicly-accessible URL through which you can access the build badge
    -- for your project.
    badgeRequestUrl :: Core.Maybe Core.Text,
    -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProjectBadge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'badgeRequestUrl', 'projectBadge_badgeRequestUrl' - The publicly-accessible URL through which you can access the build badge
-- for your project.
--
-- 'badgeEnabled', 'projectBadge_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
newProjectBadge ::
  ProjectBadge
newProjectBadge =
  ProjectBadge'
    { badgeRequestUrl = Core.Nothing,
      badgeEnabled = Core.Nothing
    }

-- | The publicly-accessible URL through which you can access the build badge
-- for your project.
projectBadge_badgeRequestUrl :: Lens.Lens' ProjectBadge (Core.Maybe Core.Text)
projectBadge_badgeRequestUrl = Lens.lens (\ProjectBadge' {badgeRequestUrl} -> badgeRequestUrl) (\s@ProjectBadge' {} a -> s {badgeRequestUrl = a} :: ProjectBadge)

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
projectBadge_badgeEnabled :: Lens.Lens' ProjectBadge (Core.Maybe Core.Bool)
projectBadge_badgeEnabled = Lens.lens (\ProjectBadge' {badgeEnabled} -> badgeEnabled) (\s@ProjectBadge' {} a -> s {badgeEnabled = a} :: ProjectBadge)

instance Core.FromJSON ProjectBadge where
  parseJSON =
    Core.withObject
      "ProjectBadge"
      ( \x ->
          ProjectBadge'
            Core.<$> (x Core..:? "badgeRequestUrl")
            Core.<*> (x Core..:? "badgeEnabled")
      )

instance Core.Hashable ProjectBadge

instance Core.NFData ProjectBadge
