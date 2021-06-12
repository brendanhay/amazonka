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
-- Module      : Network.AWS.DeviceFarm.Types.Project
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Project where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an operating-system neutral workspace for running and
-- managing tests.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The project\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The project\'s name.
    name :: Core.Maybe Core.Text,
    -- | When the project was created.
    created :: Core.Maybe Core.POSIX,
    -- | The default number of minutes (at the project level) a test run executes
    -- before it times out. The default value is 150 minutes.
    defaultJobTimeoutMinutes :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'project_arn' - The project\'s ARN.
--
-- 'name', 'project_name' - The project\'s name.
--
-- 'created', 'project_created' - When the project was created.
--
-- 'defaultJobTimeoutMinutes', 'project_defaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
newProject ::
  Project
newProject =
  Project'
    { arn = Core.Nothing,
      name = Core.Nothing,
      created = Core.Nothing,
      defaultJobTimeoutMinutes = Core.Nothing
    }

-- | The project\'s ARN.
project_arn :: Lens.Lens' Project (Core.Maybe Core.Text)
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | The project\'s name.
project_name :: Lens.Lens' Project (Core.Maybe Core.Text)
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | When the project was created.
project_created :: Lens.Lens' Project (Core.Maybe Core.UTCTime)
project_created = Lens.lens (\Project' {created} -> created) (\s@Project' {} a -> s {created = a} :: Project) Core.. Lens.mapping Core._Time

-- | The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
project_defaultJobTimeoutMinutes :: Lens.Lens' Project (Core.Maybe Core.Int)
project_defaultJobTimeoutMinutes = Lens.lens (\Project' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@Project' {} a -> s {defaultJobTimeoutMinutes = a} :: Project)

instance Core.FromJSON Project where
  parseJSON =
    Core.withObject
      "Project"
      ( \x ->
          Project'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "defaultJobTimeoutMinutes")
      )

instance Core.Hashable Project

instance Core.NFData Project
