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
-- Module      : Network.AWS.DeviceFarm.Types.Project
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Project where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an operating-system neutral workspace for running and
-- managing tests.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The project\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The project\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | When the project was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The default number of minutes (at the project level) a test run executes
    -- before it times out. The default value is 150 minutes.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      created = Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing
    }

-- | The project\'s ARN.
project_arn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | The project\'s name.
project_name :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | When the project was created.
project_created :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_created = Lens.lens (\Project' {created} -> created) (\s@Project' {} a -> s {created = a} :: Project) Prelude.. Lens.mapping Prelude._Time

-- | The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
project_defaultJobTimeoutMinutes :: Lens.Lens' Project (Prelude.Maybe Prelude.Int)
project_defaultJobTimeoutMinutes = Lens.lens (\Project' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@Project' {} a -> s {defaultJobTimeoutMinutes = a} :: Project)

instance Prelude.FromJSON Project where
  parseJSON =
    Prelude.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "defaultJobTimeoutMinutes")
      )

instance Prelude.Hashable Project

instance Prelude.NFData Project
