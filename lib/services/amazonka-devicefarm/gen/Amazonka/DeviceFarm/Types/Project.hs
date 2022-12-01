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
-- Module      : Amazonka.DeviceFarm.Types.Project
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude

-- | Represents an operating-system neutral workspace for running and
-- managing tests.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The project\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | When the project was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The project\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The default number of minutes (at the project level) a test run executes
    -- before it times out. The default value is 150 minutes.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'project_name' - The project\'s name.
--
-- 'vpcConfig', 'project_vpcConfig' - The VPC security groups and subnets that are attached to a project.
--
-- 'created', 'project_created' - When the project was created.
--
-- 'arn', 'project_arn' - The project\'s ARN.
--
-- 'defaultJobTimeoutMinutes', 'project_defaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
newProject ::
  Project
newProject =
  Project'
    { name = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      created = Prelude.Nothing,
      arn = Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing
    }

-- | The project\'s name.
project_name :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | The VPC security groups and subnets that are attached to a project.
project_vpcConfig :: Lens.Lens' Project (Prelude.Maybe VpcConfig)
project_vpcConfig = Lens.lens (\Project' {vpcConfig} -> vpcConfig) (\s@Project' {} a -> s {vpcConfig = a} :: Project)

-- | When the project was created.
project_created :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_created = Lens.lens (\Project' {created} -> created) (\s@Project' {} a -> s {created = a} :: Project) Prelude.. Lens.mapping Core._Time

-- | The project\'s ARN.
project_arn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
project_defaultJobTimeoutMinutes :: Lens.Lens' Project (Prelude.Maybe Prelude.Int)
project_defaultJobTimeoutMinutes = Lens.lens (\Project' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@Project' {} a -> s {defaultJobTimeoutMinutes = a} :: Project)

instance Core.FromJSON Project where
  parseJSON =
    Core.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "vpcConfig")
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "defaultJobTimeoutMinutes")
      )

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` defaultJobTimeoutMinutes

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultJobTimeoutMinutes
