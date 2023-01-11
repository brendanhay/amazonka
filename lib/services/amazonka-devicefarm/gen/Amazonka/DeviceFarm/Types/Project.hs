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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude

-- | Represents an operating-system neutral workspace for running and
-- managing tests.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | The project\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the project was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The default number of minutes (at the project level) a test run executes
    -- before it times out. The default value is 150 minutes.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The project\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe VpcConfig
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
-- 'arn', 'project_arn' - The project\'s ARN.
--
-- 'created', 'project_created' - When the project was created.
--
-- 'defaultJobTimeoutMinutes', 'project_defaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
--
-- 'name', 'project_name' - The project\'s name.
--
-- 'vpcConfig', 'project_vpcConfig' - The VPC security groups and subnets that are attached to a project.
newProject ::
  Project
newProject =
  Project'
    { arn = Prelude.Nothing,
      created = Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing,
      name = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The project\'s ARN.
project_arn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | When the project was created.
project_created :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_created = Lens.lens (\Project' {created} -> created) (\s@Project' {} a -> s {created = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | The default number of minutes (at the project level) a test run executes
-- before it times out. The default value is 150 minutes.
project_defaultJobTimeoutMinutes :: Lens.Lens' Project (Prelude.Maybe Prelude.Int)
project_defaultJobTimeoutMinutes = Lens.lens (\Project' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@Project' {} a -> s {defaultJobTimeoutMinutes = a} :: Project)

-- | The project\'s name.
project_name :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | The VPC security groups and subnets that are attached to a project.
project_vpcConfig :: Lens.Lens' Project (Prelude.Maybe VpcConfig)
project_vpcConfig = Lens.lens (\Project' {vpcConfig} -> vpcConfig) (\s@Project' {} a -> s {vpcConfig = a} :: Project)

instance Data.FromJSON Project where
  parseJSON =
    Data.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "defaultJobTimeoutMinutes")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "vpcConfig")
      )

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` defaultJobTimeoutMinutes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf defaultJobTimeoutMinutes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf vpcConfig
