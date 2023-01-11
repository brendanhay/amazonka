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
-- Module      : Amazonka.DeviceFarm.Types.TestGridProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridProject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.TestGridVpcConfig
import qualified Amazonka.Prelude as Prelude

-- | A Selenium testing project. Projects are used to collect and collate
-- sessions.
--
-- /See:/ 'newTestGridProject' smart constructor.
data TestGridProject = TestGridProject'
  { -- | The ARN for the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the project was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | A human-readable description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | A human-readable name for the project.
    name :: Prelude.Maybe Prelude.Text,
    -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe TestGridVpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'testGridProject_arn' - The ARN for the project.
--
-- 'created', 'testGridProject_created' - When the project was created.
--
-- 'description', 'testGridProject_description' - A human-readable description for the project.
--
-- 'name', 'testGridProject_name' - A human-readable name for the project.
--
-- 'vpcConfig', 'testGridProject_vpcConfig' - The VPC security groups and subnets that are attached to a project.
newTestGridProject ::
  TestGridProject
newTestGridProject =
  TestGridProject'
    { arn = Prelude.Nothing,
      created = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The ARN for the project.
testGridProject_arn :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_arn = Lens.lens (\TestGridProject' {arn} -> arn) (\s@TestGridProject' {} a -> s {arn = a} :: TestGridProject)

-- | When the project was created.
testGridProject_created :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.UTCTime)
testGridProject_created = Lens.lens (\TestGridProject' {created} -> created) (\s@TestGridProject' {} a -> s {created = a} :: TestGridProject) Prelude.. Lens.mapping Data._Time

-- | A human-readable description for the project.
testGridProject_description :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_description = Lens.lens (\TestGridProject' {description} -> description) (\s@TestGridProject' {} a -> s {description = a} :: TestGridProject)

-- | A human-readable name for the project.
testGridProject_name :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_name = Lens.lens (\TestGridProject' {name} -> name) (\s@TestGridProject' {} a -> s {name = a} :: TestGridProject)

-- | The VPC security groups and subnets that are attached to a project.
testGridProject_vpcConfig :: Lens.Lens' TestGridProject (Prelude.Maybe TestGridVpcConfig)
testGridProject_vpcConfig = Lens.lens (\TestGridProject' {vpcConfig} -> vpcConfig) (\s@TestGridProject' {} a -> s {vpcConfig = a} :: TestGridProject)

instance Data.FromJSON TestGridProject where
  parseJSON =
    Data.withObject
      "TestGridProject"
      ( \x ->
          TestGridProject'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "vpcConfig")
      )

instance Prelude.Hashable TestGridProject where
  hashWithSalt _salt TestGridProject' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData TestGridProject where
  rnf TestGridProject' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf vpcConfig
