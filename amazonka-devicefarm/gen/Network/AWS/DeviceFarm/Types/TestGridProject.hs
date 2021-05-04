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
-- Module      : Network.AWS.DeviceFarm.Types.TestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridProject where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A Selenium testing project. Projects are used to collect and collate
-- sessions.
--
-- /See:/ 'newTestGridProject' smart constructor.
data TestGridProject = TestGridProject'
  { -- | The ARN for the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A human-readable name for the project.
    name :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the project was created.
    created :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'name', 'testGridProject_name' - A human-readable name for the project.
--
-- 'description', 'testGridProject_description' - A human-readable description for the project.
--
-- 'created', 'testGridProject_created' - When the project was created.
newTestGridProject ::
  TestGridProject
newTestGridProject =
  TestGridProject'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      created = Prelude.Nothing
    }

-- | The ARN for the project.
testGridProject_arn :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_arn = Lens.lens (\TestGridProject' {arn} -> arn) (\s@TestGridProject' {} a -> s {arn = a} :: TestGridProject)

-- | A human-readable name for the project.
testGridProject_name :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_name = Lens.lens (\TestGridProject' {name} -> name) (\s@TestGridProject' {} a -> s {name = a} :: TestGridProject)

-- | A human-readable description for the project.
testGridProject_description :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.Text)
testGridProject_description = Lens.lens (\TestGridProject' {description} -> description) (\s@TestGridProject' {} a -> s {description = a} :: TestGridProject)

-- | When the project was created.
testGridProject_created :: Lens.Lens' TestGridProject (Prelude.Maybe Prelude.UTCTime)
testGridProject_created = Lens.lens (\TestGridProject' {created} -> created) (\s@TestGridProject' {} a -> s {created = a} :: TestGridProject) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TestGridProject where
  parseJSON =
    Prelude.withObject
      "TestGridProject"
      ( \x ->
          TestGridProject'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "created")
      )

instance Prelude.Hashable TestGridProject

instance Prelude.NFData TestGridProject
