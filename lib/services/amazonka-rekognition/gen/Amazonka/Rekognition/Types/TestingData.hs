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
-- Module      : Amazonka.Rekognition.Types.TestingData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TestingData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Asset

-- | The dataset used for testing. Optionally, if @AutoCreate@ is set, Amazon
-- Rekognition Custom Labels uses the training dataset to create a test
-- dataset with a temporary split of the training dataset.
--
-- /See:/ 'newTestingData' smart constructor.
data TestingData = TestingData'
  { -- | The assets used for testing.
    assets :: Prelude.Maybe [Asset],
    -- | If specified, Amazon Rekognition Custom Labels temporarily splits the
    -- training dataset (80%) to create a test dataset (20%) for the training
    -- job. After training completes, the test dataset is not stored and the
    -- training dataset reverts to its previous size.
    autoCreate :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestingData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'testingData_assets' - The assets used for testing.
--
-- 'autoCreate', 'testingData_autoCreate' - If specified, Amazon Rekognition Custom Labels temporarily splits the
-- training dataset (80%) to create a test dataset (20%) for the training
-- job. After training completes, the test dataset is not stored and the
-- training dataset reverts to its previous size.
newTestingData ::
  TestingData
newTestingData =
  TestingData'
    { assets = Prelude.Nothing,
      autoCreate = Prelude.Nothing
    }

-- | The assets used for testing.
testingData_assets :: Lens.Lens' TestingData (Prelude.Maybe [Asset])
testingData_assets = Lens.lens (\TestingData' {assets} -> assets) (\s@TestingData' {} a -> s {assets = a} :: TestingData) Prelude.. Lens.mapping Lens.coerced

-- | If specified, Amazon Rekognition Custom Labels temporarily splits the
-- training dataset (80%) to create a test dataset (20%) for the training
-- job. After training completes, the test dataset is not stored and the
-- training dataset reverts to its previous size.
testingData_autoCreate :: Lens.Lens' TestingData (Prelude.Maybe Prelude.Bool)
testingData_autoCreate = Lens.lens (\TestingData' {autoCreate} -> autoCreate) (\s@TestingData' {} a -> s {autoCreate = a} :: TestingData)

instance Data.FromJSON TestingData where
  parseJSON =
    Data.withObject
      "TestingData"
      ( \x ->
          TestingData'
            Prelude.<$> (x Data..:? "Assets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AutoCreate")
      )

instance Prelude.Hashable TestingData where
  hashWithSalt _salt TestingData' {..} =
    _salt `Prelude.hashWithSalt` assets
      `Prelude.hashWithSalt` autoCreate

instance Prelude.NFData TestingData where
  rnf TestingData' {..} =
    Prelude.rnf assets
      `Prelude.seq` Prelude.rnf autoCreate

instance Data.ToJSON TestingData where
  toJSON TestingData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Assets" Data..=) Prelude.<$> assets,
            ("AutoCreate" Data..=) Prelude.<$> autoCreate
          ]
      )
