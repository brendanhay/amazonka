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
-- Module      : Amazonka.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TrainingData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Asset

-- | The dataset used for training.
--
-- /See:/ 'newTrainingData' smart constructor.
data TrainingData = TrainingData'
  { -- | A Sagemaker GroundTruth manifest file that contains the training images
    -- (assets).
    assets :: Prelude.Maybe [Asset]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'trainingData_assets' - A Sagemaker GroundTruth manifest file that contains the training images
-- (assets).
newTrainingData ::
  TrainingData
newTrainingData =
  TrainingData' {assets = Prelude.Nothing}

-- | A Sagemaker GroundTruth manifest file that contains the training images
-- (assets).
trainingData_assets :: Lens.Lens' TrainingData (Prelude.Maybe [Asset])
trainingData_assets = Lens.lens (\TrainingData' {assets} -> assets) (\s@TrainingData' {} a -> s {assets = a} :: TrainingData) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TrainingData where
  parseJSON =
    Data.withObject
      "TrainingData"
      ( \x ->
          TrainingData'
            Prelude.<$> (x Data..:? "Assets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TrainingData where
  hashWithSalt _salt TrainingData' {..} =
    _salt `Prelude.hashWithSalt` assets

instance Prelude.NFData TrainingData where
  rnf TrainingData' {..} = Prelude.rnf assets

instance Data.ToJSON TrainingData where
  toJSON TrainingData' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Assets" Data..=) Prelude.<$> assets]
      )
