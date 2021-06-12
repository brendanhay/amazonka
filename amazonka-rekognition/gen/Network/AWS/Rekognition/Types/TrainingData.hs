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
-- Module      : Network.AWS.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for training.
--
-- /See:/ 'newTrainingData' smart constructor.
data TrainingData = TrainingData'
  { -- | A Sagemaker GroundTruth manifest file that contains the training images
    -- (assets).
    assets :: Core.Maybe [Asset]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  TrainingData' {assets = Core.Nothing}

-- | A Sagemaker GroundTruth manifest file that contains the training images
-- (assets).
trainingData_assets :: Lens.Lens' TrainingData (Core.Maybe [Asset])
trainingData_assets = Lens.lens (\TrainingData' {assets} -> assets) (\s@TrainingData' {} a -> s {assets = a} :: TrainingData) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TrainingData where
  parseJSON =
    Core.withObject
      "TrainingData"
      ( \x ->
          TrainingData'
            Core.<$> (x Core..:? "Assets" Core..!= Core.mempty)
      )

instance Core.Hashable TrainingData

instance Core.NFData TrainingData

instance Core.ToJSON TrainingData where
  toJSON TrainingData' {..} =
    Core.object
      (Core.catMaybes [("Assets" Core..=) Core.<$> assets])
