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
-- Module      : Network.AWS.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for training.
--
-- /See:/ 'newTrainingData' smart constructor.
data TrainingData = TrainingData'
  { -- | A Sagemaker GroundTruth manifest file that contains the training images
    -- (assets).
    assets :: Prelude.Maybe [Asset]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
trainingData_assets = Lens.lens (\TrainingData' {assets} -> assets) (\s@TrainingData' {} a -> s {assets = a} :: TrainingData) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TrainingData where
  parseJSON =
    Prelude.withObject
      "TrainingData"
      ( \x ->
          TrainingData'
            Prelude.<$> (x Prelude..:? "Assets" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable TrainingData

instance Prelude.NFData TrainingData

instance Prelude.ToJSON TrainingData where
  toJSON TrainingData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Assets" Prelude..=) Prelude.<$> assets]
      )
