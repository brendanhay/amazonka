{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingData
  ( TrainingData (..),

    -- * Smart constructor
    mkTrainingData,

    -- * Lenses
    tAssets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for training.
--
-- /See:/ 'mkTrainingData' smart constructor.
newtype TrainingData = TrainingData'
  { -- | A Sagemaker GroundTruth manifest file that contains the training images (assets).
    assets :: Lude.Maybe [Asset]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingData' with the minimum fields required to make a request.
--
-- * 'assets' - A Sagemaker GroundTruth manifest file that contains the training images (assets).
mkTrainingData ::
  TrainingData
mkTrainingData = TrainingData' {assets = Lude.Nothing}

-- | A Sagemaker GroundTruth manifest file that contains the training images (assets).
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAssets :: Lens.Lens' TrainingData (Lude.Maybe [Asset])
tAssets = Lens.lens (assets :: TrainingData -> Lude.Maybe [Asset]) (\s a -> s {assets = a} :: TrainingData)
{-# DEPRECATED tAssets "Use generic-lens or generic-optics with 'assets' instead." #-}

instance Lude.FromJSON TrainingData where
  parseJSON =
    Lude.withObject
      "TrainingData"
      ( \x ->
          TrainingData' Lude.<$> (x Lude..:? "Assets" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TrainingData where
  toJSON TrainingData' {..} =
    Lude.object (Lude.catMaybes [("Assets" Lude..=) Lude.<$> assets])
