{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TrainingData
  ( TrainingData (..)
  -- * Smart constructor
  , mkTrainingData
  -- * Lenses
  , tAssets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Asset as Types

-- | The dataset used for training.
--
-- /See:/ 'mkTrainingData' smart constructor.
newtype TrainingData = TrainingData'
  { assets :: Core.Maybe [Types.Asset]
    -- ^ A Sagemaker GroundTruth manifest file that contains the training images (assets).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TrainingData' value with any optional fields omitted.
mkTrainingData
    :: TrainingData
mkTrainingData = TrainingData'{assets = Core.Nothing}

-- | A Sagemaker GroundTruth manifest file that contains the training images (assets).
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAssets :: Lens.Lens' TrainingData (Core.Maybe [Types.Asset])
tAssets = Lens.field @"assets"
{-# INLINEABLE tAssets #-}
{-# DEPRECATED assets "Use generic-lens or generic-optics with 'assets' instead"  #-}

instance Core.FromJSON TrainingData where
        toJSON TrainingData{..}
          = Core.object (Core.catMaybes [("Assets" Core..=) Core.<$> assets])

instance Core.FromJSON TrainingData where
        parseJSON
          = Core.withObject "TrainingData" Core.$
              \ x -> TrainingData' Core.<$> (x Core..:? "Assets")
