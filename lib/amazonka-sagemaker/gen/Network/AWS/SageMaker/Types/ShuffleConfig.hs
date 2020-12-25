{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ShuffleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ShuffleConfig
  ( ShuffleConfig (..),

    -- * Smart constructor
    mkShuffleConfig,

    -- * Lenses
    scSeed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , the results of the S3 key prefix matches are shuffled. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
-- For Pipe input mode, when @ShuffleConfig@ is specified shuffling is done at the start of every epoch. With large datasets, this ensures that the order of the training data is different for each epoch, and it helps reduce bias and possible overfitting. In a multi-node training job when @ShuffleConfig@ is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- /See:/ 'mkShuffleConfig' smart constructor.
newtype ShuffleConfig = ShuffleConfig'
  { -- | Determines the shuffling order in @ShuffleConfig@ value.
    seed :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ShuffleConfig' value with any optional fields omitted.
mkShuffleConfig ::
  -- | 'seed'
  Core.Integer ->
  ShuffleConfig
mkShuffleConfig seed = ShuffleConfig' {seed}

-- | Determines the shuffling order in @ShuffleConfig@ value.
--
-- /Note:/ Consider using 'seed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSeed :: Lens.Lens' ShuffleConfig Core.Integer
scSeed = Lens.field @"seed"
{-# DEPRECATED scSeed "Use generic-lens or generic-optics with 'seed' instead." #-}

instance Core.FromJSON ShuffleConfig where
  toJSON ShuffleConfig {..} =
    Core.object (Core.catMaybes [Core.Just ("Seed" Core..= seed)])

instance Core.FromJSON ShuffleConfig where
  parseJSON =
    Core.withObject "ShuffleConfig" Core.$
      \x -> ShuffleConfig' Core.<$> (x Core..: "Seed")
