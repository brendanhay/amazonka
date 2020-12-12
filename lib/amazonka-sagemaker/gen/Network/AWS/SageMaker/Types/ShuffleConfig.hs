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
import qualified Network.AWS.Prelude as Lude

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , the results of the S3 key prefix matches are shuffled. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
-- For Pipe input mode, when @ShuffleConfig@ is specified shuffling is done at the start of every epoch. With large datasets, this ensures that the order of the training data is different for each epoch, and it helps reduce bias and possible overfitting. In a multi-node training job when @ShuffleConfig@ is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- /See:/ 'mkShuffleConfig' smart constructor.
newtype ShuffleConfig = ShuffleConfig' {seed :: Lude.Integer}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShuffleConfig' with the minimum fields required to make a request.
--
-- * 'seed' - Determines the shuffling order in @ShuffleConfig@ value.
mkShuffleConfig ::
  -- | 'seed'
  Lude.Integer ->
  ShuffleConfig
mkShuffleConfig pSeed_ = ShuffleConfig' {seed = pSeed_}

-- | Determines the shuffling order in @ShuffleConfig@ value.
--
-- /Note:/ Consider using 'seed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSeed :: Lens.Lens' ShuffleConfig Lude.Integer
scSeed = Lens.lens (seed :: ShuffleConfig -> Lude.Integer) (\s a -> s {seed = a} :: ShuffleConfig)
{-# DEPRECATED scSeed "Use generic-lens or generic-optics with 'seed' instead." #-}

instance Lude.FromJSON ShuffleConfig where
  parseJSON =
    Lude.withObject
      "ShuffleConfig"
      (\x -> ShuffleConfig' Lude.<$> (x Lude..: "Seed"))

instance Lude.ToJSON ShuffleConfig where
  toJSON ShuffleConfig' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Seed" Lude..= seed)])
