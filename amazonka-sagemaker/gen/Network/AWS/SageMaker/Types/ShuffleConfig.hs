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
-- Module      : Network.AWS.SageMaker.Types.ShuffleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ShuffleConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A configuration for a shuffle option for input data in a channel. If you
-- use @S3Prefix@ for @S3DataType@, the results of the S3 key prefix
-- matches are shuffled. If you use @ManifestFile@, the order of the S3
-- object references in the @ManifestFile@ is shuffled. If you use
-- @AugmentedManifestFile@, the order of the JSON lines in the
-- @AugmentedManifestFile@ is shuffled. The shuffling order is determined
-- using the @Seed@ value.
--
-- For Pipe input mode, when @ShuffleConfig@ is specified shuffling is done
-- at the start of every epoch. With large datasets, this ensures that the
-- order of the training data is different for each epoch, and it helps
-- reduce bias and possible overfitting. In a multi-node training job when
-- @ShuffleConfig@ is combined with @S3DataDistributionType@ of
-- @ShardedByS3Key@, the data is shuffled across nodes so that the content
-- sent to a particular node on the first epoch might be sent to a
-- different node on the second epoch.
--
-- /See:/ 'newShuffleConfig' smart constructor.
data ShuffleConfig = ShuffleConfig'
  { -- | Determines the shuffling order in @ShuffleConfig@ value.
    seed :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ShuffleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seed', 'shuffleConfig_seed' - Determines the shuffling order in @ShuffleConfig@ value.
newShuffleConfig ::
  -- | 'seed'
  Prelude.Integer ->
  ShuffleConfig
newShuffleConfig pSeed_ =
  ShuffleConfig' {seed = pSeed_}

-- | Determines the shuffling order in @ShuffleConfig@ value.
shuffleConfig_seed :: Lens.Lens' ShuffleConfig Prelude.Integer
shuffleConfig_seed = Lens.lens (\ShuffleConfig' {seed} -> seed) (\s@ShuffleConfig' {} a -> s {seed = a} :: ShuffleConfig)

instance Prelude.FromJSON ShuffleConfig where
  parseJSON =
    Prelude.withObject
      "ShuffleConfig"
      ( \x ->
          ShuffleConfig' Prelude.<$> (x Prelude..: "Seed")
      )

instance Prelude.Hashable ShuffleConfig

instance Prelude.NFData ShuffleConfig

instance Prelude.ToJSON ShuffleConfig where
  toJSON ShuffleConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Seed" Prelude..= seed)]
      )
