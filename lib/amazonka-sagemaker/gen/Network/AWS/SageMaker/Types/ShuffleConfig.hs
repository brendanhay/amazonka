{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ShuffleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ShuffleConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , the results of the S3 key prefix matches are shuffled. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
--
-- For Pipe input mode, when @ShuffleConfig@ is specified shuffling is done at the start of every epoch. With large datasets, this ensures that the order of the training data is different for each epoch, and it helps reduce bias and possible overfitting. In a multi-node training job when @ShuffleConfig@ is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
--
-- /See:/ 'shuffleConfig' smart constructor.
newtype ShuffleConfig = ShuffleConfig' {_scSeed :: Integer}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShuffleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scSeed' - Determines the shuffling order in @ShuffleConfig@ value.
shuffleConfig ::
  -- | 'scSeed'
  Integer ->
  ShuffleConfig
shuffleConfig pSeed_ = ShuffleConfig' {_scSeed = pSeed_}

-- | Determines the shuffling order in @ShuffleConfig@ value.
scSeed :: Lens' ShuffleConfig Integer
scSeed = lens _scSeed (\s a -> s {_scSeed = a})

instance FromJSON ShuffleConfig where
  parseJSON =
    withObject
      "ShuffleConfig"
      (\x -> ShuffleConfig' <$> (x .: "Seed"))

instance Hashable ShuffleConfig

instance NFData ShuffleConfig

instance ToJSON ShuffleConfig where
  toJSON ShuffleConfig' {..} =
    object (catMaybes [Just ("Seed" .= _scSeed)])
