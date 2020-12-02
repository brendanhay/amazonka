{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for training.
--
--
--
-- /See:/ 'trainingData' smart constructor.
newtype TrainingData = TrainingData' {_tAssets :: Maybe [Asset]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tAssets' - A Sagemaker GroundTruth manifest file that contains the training images (assets).
trainingData ::
  TrainingData
trainingData = TrainingData' {_tAssets = Nothing}

-- | A Sagemaker GroundTruth manifest file that contains the training images (assets).
tAssets :: Lens' TrainingData [Asset]
tAssets = lens _tAssets (\s a -> s {_tAssets = a}) . _Default . _Coerce

instance FromJSON TrainingData where
  parseJSON =
    withObject
      "TrainingData"
      (\x -> TrainingData' <$> (x .:? "Assets" .!= mempty))

instance Hashable TrainingData

instance NFData TrainingData

instance ToJSON TrainingData where
  toJSON TrainingData' {..} =
    object (catMaybes [("Assets" .=) <$> _tAssets])
