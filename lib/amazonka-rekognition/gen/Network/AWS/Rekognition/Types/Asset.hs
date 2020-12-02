{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Asset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Asset where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.GroundTruthManifest

-- | Assets are the images that you use to train and evaluate a model version. Assets can also contain validation information that you use to debug a failed model training.
--
--
--
-- /See:/ 'asset' smart constructor.
newtype Asset = Asset'
  { _aGroundTruthManifest ::
      Maybe GroundTruthManifest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Asset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aGroundTruthManifest' - Undocumented member.
asset ::
  Asset
asset = Asset' {_aGroundTruthManifest = Nothing}

-- | Undocumented member.
aGroundTruthManifest :: Lens' Asset (Maybe GroundTruthManifest)
aGroundTruthManifest = lens _aGroundTruthManifest (\s a -> s {_aGroundTruthManifest = a})

instance FromJSON Asset where
  parseJSON =
    withObject
      "Asset"
      (\x -> Asset' <$> (x .:? "GroundTruthManifest"))

instance Hashable Asset

instance NFData Asset

instance ToJSON Asset where
  toJSON Asset' {..} =
    object
      (catMaybes [("GroundTruthManifest" .=) <$> _aGroundTruthManifest])
