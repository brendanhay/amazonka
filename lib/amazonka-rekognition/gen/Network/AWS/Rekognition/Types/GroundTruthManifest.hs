{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GroundTruthManifest where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format manifest file.
--
--
--
-- /See:/ 'groundTruthManifest' smart constructor.
newtype GroundTruthManifest = GroundTruthManifest'
  { _gtmS3Object ::
      Maybe S3Object
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroundTruthManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmS3Object' - Undocumented member.
groundTruthManifest ::
  GroundTruthManifest
groundTruthManifest = GroundTruthManifest' {_gtmS3Object = Nothing}

-- | Undocumented member.
gtmS3Object :: Lens' GroundTruthManifest (Maybe S3Object)
gtmS3Object = lens _gtmS3Object (\s a -> s {_gtmS3Object = a})

instance FromJSON GroundTruthManifest where
  parseJSON =
    withObject
      "GroundTruthManifest"
      (\x -> GroundTruthManifest' <$> (x .:? "S3Object"))

instance Hashable GroundTruthManifest

instance NFData GroundTruthManifest

instance ToJSON GroundTruthManifest where
  toJSON GroundTruthManifest' {..} =
    object (catMaybes [("S3Object" .=) <$> _gtmS3Object])
