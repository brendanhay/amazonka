{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ValidationData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ValidationData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Asset

-- | Contains the Amazon S3 bucket location of the validation data for a model training job.
--
--
-- The validation data includes error information for individual JSON lines in the dataset. For more information, see Debugging a Failed Model Training in the Amazon Rekognition Custom Labels Developer Guide.
--
-- You get the @ValidationData@ object for the training dataset ('TrainingDataResult' ) and the test dataset ('TestingDataResult' ) by calling 'DescribeProjectVersions' .
--
-- The assets array contains a single 'Asset' object. The 'GroundTruthManifest' field of the Asset object contains the S3 bucket location of the validation data.
--
--
-- /See:/ 'validationData' smart constructor.
newtype ValidationData = ValidationData'
  { _vdAssets ::
      Maybe [Asset]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdAssets' - The assets that comprise the validation data.
validationData ::
  ValidationData
validationData = ValidationData' {_vdAssets = Nothing}

-- | The assets that comprise the validation data.
vdAssets :: Lens' ValidationData [Asset]
vdAssets = lens _vdAssets (\s a -> s {_vdAssets = a}) . _Default . _Coerce

instance FromJSON ValidationData where
  parseJSON =
    withObject
      "ValidationData"
      (\x -> ValidationData' <$> (x .:? "Assets" .!= mempty))

instance Hashable ValidationData

instance NFData ValidationData
