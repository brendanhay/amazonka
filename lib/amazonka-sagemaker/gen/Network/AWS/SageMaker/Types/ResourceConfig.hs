{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TrainingInstanceType

-- | Describes the resources, including ML compute instances and ML storage volumes, to use for model training.
--
--
--
-- /See:/ 'resourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { _rcVolumeKMSKeyId ::
      !(Maybe Text),
    _rcInstanceType :: !TrainingInstanceType,
    _rcInstanceCount :: !Nat,
    _rcVolumeSizeInGB :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcVolumeKMSKeyId' - The AWS KMS key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be in any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'rcInstanceType' - The ML compute instance type.
--
-- * 'rcInstanceCount' - The number of ML compute instances to use. For distributed training, provide a value greater than 1.
--
-- * 'rcVolumeSizeInGB' - The size of the ML storage volume that you want to provision.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.  You must specify sufficient ML storage for your scenario.
resourceConfig ::
  -- | 'rcInstanceType'
  TrainingInstanceType ->
  -- | 'rcInstanceCount'
  Natural ->
  -- | 'rcVolumeSizeInGB'
  Natural ->
  ResourceConfig
resourceConfig pInstanceType_ pInstanceCount_ pVolumeSizeInGB_ =
  ResourceConfig'
    { _rcVolumeKMSKeyId = Nothing,
      _rcInstanceType = pInstanceType_,
      _rcInstanceCount = _Nat # pInstanceCount_,
      _rcVolumeSizeInGB = _Nat # pVolumeSizeInGB_
    }

-- | The AWS KMS key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be in any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
rcVolumeKMSKeyId :: Lens' ResourceConfig (Maybe Text)
rcVolumeKMSKeyId = lens _rcVolumeKMSKeyId (\s a -> s {_rcVolumeKMSKeyId = a})

-- | The ML compute instance type.
rcInstanceType :: Lens' ResourceConfig TrainingInstanceType
rcInstanceType = lens _rcInstanceType (\s a -> s {_rcInstanceType = a})

-- | The number of ML compute instances to use. For distributed training, provide a value greater than 1.
rcInstanceCount :: Lens' ResourceConfig Natural
rcInstanceCount = lens _rcInstanceCount (\s a -> s {_rcInstanceCount = a}) . _Nat

-- | The size of the ML storage volume that you want to provision.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.  You must specify sufficient ML storage for your scenario.
rcVolumeSizeInGB :: Lens' ResourceConfig Natural
rcVolumeSizeInGB = lens _rcVolumeSizeInGB (\s a -> s {_rcVolumeSizeInGB = a}) . _Nat

instance FromJSON ResourceConfig where
  parseJSON =
    withObject
      "ResourceConfig"
      ( \x ->
          ResourceConfig'
            <$> (x .:? "VolumeKmsKeyId")
            <*> (x .: "InstanceType")
            <*> (x .: "InstanceCount")
            <*> (x .: "VolumeSizeInGB")
      )

instance Hashable ResourceConfig

instance NFData ResourceConfig

instance ToJSON ResourceConfig where
  toJSON ResourceConfig' {..} =
    object
      ( catMaybes
          [ ("VolumeKmsKeyId" .=) <$> _rcVolumeKMSKeyId,
            Just ("InstanceType" .= _rcInstanceType),
            Just ("InstanceCount" .= _rcInstanceCount),
            Just ("VolumeSizeInGB" .= _rcVolumeSizeInGB)
          ]
      )
