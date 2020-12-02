{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformResources where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Describes the resources, including ML instance types and ML instance count, to use for transform job.
--
--
--
-- /See:/ 'transformResources' smart constructor.
data TransformResources = TransformResources'
  { _trVolumeKMSKeyId ::
      !(Maybe Text),
    _trInstanceType :: !TransformInstanceType,
    _trInstanceCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trVolumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt model data on the storage volume attached to the ML compute instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can be any of the following formats:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
--
-- * 'trInstanceType' - The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
--
-- * 'trInstanceCount' - The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
transformResources ::
  -- | 'trInstanceType'
  TransformInstanceType ->
  -- | 'trInstanceCount'
  Natural ->
  TransformResources
transformResources pInstanceType_ pInstanceCount_ =
  TransformResources'
    { _trVolumeKMSKeyId = Nothing,
      _trInstanceType = pInstanceType_,
      _trInstanceCount = _Nat # pInstanceCount_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt model data on the storage volume attached to the ML compute instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can be any of the following formats:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
trVolumeKMSKeyId :: Lens' TransformResources (Maybe Text)
trVolumeKMSKeyId = lens _trVolumeKMSKeyId (\s a -> s {_trVolumeKMSKeyId = a})

-- | The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
trInstanceType :: Lens' TransformResources TransformInstanceType
trInstanceType = lens _trInstanceType (\s a -> s {_trInstanceType = a})

-- | The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
trInstanceCount :: Lens' TransformResources Natural
trInstanceCount = lens _trInstanceCount (\s a -> s {_trInstanceCount = a}) . _Nat

instance FromJSON TransformResources where
  parseJSON =
    withObject
      "TransformResources"
      ( \x ->
          TransformResources'
            <$> (x .:? "VolumeKmsKeyId")
            <*> (x .: "InstanceType")
            <*> (x .: "InstanceCount")
      )

instance Hashable TransformResources

instance NFData TransformResources

instance ToJSON TransformResources where
  toJSON TransformResources' {..} =
    object
      ( catMaybes
          [ ("VolumeKmsKeyId" .=) <$> _trVolumeKMSKeyId,
            Just ("InstanceType" .= _trInstanceType),
            Just ("InstanceCount" .= _trInstanceCount)
          ]
      )
