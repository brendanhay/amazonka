{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingClusterConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run a processing job.
--
--
--
-- /See:/ 'processingClusterConfig' smart constructor.
data ProcessingClusterConfig = ProcessingClusterConfig'
  { _pccVolumeKMSKeyId ::
      !(Maybe Text),
    _pccInstanceCount :: !Nat,
    _pccInstanceType :: !ProcessingInstanceType,
    _pccVolumeSizeInGB :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingClusterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pccVolumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job.
--
-- * 'pccInstanceCount' - The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- * 'pccInstanceType' - The ML compute instance type for the processing job.
--
-- * 'pccVolumeSizeInGB' - The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
processingClusterConfig ::
  -- | 'pccInstanceCount'
  Natural ->
  -- | 'pccInstanceType'
  ProcessingInstanceType ->
  -- | 'pccVolumeSizeInGB'
  Natural ->
  ProcessingClusterConfig
processingClusterConfig
  pInstanceCount_
  pInstanceType_
  pVolumeSizeInGB_ =
    ProcessingClusterConfig'
      { _pccVolumeKMSKeyId = Nothing,
        _pccInstanceCount = _Nat # pInstanceCount_,
        _pccInstanceType = pInstanceType_,
        _pccVolumeSizeInGB = _Nat # pVolumeSizeInGB_
      }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job.
pccVolumeKMSKeyId :: Lens' ProcessingClusterConfig (Maybe Text)
pccVolumeKMSKeyId = lens _pccVolumeKMSKeyId (\s a -> s {_pccVolumeKMSKeyId = a})

-- | The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
pccInstanceCount :: Lens' ProcessingClusterConfig Natural
pccInstanceCount = lens _pccInstanceCount (\s a -> s {_pccInstanceCount = a}) . _Nat

-- | The ML compute instance type for the processing job.
pccInstanceType :: Lens' ProcessingClusterConfig ProcessingInstanceType
pccInstanceType = lens _pccInstanceType (\s a -> s {_pccInstanceType = a})

-- | The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
pccVolumeSizeInGB :: Lens' ProcessingClusterConfig Natural
pccVolumeSizeInGB = lens _pccVolumeSizeInGB (\s a -> s {_pccVolumeSizeInGB = a}) . _Nat

instance FromJSON ProcessingClusterConfig where
  parseJSON =
    withObject
      "ProcessingClusterConfig"
      ( \x ->
          ProcessingClusterConfig'
            <$> (x .:? "VolumeKmsKeyId")
            <*> (x .: "InstanceCount")
            <*> (x .: "InstanceType")
            <*> (x .: "VolumeSizeInGB")
      )

instance Hashable ProcessingClusterConfig

instance NFData ProcessingClusterConfig

instance ToJSON ProcessingClusterConfig where
  toJSON ProcessingClusterConfig' {..} =
    object
      ( catMaybes
          [ ("VolumeKmsKeyId" .=) <$> _pccVolumeKMSKeyId,
            Just ("InstanceCount" .= _pccInstanceCount),
            Just ("InstanceType" .= _pccInstanceType),
            Just ("VolumeSizeInGB" .= _pccVolumeSizeInGB)
          ]
      )
