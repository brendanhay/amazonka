{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringClusterConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run model monitoring jobs.
--
--
--
-- /See:/ 'monitoringClusterConfig' smart constructor.
data MonitoringClusterConfig = MonitoringClusterConfig'
  { _mccVolumeKMSKeyId ::
      !(Maybe Text),
    _mccInstanceCount :: !Nat,
    _mccInstanceType :: !ProcessingInstanceType,
    _mccVolumeSizeInGB :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringClusterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mccVolumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
--
-- * 'mccInstanceCount' - The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- * 'mccInstanceType' - The ML compute instance type for the processing job.
--
-- * 'mccVolumeSizeInGB' - The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
monitoringClusterConfig ::
  -- | 'mccInstanceCount'
  Natural ->
  -- | 'mccInstanceType'
  ProcessingInstanceType ->
  -- | 'mccVolumeSizeInGB'
  Natural ->
  MonitoringClusterConfig
monitoringClusterConfig
  pInstanceCount_
  pInstanceType_
  pVolumeSizeInGB_ =
    MonitoringClusterConfig'
      { _mccVolumeKMSKeyId = Nothing,
        _mccInstanceCount = _Nat # pInstanceCount_,
        _mccInstanceType = pInstanceType_,
        _mccVolumeSizeInGB = _Nat # pVolumeSizeInGB_
      }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
mccVolumeKMSKeyId :: Lens' MonitoringClusterConfig (Maybe Text)
mccVolumeKMSKeyId = lens _mccVolumeKMSKeyId (\s a -> s {_mccVolumeKMSKeyId = a})

-- | The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
mccInstanceCount :: Lens' MonitoringClusterConfig Natural
mccInstanceCount = lens _mccInstanceCount (\s a -> s {_mccInstanceCount = a}) . _Nat

-- | The ML compute instance type for the processing job.
mccInstanceType :: Lens' MonitoringClusterConfig ProcessingInstanceType
mccInstanceType = lens _mccInstanceType (\s a -> s {_mccInstanceType = a})

-- | The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
mccVolumeSizeInGB :: Lens' MonitoringClusterConfig Natural
mccVolumeSizeInGB = lens _mccVolumeSizeInGB (\s a -> s {_mccVolumeSizeInGB = a}) . _Nat

instance FromJSON MonitoringClusterConfig where
  parseJSON =
    withObject
      "MonitoringClusterConfig"
      ( \x ->
          MonitoringClusterConfig'
            <$> (x .:? "VolumeKmsKeyId")
            <*> (x .: "InstanceCount")
            <*> (x .: "InstanceType")
            <*> (x .: "VolumeSizeInGB")
      )

instance Hashable MonitoringClusterConfig

instance NFData MonitoringClusterConfig

instance ToJSON MonitoringClusterConfig where
  toJSON MonitoringClusterConfig' {..} =
    object
      ( catMaybes
          [ ("VolumeKmsKeyId" .=) <$> _mccVolumeKMSKeyId,
            Just ("InstanceCount" .= _mccInstanceCount),
            Just ("InstanceType" .= _mccInstanceType),
            Just ("VolumeSizeInGB" .= _mccVolumeSizeInGB)
          ]
      )
