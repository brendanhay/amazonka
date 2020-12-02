{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeModification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeModificationState
import Network.AWS.EC2.Types.VolumeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the modification status of an EBS volume.
--
--
-- If the volume has never been modified, some element values will be null.
--
--
-- /See:/ 'volumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { _vmProgress ::
      !(Maybe Integer),
    _vmStartTime :: !(Maybe ISO8601),
    _vmModificationState ::
      !(Maybe VolumeModificationState),
    _vmTargetVolumeType :: !(Maybe VolumeType),
    _vmOriginalVolumeType :: !(Maybe VolumeType),
    _vmTargetSize :: !(Maybe Int),
    _vmTargetIOPS :: !(Maybe Int),
    _vmOriginalSize :: !(Maybe Int),
    _vmOriginalIOPS :: !(Maybe Int),
    _vmStatusMessage :: !(Maybe Text),
    _vmEndTime :: !(Maybe ISO8601),
    _vmVolumeId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeModification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmProgress' - The modification progress, from 0 to 100 percent complete.
--
-- * 'vmStartTime' - The modification start time.
--
-- * 'vmModificationState' - The current modification state. The modification state is null for unmodified volumes.
--
-- * 'vmTargetVolumeType' - The target EBS volume type of the volume.
--
-- * 'vmOriginalVolumeType' - The original EBS volume type of the volume.
--
-- * 'vmTargetSize' - The target size of the volume, in GiB.
--
-- * 'vmTargetIOPS' - The target IOPS rate of the volume.
--
-- * 'vmOriginalSize' - The original size of the volume, in GiB.
--
-- * 'vmOriginalIOPS' - The original IOPS rate of the volume.
--
-- * 'vmStatusMessage' - A status message about the modification progress or failure.
--
-- * 'vmEndTime' - The modification completion or failure time.
--
-- * 'vmVolumeId' - The ID of the volume.
volumeModification ::
  VolumeModification
volumeModification =
  VolumeModification'
    { _vmProgress = Nothing,
      _vmStartTime = Nothing,
      _vmModificationState = Nothing,
      _vmTargetVolumeType = Nothing,
      _vmOriginalVolumeType = Nothing,
      _vmTargetSize = Nothing,
      _vmTargetIOPS = Nothing,
      _vmOriginalSize = Nothing,
      _vmOriginalIOPS = Nothing,
      _vmStatusMessage = Nothing,
      _vmEndTime = Nothing,
      _vmVolumeId = Nothing
    }

-- | The modification progress, from 0 to 100 percent complete.
vmProgress :: Lens' VolumeModification (Maybe Integer)
vmProgress = lens _vmProgress (\s a -> s {_vmProgress = a})

-- | The modification start time.
vmStartTime :: Lens' VolumeModification (Maybe UTCTime)
vmStartTime = lens _vmStartTime (\s a -> s {_vmStartTime = a}) . mapping _Time

-- | The current modification state. The modification state is null for unmodified volumes.
vmModificationState :: Lens' VolumeModification (Maybe VolumeModificationState)
vmModificationState = lens _vmModificationState (\s a -> s {_vmModificationState = a})

-- | The target EBS volume type of the volume.
vmTargetVolumeType :: Lens' VolumeModification (Maybe VolumeType)
vmTargetVolumeType = lens _vmTargetVolumeType (\s a -> s {_vmTargetVolumeType = a})

-- | The original EBS volume type of the volume.
vmOriginalVolumeType :: Lens' VolumeModification (Maybe VolumeType)
vmOriginalVolumeType = lens _vmOriginalVolumeType (\s a -> s {_vmOriginalVolumeType = a})

-- | The target size of the volume, in GiB.
vmTargetSize :: Lens' VolumeModification (Maybe Int)
vmTargetSize = lens _vmTargetSize (\s a -> s {_vmTargetSize = a})

-- | The target IOPS rate of the volume.
vmTargetIOPS :: Lens' VolumeModification (Maybe Int)
vmTargetIOPS = lens _vmTargetIOPS (\s a -> s {_vmTargetIOPS = a})

-- | The original size of the volume, in GiB.
vmOriginalSize :: Lens' VolumeModification (Maybe Int)
vmOriginalSize = lens _vmOriginalSize (\s a -> s {_vmOriginalSize = a})

-- | The original IOPS rate of the volume.
vmOriginalIOPS :: Lens' VolumeModification (Maybe Int)
vmOriginalIOPS = lens _vmOriginalIOPS (\s a -> s {_vmOriginalIOPS = a})

-- | A status message about the modification progress or failure.
vmStatusMessage :: Lens' VolumeModification (Maybe Text)
vmStatusMessage = lens _vmStatusMessage (\s a -> s {_vmStatusMessage = a})

-- | The modification completion or failure time.
vmEndTime :: Lens' VolumeModification (Maybe UTCTime)
vmEndTime = lens _vmEndTime (\s a -> s {_vmEndTime = a}) . mapping _Time

-- | The ID of the volume.
vmVolumeId :: Lens' VolumeModification (Maybe Text)
vmVolumeId = lens _vmVolumeId (\s a -> s {_vmVolumeId = a})

instance FromXML VolumeModification where
  parseXML x =
    VolumeModification'
      <$> (x .@? "progress")
      <*> (x .@? "startTime")
      <*> (x .@? "modificationState")
      <*> (x .@? "targetVolumeType")
      <*> (x .@? "originalVolumeType")
      <*> (x .@? "targetSize")
      <*> (x .@? "targetIops")
      <*> (x .@? "originalSize")
      <*> (x .@? "originalIops")
      <*> (x .@? "statusMessage")
      <*> (x .@? "endTime")
      <*> (x .@? "volumeId")

instance Hashable VolumeModification

instance NFData VolumeModification
