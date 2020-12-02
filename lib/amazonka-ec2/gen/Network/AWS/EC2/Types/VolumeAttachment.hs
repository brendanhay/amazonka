{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeAttachmentState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes volume attachment details.
--
--
--
-- /See:/ 'volumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { _volInstanceId ::
      !(Maybe Text),
    _volDeleteOnTermination :: !(Maybe Bool),
    _volState :: !(Maybe VolumeAttachmentState),
    _volDevice :: !(Maybe Text),
    _volVolumeId :: !(Maybe Text),
    _volAttachTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'volInstanceId' - The ID of the instance.
--
-- * 'volDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'volState' - The attachment state of the volume.
--
-- * 'volDevice' - The device name.
--
-- * 'volVolumeId' - The ID of the volume.
--
-- * 'volAttachTime' - The time stamp when the attachment initiated.
volumeAttachment ::
  VolumeAttachment
volumeAttachment =
  VolumeAttachment'
    { _volInstanceId = Nothing,
      _volDeleteOnTermination = Nothing,
      _volState = Nothing,
      _volDevice = Nothing,
      _volVolumeId = Nothing,
      _volAttachTime = Nothing
    }

-- | The ID of the instance.
volInstanceId :: Lens' VolumeAttachment (Maybe Text)
volInstanceId = lens _volInstanceId (\s a -> s {_volInstanceId = a})

-- | Indicates whether the EBS volume is deleted on instance termination.
volDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
volDeleteOnTermination = lens _volDeleteOnTermination (\s a -> s {_volDeleteOnTermination = a})

-- | The attachment state of the volume.
volState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
volState = lens _volState (\s a -> s {_volState = a})

-- | The device name.
volDevice :: Lens' VolumeAttachment (Maybe Text)
volDevice = lens _volDevice (\s a -> s {_volDevice = a})

-- | The ID of the volume.
volVolumeId :: Lens' VolumeAttachment (Maybe Text)
volVolumeId = lens _volVolumeId (\s a -> s {_volVolumeId = a})

-- | The time stamp when the attachment initiated.
volAttachTime :: Lens' VolumeAttachment (Maybe UTCTime)
volAttachTime = lens _volAttachTime (\s a -> s {_volAttachTime = a}) . mapping _Time

instance FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      <$> (x .@? "instanceId")
      <*> (x .@? "deleteOnTermination")
      <*> (x .@? "status")
      <*> (x .@? "device")
      <*> (x .@? "volumeId")
      <*> (x .@? "attachTime")

instance Hashable VolumeAttachment

instance NFData VolumeAttachment
