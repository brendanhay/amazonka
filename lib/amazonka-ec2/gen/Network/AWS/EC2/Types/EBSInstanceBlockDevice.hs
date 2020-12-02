{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInstanceBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInstanceBlockDevice where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a parameter used to set up an EBS volume in a block device mapping.
--
--
--
-- /See:/ 'ebsInstanceBlockDevice' smart constructor.
data EBSInstanceBlockDevice = EBSInstanceBlockDevice'
  { _eibdStatus ::
      !(Maybe AttachmentStatus),
    _eibdDeleteOnTermination :: !(Maybe Bool),
    _eibdVolumeId :: !(Maybe Text),
    _eibdAttachTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSInstanceBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eibdStatus' - The attachment state.
--
-- * 'eibdDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'eibdVolumeId' - The ID of the EBS volume.
--
-- * 'eibdAttachTime' - The time stamp when the attachment initiated.
ebsInstanceBlockDevice ::
  EBSInstanceBlockDevice
ebsInstanceBlockDevice =
  EBSInstanceBlockDevice'
    { _eibdStatus = Nothing,
      _eibdDeleteOnTermination = Nothing,
      _eibdVolumeId = Nothing,
      _eibdAttachTime = Nothing
    }

-- | The attachment state.
eibdStatus :: Lens' EBSInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\s a -> s {_eibdStatus = a})

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EBSInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination = lens _eibdDeleteOnTermination (\s a -> s {_eibdDeleteOnTermination = a})

-- | The ID of the EBS volume.
eibdVolumeId :: Lens' EBSInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\s a -> s {_eibdVolumeId = a})

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EBSInstanceBlockDevice (Maybe UTCTime)
eibdAttachTime = lens _eibdAttachTime (\s a -> s {_eibdAttachTime = a}) . mapping _Time

instance FromXML EBSInstanceBlockDevice where
  parseXML x =
    EBSInstanceBlockDevice'
      <$> (x .@? "status")
      <*> (x .@? "deleteOnTermination")
      <*> (x .@? "volumeId")
      <*> (x .@? "attachTime")

instance Hashable EBSInstanceBlockDevice

instance NFData EBSInstanceBlockDevice
