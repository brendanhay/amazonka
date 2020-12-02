{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface attachment.
--
--
--
-- /See:/ 'instanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { _iniaStatus ::
      !( Maybe
           AttachmentStatus
       ),
    _iniaDeleteOnTermination ::
      !(Maybe Bool),
    _iniaAttachmentId ::
      !(Maybe Text),
    _iniaNetworkCardIndex ::
      !(Maybe Int),
    _iniaAttachTime ::
      !(Maybe ISO8601),
    _iniaDeviceIndex ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceNetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniaStatus' - The attachment state.
--
-- * 'iniaDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'iniaAttachmentId' - The ID of the network interface attachment.
--
-- * 'iniaNetworkCardIndex' - The index of the network card.
--
-- * 'iniaAttachTime' - The time stamp when the attachment initiated.
--
-- * 'iniaDeviceIndex' - The index of the device on the instance for the network interface attachment.
instanceNetworkInterfaceAttachment ::
  InstanceNetworkInterfaceAttachment
instanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { _iniaStatus = Nothing,
      _iniaDeleteOnTermination = Nothing,
      _iniaAttachmentId = Nothing,
      _iniaNetworkCardIndex = Nothing,
      _iniaAttachTime = Nothing,
      _iniaDeviceIndex = Nothing
    }

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\s a -> s {_iniaStatus = a})

-- | Indicates whether the network interface is deleted when the instance is terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination = lens _iniaDeleteOnTermination (\s a -> s {_iniaDeleteOnTermination = a})

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\s a -> s {_iniaAttachmentId = a})

-- | The index of the network card.
iniaNetworkCardIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaNetworkCardIndex = lens _iniaNetworkCardIndex (\s a -> s {_iniaNetworkCardIndex = a})

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe UTCTime)
iniaAttachTime = lens _iniaAttachTime (\s a -> s {_iniaAttachTime = a}) . mapping _Time

-- | The index of the device on the instance for the network interface attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaDeviceIndex = lens _iniaDeviceIndex (\s a -> s {_iniaDeviceIndex = a})

instance FromXML InstanceNetworkInterfaceAttachment where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      <$> (x .@? "status")
      <*> (x .@? "deleteOnTermination")
      <*> (x .@? "attachmentId")
      <*> (x .@? "networkCardIndex")
      <*> (x .@? "attachTime")
      <*> (x .@? "deviceIndex")

instance Hashable InstanceNetworkInterfaceAttachment

instance NFData InstanceNetworkInterfaceAttachment
