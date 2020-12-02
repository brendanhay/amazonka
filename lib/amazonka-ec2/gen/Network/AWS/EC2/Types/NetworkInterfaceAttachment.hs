{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface attachment.
--
--
--
-- /See:/ 'networkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { _niaInstanceId ::
      !(Maybe Text),
    _niaStatus ::
      !(Maybe AttachmentStatus),
    _niaDeleteOnTermination ::
      !(Maybe Bool),
    _niaAttachmentId :: !(Maybe Text),
    _niaNetworkCardIndex :: !(Maybe Int),
    _niaInstanceOwnerId :: !(Maybe Text),
    _niaAttachTime :: !(Maybe ISO8601),
    _niaDeviceIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niaInstanceId' - The ID of the instance.
--
-- * 'niaStatus' - The attachment state.
--
-- * 'niaDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'niaAttachmentId' - The ID of the network interface attachment.
--
-- * 'niaNetworkCardIndex' - The index of the network card.
--
-- * 'niaInstanceOwnerId' - The AWS account ID of the owner of the instance.
--
-- * 'niaAttachTime' - The timestamp indicating when the attachment initiated.
--
-- * 'niaDeviceIndex' - The device index of the network interface attachment on the instance.
networkInterfaceAttachment ::
  NetworkInterfaceAttachment
networkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { _niaInstanceId = Nothing,
      _niaStatus = Nothing,
      _niaDeleteOnTermination = Nothing,
      _niaAttachmentId = Nothing,
      _niaNetworkCardIndex = Nothing,
      _niaInstanceOwnerId = Nothing,
      _niaAttachTime = Nothing,
      _niaDeviceIndex = Nothing
    }

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\s a -> s {_niaInstanceId = a})

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\s a -> s {_niaStatus = a})

-- | Indicates whether the network interface is deleted when the instance is terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination = lens _niaDeleteOnTermination (\s a -> s {_niaDeleteOnTermination = a})

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\s a -> s {_niaAttachmentId = a})

-- | The index of the network card.
niaNetworkCardIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaNetworkCardIndex = lens _niaNetworkCardIndex (\s a -> s {_niaNetworkCardIndex = a})

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId = lens _niaInstanceOwnerId (\s a -> s {_niaInstanceOwnerId = a})

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe UTCTime)
niaAttachTime = lens _niaAttachTime (\s a -> s {_niaAttachTime = a}) . mapping _Time

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaDeviceIndex = lens _niaDeviceIndex (\s a -> s {_niaDeviceIndex = a})

instance FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      <$> (x .@? "instanceId")
      <*> (x .@? "status")
      <*> (x .@? "deleteOnTermination")
      <*> (x .@? "attachmentId")
      <*> (x .@? "networkCardIndex")
      <*> (x .@? "instanceOwnerId")
      <*> (x .@? "attachTime")
      <*> (x .@? "deviceIndex")

instance Hashable NetworkInterfaceAttachment

instance NFData NetworkInterfaceAttachment
