{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment where

import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an attachment between a Direct Connect gateway and a virtual interface.
--
--
--
-- /See:/ 'directConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { _dDirectConnectGatewayId ::
      !(Maybe Text),
    _dAttachmentState ::
      !( Maybe
           DirectConnectGatewayAttachmentState
       ),
    _dStateChangeError ::
      !(Maybe Text),
    _dVirtualInterfaceRegion ::
      !(Maybe Text),
    _dVirtualInterfaceOwnerAccount ::
      !(Maybe Text),
    _dVirtualInterfaceId ::
      !(Maybe Text),
    _dAttachmentType ::
      !( Maybe
           DirectConnectGatewayAttachmentType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectConnectGatewayAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'dAttachmentState' - The state of the attachment. The following are the possible values:     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
--
-- * 'dStateChangeError' - The error message if the state of an object failed to advance.
--
-- * 'dVirtualInterfaceRegion' - The AWS Region where the virtual interface is located.
--
-- * 'dVirtualInterfaceOwnerAccount' - The ID of the AWS account that owns the virtual interface.
--
-- * 'dVirtualInterfaceId' - The ID of the virtual interface.
--
-- * 'dAttachmentType' - The type of attachment.
directConnectGatewayAttachment ::
  DirectConnectGatewayAttachment
directConnectGatewayAttachment =
  DirectConnectGatewayAttachment'
    { _dDirectConnectGatewayId =
        Nothing,
      _dAttachmentState = Nothing,
      _dStateChangeError = Nothing,
      _dVirtualInterfaceRegion = Nothing,
      _dVirtualInterfaceOwnerAccount = Nothing,
      _dVirtualInterfaceId = Nothing,
      _dAttachmentType = Nothing
    }

-- | The ID of the Direct Connect gateway.
dDirectConnectGatewayId :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dDirectConnectGatewayId = lens _dDirectConnectGatewayId (\s a -> s {_dDirectConnectGatewayId = a})

-- | The state of the attachment. The following are the possible values:     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
dAttachmentState :: Lens' DirectConnectGatewayAttachment (Maybe DirectConnectGatewayAttachmentState)
dAttachmentState = lens _dAttachmentState (\s a -> s {_dAttachmentState = a})

-- | The error message if the state of an object failed to advance.
dStateChangeError :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dStateChangeError = lens _dStateChangeError (\s a -> s {_dStateChangeError = a})

-- | The AWS Region where the virtual interface is located.
dVirtualInterfaceRegion :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceRegion = lens _dVirtualInterfaceRegion (\s a -> s {_dVirtualInterfaceRegion = a})

-- | The ID of the AWS account that owns the virtual interface.
dVirtualInterfaceOwnerAccount :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceOwnerAccount = lens _dVirtualInterfaceOwnerAccount (\s a -> s {_dVirtualInterfaceOwnerAccount = a})

-- | The ID of the virtual interface.
dVirtualInterfaceId :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceId = lens _dVirtualInterfaceId (\s a -> s {_dVirtualInterfaceId = a})

-- | The type of attachment.
dAttachmentType :: Lens' DirectConnectGatewayAttachment (Maybe DirectConnectGatewayAttachmentType)
dAttachmentType = lens _dAttachmentType (\s a -> s {_dAttachmentType = a})

instance FromJSON DirectConnectGatewayAttachment where
  parseJSON =
    withObject
      "DirectConnectGatewayAttachment"
      ( \x ->
          DirectConnectGatewayAttachment'
            <$> (x .:? "directConnectGatewayId")
            <*> (x .:? "attachmentState")
            <*> (x .:? "stateChangeError")
            <*> (x .:? "virtualInterfaceRegion")
            <*> (x .:? "virtualInterfaceOwnerAccount")
            <*> (x .:? "virtualInterfaceId")
            <*> (x .:? "attachmentType")
      )

instance Hashable DirectConnectGatewayAttachment

instance NFData DirectConnectGatewayAttachment
