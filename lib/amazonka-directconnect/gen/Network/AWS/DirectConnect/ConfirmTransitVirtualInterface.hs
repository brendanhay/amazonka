{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a transit virtual interface created by another AWS account.
--
--
-- After the owner of the transit virtual interface makes this call, the specified transit virtual interface is created and made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
  ( -- * Creating a Request
    confirmTransitVirtualInterface,
    ConfirmTransitVirtualInterface,

    -- * Request Lenses
    ctviVirtualInterfaceId,
    ctviDirectConnectGatewayId,

    -- * Destructuring the Response
    confirmTransitVirtualInterfaceResponse,
    ConfirmTransitVirtualInterfaceResponse,

    -- * Response Lenses
    conrsVirtualInterfaceState,
    conrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'confirmTransitVirtualInterface' smart constructor.
data ConfirmTransitVirtualInterface = ConfirmTransitVirtualInterface'
  { _ctviVirtualInterfaceId ::
      !Text,
    _ctviDirectConnectGatewayId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfirmTransitVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctviVirtualInterfaceId' - The ID of the virtual interface.
--
-- * 'ctviDirectConnectGatewayId' - The ID of the Direct Connect gateway.
confirmTransitVirtualInterface ::
  -- | 'ctviVirtualInterfaceId'
  Text ->
  -- | 'ctviDirectConnectGatewayId'
  Text ->
  ConfirmTransitVirtualInterface
confirmTransitVirtualInterface
  pVirtualInterfaceId_
  pDirectConnectGatewayId_ =
    ConfirmTransitVirtualInterface'
      { _ctviVirtualInterfaceId =
          pVirtualInterfaceId_,
        _ctviDirectConnectGatewayId = pDirectConnectGatewayId_
      }

-- | The ID of the virtual interface.
ctviVirtualInterfaceId :: Lens' ConfirmTransitVirtualInterface Text
ctviVirtualInterfaceId = lens _ctviVirtualInterfaceId (\s a -> s {_ctviVirtualInterfaceId = a})

-- | The ID of the Direct Connect gateway.
ctviDirectConnectGatewayId :: Lens' ConfirmTransitVirtualInterface Text
ctviDirectConnectGatewayId = lens _ctviDirectConnectGatewayId (\s a -> s {_ctviDirectConnectGatewayId = a})

instance AWSRequest ConfirmTransitVirtualInterface where
  type
    Rs ConfirmTransitVirtualInterface =
      ConfirmTransitVirtualInterfaceResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          ConfirmTransitVirtualInterfaceResponse'
            <$> (x .?> "virtualInterfaceState") <*> (pure (fromEnum s))
      )

instance Hashable ConfirmTransitVirtualInterface

instance NFData ConfirmTransitVirtualInterface

instance ToHeaders ConfirmTransitVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.ConfirmTransitVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ConfirmTransitVirtualInterface where
  toJSON ConfirmTransitVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("virtualInterfaceId" .= _ctviVirtualInterfaceId),
            Just ("directConnectGatewayId" .= _ctviDirectConnectGatewayId)
          ]
      )

instance ToPath ConfirmTransitVirtualInterface where
  toPath = const "/"

instance ToQuery ConfirmTransitVirtualInterface where
  toQuery = const mempty

-- | /See:/ 'confirmTransitVirtualInterfaceResponse' smart constructor.
data ConfirmTransitVirtualInterfaceResponse = ConfirmTransitVirtualInterfaceResponse'
  { _conrsVirtualInterfaceState ::
      !( Maybe
           VirtualInterfaceState
       ),
    _conrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfirmTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'conrsVirtualInterfaceState' - The state of the virtual interface. The following are the possible values:     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.     * @available@ : A virtual interface that is able to forward traffic.     * @down@ : A virtual interface that is BGP down.     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.     * @deleted@ : A virtual interface that cannot forward traffic.     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.     * @unknown@ : The state of the virtual interface is not available.
--
-- * 'conrsResponseStatus' - -- | The response status code.
confirmTransitVirtualInterfaceResponse ::
  -- | 'conrsResponseStatus'
  Int ->
  ConfirmTransitVirtualInterfaceResponse
confirmTransitVirtualInterfaceResponse pResponseStatus_ =
  ConfirmTransitVirtualInterfaceResponse'
    { _conrsVirtualInterfaceState =
        Nothing,
      _conrsResponseStatus = pResponseStatus_
    }

-- | The state of the virtual interface. The following are the possible values:     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.     * @available@ : A virtual interface that is able to forward traffic.     * @down@ : A virtual interface that is BGP down.     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.     * @deleted@ : A virtual interface that cannot forward traffic.     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.     * @unknown@ : The state of the virtual interface is not available.
conrsVirtualInterfaceState :: Lens' ConfirmTransitVirtualInterfaceResponse (Maybe VirtualInterfaceState)
conrsVirtualInterfaceState = lens _conrsVirtualInterfaceState (\s a -> s {_conrsVirtualInterfaceState = a})

-- | -- | The response status code.
conrsResponseStatus :: Lens' ConfirmTransitVirtualInterfaceResponse Int
conrsResponseStatus = lens _conrsResponseStatus (\s a -> s {_conrsResponseStatus = a})

instance NFData ConfirmTransitVirtualInterfaceResponse
