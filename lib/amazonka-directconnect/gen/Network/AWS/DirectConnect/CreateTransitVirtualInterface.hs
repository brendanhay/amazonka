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
-- Module      : Network.AWS.DirectConnect.CreateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit virtual interface. A transit virtual interface should be used to access one or more transit gateways associated with Direct Connect gateways. A transit virtual interface enables the connection of multiple VPCs attached to a transit gateway to a Direct Connect gateway.
--
--
-- /Important:/ If you associate your transit gateway with one or more Direct Connect gateways, the Autonomous System Number (ASN) used by the transit gateway and the Direct Connect gateway must be different. For example, if you use the default ASN 64512 for both your the transit gateway and Direct Connect gateway, the association request fails.
--
-- Setting the MTU of a virtual interface to 8500 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.CreateTransitVirtualInterface
  ( -- * Creating a Request
    createTransitVirtualInterface,
    CreateTransitVirtualInterface,

    -- * Request Lenses
    ctviConnectionId,
    ctviNewTransitVirtualInterface,

    -- * Destructuring the Response
    createTransitVirtualInterfaceResponse,
    CreateTransitVirtualInterfaceResponse,

    -- * Response Lenses
    ctvirsVirtualInterface,
    ctvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitVirtualInterface' smart constructor.
data CreateTransitVirtualInterface = CreateTransitVirtualInterface'
  { _ctviConnectionId ::
      !Text,
    _ctviNewTransitVirtualInterface ::
      !NewTransitVirtualInterface
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransitVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctviConnectionId' - The ID of the connection.
--
-- * 'ctviNewTransitVirtualInterface' - Information about the transit virtual interface.
createTransitVirtualInterface ::
  -- | 'ctviConnectionId'
  Text ->
  -- | 'ctviNewTransitVirtualInterface'
  NewTransitVirtualInterface ->
  CreateTransitVirtualInterface
createTransitVirtualInterface
  pConnectionId_
  pNewTransitVirtualInterface_ =
    CreateTransitVirtualInterface'
      { _ctviConnectionId =
          pConnectionId_,
        _ctviNewTransitVirtualInterface = pNewTransitVirtualInterface_
      }

-- | The ID of the connection.
ctviConnectionId :: Lens' CreateTransitVirtualInterface Text
ctviConnectionId = lens _ctviConnectionId (\s a -> s {_ctviConnectionId = a})

-- | Information about the transit virtual interface.
ctviNewTransitVirtualInterface :: Lens' CreateTransitVirtualInterface NewTransitVirtualInterface
ctviNewTransitVirtualInterface = lens _ctviNewTransitVirtualInterface (\s a -> s {_ctviNewTransitVirtualInterface = a})

instance AWSRequest CreateTransitVirtualInterface where
  type
    Rs CreateTransitVirtualInterface =
      CreateTransitVirtualInterfaceResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          CreateTransitVirtualInterfaceResponse'
            <$> (x .?> "virtualInterface") <*> (pure (fromEnum s))
      )

instance Hashable CreateTransitVirtualInterface

instance NFData CreateTransitVirtualInterface

instance ToHeaders CreateTransitVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.CreateTransitVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTransitVirtualInterface where
  toJSON CreateTransitVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("connectionId" .= _ctviConnectionId),
            Just
              ("newTransitVirtualInterface" .= _ctviNewTransitVirtualInterface)
          ]
      )

instance ToPath CreateTransitVirtualInterface where
  toPath = const "/"

instance ToQuery CreateTransitVirtualInterface where
  toQuery = const mempty

-- | /See:/ 'createTransitVirtualInterfaceResponse' smart constructor.
data CreateTransitVirtualInterfaceResponse = CreateTransitVirtualInterfaceResponse'
  { _ctvirsVirtualInterface ::
      !( Maybe
           VirtualInterface
       ),
    _ctvirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctvirsVirtualInterface' - Undocumented member.
--
-- * 'ctvirsResponseStatus' - -- | The response status code.
createTransitVirtualInterfaceResponse ::
  -- | 'ctvirsResponseStatus'
  Int ->
  CreateTransitVirtualInterfaceResponse
createTransitVirtualInterfaceResponse pResponseStatus_ =
  CreateTransitVirtualInterfaceResponse'
    { _ctvirsVirtualInterface =
        Nothing,
      _ctvirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ctvirsVirtualInterface :: Lens' CreateTransitVirtualInterfaceResponse (Maybe VirtualInterface)
ctvirsVirtualInterface = lens _ctvirsVirtualInterface (\s a -> s {_ctvirsVirtualInterface = a})

-- | -- | The response status code.
ctvirsResponseStatus :: Lens' CreateTransitVirtualInterfaceResponse Int
ctvirsResponseStatus = lens _ctvirsResponseStatus (\s a -> s {_ctvirsResponseStatus = a})

instance NFData CreateTransitVirtualInterfaceResponse
