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
-- Module      : Network.AWS.DirectConnect.AllocateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a transit virtual interface to be owned by the specified AWS account. Use this type of interface to connect a transit gateway to your Direct Connect gateway.
--
--
-- The owner of a connection provisions a transit virtual interface to be owned by the specified AWS account.
--
-- After you create a transit virtual interface, it must be confirmed by the owner using 'ConfirmTransitVirtualInterface' . Until this step has been completed, the transit virtual interface is in the @requested@ state and is not available to handle traffic.
module Network.AWS.DirectConnect.AllocateTransitVirtualInterface
  ( -- * Creating a Request
    allocateTransitVirtualInterface,
    AllocateTransitVirtualInterface,

    -- * Request Lenses
    atviConnectionId,
    atviOwnerAccount,
    atviNewTransitVirtualInterfaceAllocation,

    -- * Destructuring the Response
    allocateTransitVirtualInterfaceResponse,
    AllocateTransitVirtualInterfaceResponse,

    -- * Response Lenses
    atvirsVirtualInterface,
    atvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateTransitVirtualInterface' smart constructor.
data AllocateTransitVirtualInterface = AllocateTransitVirtualInterface'
  { _atviConnectionId ::
      !Text,
    _atviOwnerAccount :: !Text,
    _atviNewTransitVirtualInterfaceAllocation ::
      !NewTransitVirtualInterfaceAllocation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateTransitVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atviConnectionId' - The ID of the connection on which the transit virtual interface is provisioned.
--
-- * 'atviOwnerAccount' - The ID of the AWS account that owns the transit virtual interface.
--
-- * 'atviNewTransitVirtualInterfaceAllocation' - Information about the transit virtual interface.
allocateTransitVirtualInterface ::
  -- | 'atviConnectionId'
  Text ->
  -- | 'atviOwnerAccount'
  Text ->
  -- | 'atviNewTransitVirtualInterfaceAllocation'
  NewTransitVirtualInterfaceAllocation ->
  AllocateTransitVirtualInterface
allocateTransitVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewTransitVirtualInterfaceAllocation_ =
    AllocateTransitVirtualInterface'
      { _atviConnectionId =
          pConnectionId_,
        _atviOwnerAccount = pOwnerAccount_,
        _atviNewTransitVirtualInterfaceAllocation =
          pNewTransitVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the transit virtual interface is provisioned.
atviConnectionId :: Lens' AllocateTransitVirtualInterface Text
atviConnectionId = lens _atviConnectionId (\s a -> s {_atviConnectionId = a})

-- | The ID of the AWS account that owns the transit virtual interface.
atviOwnerAccount :: Lens' AllocateTransitVirtualInterface Text
atviOwnerAccount = lens _atviOwnerAccount (\s a -> s {_atviOwnerAccount = a})

-- | Information about the transit virtual interface.
atviNewTransitVirtualInterfaceAllocation :: Lens' AllocateTransitVirtualInterface NewTransitVirtualInterfaceAllocation
atviNewTransitVirtualInterfaceAllocation = lens _atviNewTransitVirtualInterfaceAllocation (\s a -> s {_atviNewTransitVirtualInterfaceAllocation = a})

instance AWSRequest AllocateTransitVirtualInterface where
  type
    Rs AllocateTransitVirtualInterface =
      AllocateTransitVirtualInterfaceResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          AllocateTransitVirtualInterfaceResponse'
            <$> (x .?> "virtualInterface") <*> (pure (fromEnum s))
      )

instance Hashable AllocateTransitVirtualInterface

instance NFData AllocateTransitVirtualInterface

instance ToHeaders AllocateTransitVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.AllocateTransitVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AllocateTransitVirtualInterface where
  toJSON AllocateTransitVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("connectionId" .= _atviConnectionId),
            Just ("ownerAccount" .= _atviOwnerAccount),
            Just
              ( "newTransitVirtualInterfaceAllocation"
                  .= _atviNewTransitVirtualInterfaceAllocation
              )
          ]
      )

instance ToPath AllocateTransitVirtualInterface where
  toPath = const "/"

instance ToQuery AllocateTransitVirtualInterface where
  toQuery = const mempty

-- | /See:/ 'allocateTransitVirtualInterfaceResponse' smart constructor.
data AllocateTransitVirtualInterfaceResponse = AllocateTransitVirtualInterfaceResponse'
  { _atvirsVirtualInterface ::
      !( Maybe
           VirtualInterface
       ),
    _atvirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atvirsVirtualInterface' - Undocumented member.
--
-- * 'atvirsResponseStatus' - -- | The response status code.
allocateTransitVirtualInterfaceResponse ::
  -- | 'atvirsResponseStatus'
  Int ->
  AllocateTransitVirtualInterfaceResponse
allocateTransitVirtualInterfaceResponse pResponseStatus_ =
  AllocateTransitVirtualInterfaceResponse'
    { _atvirsVirtualInterface =
        Nothing,
      _atvirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
atvirsVirtualInterface :: Lens' AllocateTransitVirtualInterfaceResponse (Maybe VirtualInterface)
atvirsVirtualInterface = lens _atvirsVirtualInterface (\s a -> s {_atvirsVirtualInterface = a})

-- | -- | The response status code.
atvirsResponseStatus :: Lens' AllocateTransitVirtualInterfaceResponse Int
atvirsResponseStatus = lens _atvirsResponseStatus (\s a -> s {_atvirsResponseStatus = a})

instance NFData AllocateTransitVirtualInterfaceResponse
