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
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.
module Network.AWS.EC2.DeleteNatGateway
  ( -- * Creating a Request
    deleteNatGateway,
    DeleteNatGateway,

    -- * Request Lenses
    dngnDryRun,
    dngnNatGatewayId,

    -- * Destructuring the Response
    deleteNatGatewayResponse,
    DeleteNatGatewayResponse,

    -- * Response Lenses
    dngnrsNatGatewayId,
    dngnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { _dngnDryRun ::
      !(Maybe Bool),
    _dngnNatGatewayId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteNatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dngnNatGatewayId' - The ID of the NAT gateway.
deleteNatGateway ::
  -- | 'dngnNatGatewayId'
  Text ->
  DeleteNatGateway
deleteNatGateway pNatGatewayId_ =
  DeleteNatGateway'
    { _dngnDryRun = Nothing,
      _dngnNatGatewayId = pNatGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dngnDryRun :: Lens' DeleteNatGateway (Maybe Bool)
dngnDryRun = lens _dngnDryRun (\s a -> s {_dngnDryRun = a})

-- | The ID of the NAT gateway.
dngnNatGatewayId :: Lens' DeleteNatGateway Text
dngnNatGatewayId = lens _dngnNatGatewayId (\s a -> s {_dngnNatGatewayId = a})

instance AWSRequest DeleteNatGateway where
  type Rs DeleteNatGateway = DeleteNatGatewayResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteNatGatewayResponse'
            <$> (x .@? "natGatewayId") <*> (pure (fromEnum s))
      )

instance Hashable DeleteNatGateway

instance NFData DeleteNatGateway

instance ToHeaders DeleteNatGateway where
  toHeaders = const mempty

instance ToPath DeleteNatGateway where
  toPath = const "/"

instance ToQuery DeleteNatGateway where
  toQuery DeleteNatGateway' {..} =
    mconcat
      [ "Action" =: ("DeleteNatGateway" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dngnDryRun,
        "NatGatewayId" =: _dngnNatGatewayId
      ]

-- | /See:/ 'deleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { _dngnrsNatGatewayId ::
      !(Maybe Text),
    _dngnrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteNatGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngnrsNatGatewayId' - The ID of the NAT gateway.
--
-- * 'dngnrsResponseStatus' - -- | The response status code.
deleteNatGatewayResponse ::
  -- | 'dngnrsResponseStatus'
  Int ->
  DeleteNatGatewayResponse
deleteNatGatewayResponse pResponseStatus_ =
  DeleteNatGatewayResponse'
    { _dngnrsNatGatewayId = Nothing,
      _dngnrsResponseStatus = pResponseStatus_
    }

-- | The ID of the NAT gateway.
dngnrsNatGatewayId :: Lens' DeleteNatGatewayResponse (Maybe Text)
dngnrsNatGatewayId = lens _dngnrsNatGatewayId (\s a -> s {_dngnrsNatGatewayId = a})

-- | -- | The response status code.
dngnrsResponseStatus :: Lens' DeleteNatGatewayResponse Int
dngnrsResponseStatus = lens _dngnrsResponseStatus (\s a -> s {_dngnrsResponseStatus = a})

instance NFData DeleteNatGatewayResponse
