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
-- Module      : Network.AWS.EC2.DeleteCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a carrier gateway.
--
--
-- /Important:/ If you do not delete the route that contains the carrier gateway as the Target, the route is a blackhole route. For information about how to delete a route, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteRoute.html DeleteRoute> .
module Network.AWS.EC2.DeleteCarrierGateway
  ( -- * Creating a Request
    deleteCarrierGateway,
    DeleteCarrierGateway,

    -- * Request Lenses
    dcgcDryRun,
    dcgcCarrierGatewayId,

    -- * Destructuring the Response
    deleteCarrierGatewayResponse,
    DeleteCarrierGatewayResponse,

    -- * Response Lenses
    dcgrsCarrierGateway,
    dcgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCarrierGateway' smart constructor.
data DeleteCarrierGateway = DeleteCarrierGateway'
  { _dcgcDryRun ::
      !(Maybe Bool),
    _dcgcCarrierGatewayId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCarrierGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcgcCarrierGatewayId' - The ID of the carrier gateway.
deleteCarrierGateway ::
  -- | 'dcgcCarrierGatewayId'
  Text ->
  DeleteCarrierGateway
deleteCarrierGateway pCarrierGatewayId_ =
  DeleteCarrierGateway'
    { _dcgcDryRun = Nothing,
      _dcgcCarrierGatewayId = pCarrierGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcgcDryRun :: Lens' DeleteCarrierGateway (Maybe Bool)
dcgcDryRun = lens _dcgcDryRun (\s a -> s {_dcgcDryRun = a})

-- | The ID of the carrier gateway.
dcgcCarrierGatewayId :: Lens' DeleteCarrierGateway Text
dcgcCarrierGatewayId = lens _dcgcCarrierGatewayId (\s a -> s {_dcgcCarrierGatewayId = a})

instance AWSRequest DeleteCarrierGateway where
  type Rs DeleteCarrierGateway = DeleteCarrierGatewayResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteCarrierGatewayResponse'
            <$> (x .@? "carrierGateway") <*> (pure (fromEnum s))
      )

instance Hashable DeleteCarrierGateway

instance NFData DeleteCarrierGateway

instance ToHeaders DeleteCarrierGateway where
  toHeaders = const mempty

instance ToPath DeleteCarrierGateway where
  toPath = const "/"

instance ToQuery DeleteCarrierGateway where
  toQuery DeleteCarrierGateway' {..} =
    mconcat
      [ "Action" =: ("DeleteCarrierGateway" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dcgcDryRun,
        "CarrierGatewayId" =: _dcgcCarrierGatewayId
      ]

-- | /See:/ 'deleteCarrierGatewayResponse' smart constructor.
data DeleteCarrierGatewayResponse = DeleteCarrierGatewayResponse'
  { _dcgrsCarrierGateway ::
      !(Maybe CarrierGateway),
    _dcgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCarrierGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgrsCarrierGateway' - Information about the carrier gateway.
--
-- * 'dcgrsResponseStatus' - -- | The response status code.
deleteCarrierGatewayResponse ::
  -- | 'dcgrsResponseStatus'
  Int ->
  DeleteCarrierGatewayResponse
deleteCarrierGatewayResponse pResponseStatus_ =
  DeleteCarrierGatewayResponse'
    { _dcgrsCarrierGateway = Nothing,
      _dcgrsResponseStatus = pResponseStatus_
    }

-- | Information about the carrier gateway.
dcgrsCarrierGateway :: Lens' DeleteCarrierGatewayResponse (Maybe CarrierGateway)
dcgrsCarrierGateway = lens _dcgrsCarrierGateway (\s a -> s {_dcgrsCarrierGateway = a})

-- | -- | The response status code.
dcgrsResponseStatus :: Lens' DeleteCarrierGatewayResponse Int
dcgrsResponseStatus = lens _dcgrsResponseStatus (\s a -> s {_dcgrsResponseStatus = a})

instance NFData DeleteCarrierGatewayResponse
