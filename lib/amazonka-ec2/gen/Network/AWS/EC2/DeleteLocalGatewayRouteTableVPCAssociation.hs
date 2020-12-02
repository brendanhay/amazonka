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
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified association between a VPC and local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation
  ( -- * Creating a Request
    deleteLocalGatewayRouteTableVPCAssociation,
    DeleteLocalGatewayRouteTableVPCAssociation,

    -- * Request Lenses
    dlgrtvaDryRun,
    dlgrtvaLocalGatewayRouteTableVPCAssociationId,

    -- * Destructuring the Response
    deleteLocalGatewayRouteTableVPCAssociationResponse,
    DeleteLocalGatewayRouteTableVPCAssociationResponse,

    -- * Response Lenses
    dlgrtvarsLocalGatewayRouteTableVPCAssociation,
    dlgrtvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLocalGatewayRouteTableVPCAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVPCAssociation = DeleteLocalGatewayRouteTableVPCAssociation'
  { _dlgrtvaDryRun ::
      !( Maybe
           Bool
       ),
    _dlgrtvaLocalGatewayRouteTableVPCAssociationId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteLocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlgrtvaLocalGatewayRouteTableVPCAssociationId' - The ID of the association.
deleteLocalGatewayRouteTableVPCAssociation ::
  -- | 'dlgrtvaLocalGatewayRouteTableVPCAssociationId'
  Text ->
  DeleteLocalGatewayRouteTableVPCAssociation
deleteLocalGatewayRouteTableVPCAssociation
  pLocalGatewayRouteTableVPCAssociationId_ =
    DeleteLocalGatewayRouteTableVPCAssociation'
      { _dlgrtvaDryRun =
          Nothing,
        _dlgrtvaLocalGatewayRouteTableVPCAssociationId =
          pLocalGatewayRouteTableVPCAssociationId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlgrtvaDryRun :: Lens' DeleteLocalGatewayRouteTableVPCAssociation (Maybe Bool)
dlgrtvaDryRun = lens _dlgrtvaDryRun (\s a -> s {_dlgrtvaDryRun = a})

-- | The ID of the association.
dlgrtvaLocalGatewayRouteTableVPCAssociationId :: Lens' DeleteLocalGatewayRouteTableVPCAssociation Text
dlgrtvaLocalGatewayRouteTableVPCAssociationId = lens _dlgrtvaLocalGatewayRouteTableVPCAssociationId (\s a -> s {_dlgrtvaLocalGatewayRouteTableVPCAssociationId = a})

instance AWSRequest DeleteLocalGatewayRouteTableVPCAssociation where
  type
    Rs DeleteLocalGatewayRouteTableVPCAssociation =
      DeleteLocalGatewayRouteTableVPCAssociationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVPCAssociationResponse'
            <$> (x .@? "localGatewayRouteTableVpcAssociation")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteLocalGatewayRouteTableVPCAssociation

instance NFData DeleteLocalGatewayRouteTableVPCAssociation

instance ToHeaders DeleteLocalGatewayRouteTableVPCAssociation where
  toHeaders = const mempty

instance ToPath DeleteLocalGatewayRouteTableVPCAssociation where
  toPath = const "/"

instance ToQuery DeleteLocalGatewayRouteTableVPCAssociation where
  toQuery DeleteLocalGatewayRouteTableVPCAssociation' {..} =
    mconcat
      [ "Action"
          =: ("DeleteLocalGatewayRouteTableVpcAssociation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dlgrtvaDryRun,
        "LocalGatewayRouteTableVpcAssociationId"
          =: _dlgrtvaLocalGatewayRouteTableVPCAssociationId
      ]

-- | /See:/ 'deleteLocalGatewayRouteTableVPCAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVPCAssociationResponse = DeleteLocalGatewayRouteTableVPCAssociationResponse'
  { _dlgrtvarsLocalGatewayRouteTableVPCAssociation ::
      !( Maybe
           LocalGatewayRouteTableVPCAssociation
       ),
    _dlgrtvarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteLocalGatewayRouteTableVPCAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtvarsLocalGatewayRouteTableVPCAssociation' - Information about the association.
--
-- * 'dlgrtvarsResponseStatus' - -- | The response status code.
deleteLocalGatewayRouteTableVPCAssociationResponse ::
  -- | 'dlgrtvarsResponseStatus'
  Int ->
  DeleteLocalGatewayRouteTableVPCAssociationResponse
deleteLocalGatewayRouteTableVPCAssociationResponse pResponseStatus_ =
  DeleteLocalGatewayRouteTableVPCAssociationResponse'
    { _dlgrtvarsLocalGatewayRouteTableVPCAssociation =
        Nothing,
      _dlgrtvarsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
dlgrtvarsLocalGatewayRouteTableVPCAssociation :: Lens' DeleteLocalGatewayRouteTableVPCAssociationResponse (Maybe LocalGatewayRouteTableVPCAssociation)
dlgrtvarsLocalGatewayRouteTableVPCAssociation = lens _dlgrtvarsLocalGatewayRouteTableVPCAssociation (\s a -> s {_dlgrtvarsLocalGatewayRouteTableVPCAssociation = a})

-- | -- | The response status code.
dlgrtvarsResponseStatus :: Lens' DeleteLocalGatewayRouteTableVPCAssociationResponse Int
dlgrtvarsResponseStatus = lens _dlgrtvarsResponseStatus (\s a -> s {_dlgrtvarsResponseStatus = a})

instance NFData DeleteLocalGatewayRouteTableVPCAssociationResponse
