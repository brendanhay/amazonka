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
-- Module      : Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified VPC with the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation
  ( -- * Creating a Request
    createLocalGatewayRouteTableVPCAssociation,
    CreateLocalGatewayRouteTableVPCAssociation,

    -- * Request Lenses
    clgrtvaTagSpecifications,
    clgrtvaDryRun,
    clgrtvaLocalGatewayRouteTableId,
    clgrtvaVPCId,

    -- * Destructuring the Response
    createLocalGatewayRouteTableVPCAssociationResponse,
    CreateLocalGatewayRouteTableVPCAssociationResponse,

    -- * Response Lenses
    clgrtvarsLocalGatewayRouteTableVPCAssociation,
    clgrtvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLocalGatewayRouteTableVPCAssociation' smart constructor.
data CreateLocalGatewayRouteTableVPCAssociation = CreateLocalGatewayRouteTableVPCAssociation'
  { _clgrtvaTagSpecifications ::
      !( Maybe
           [TagSpecification]
       ),
    _clgrtvaDryRun ::
      !( Maybe
           Bool
       ),
    _clgrtvaLocalGatewayRouteTableId ::
      !Text,
    _clgrtvaVPCId ::
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

-- | Creates a value of 'CreateLocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgrtvaTagSpecifications' - The tags to assign to the local gateway route table VPC association.
--
-- * 'clgrtvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'clgrtvaLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'clgrtvaVPCId' - The ID of the VPC.
createLocalGatewayRouteTableVPCAssociation ::
  -- | 'clgrtvaLocalGatewayRouteTableId'
  Text ->
  -- | 'clgrtvaVPCId'
  Text ->
  CreateLocalGatewayRouteTableVPCAssociation
createLocalGatewayRouteTableVPCAssociation
  pLocalGatewayRouteTableId_
  pVPCId_ =
    CreateLocalGatewayRouteTableVPCAssociation'
      { _clgrtvaTagSpecifications =
          Nothing,
        _clgrtvaDryRun = Nothing,
        _clgrtvaLocalGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        _clgrtvaVPCId = pVPCId_
      }

-- | The tags to assign to the local gateway route table VPC association.
clgrtvaTagSpecifications :: Lens' CreateLocalGatewayRouteTableVPCAssociation [TagSpecification]
clgrtvaTagSpecifications = lens _clgrtvaTagSpecifications (\s a -> s {_clgrtvaTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
clgrtvaDryRun :: Lens' CreateLocalGatewayRouteTableVPCAssociation (Maybe Bool)
clgrtvaDryRun = lens _clgrtvaDryRun (\s a -> s {_clgrtvaDryRun = a})

-- | The ID of the local gateway route table.
clgrtvaLocalGatewayRouteTableId :: Lens' CreateLocalGatewayRouteTableVPCAssociation Text
clgrtvaLocalGatewayRouteTableId = lens _clgrtvaLocalGatewayRouteTableId (\s a -> s {_clgrtvaLocalGatewayRouteTableId = a})

-- | The ID of the VPC.
clgrtvaVPCId :: Lens' CreateLocalGatewayRouteTableVPCAssociation Text
clgrtvaVPCId = lens _clgrtvaVPCId (\s a -> s {_clgrtvaVPCId = a})

instance AWSRequest CreateLocalGatewayRouteTableVPCAssociation where
  type
    Rs CreateLocalGatewayRouteTableVPCAssociation =
      CreateLocalGatewayRouteTableVPCAssociationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteTableVPCAssociationResponse'
            <$> (x .@? "localGatewayRouteTableVpcAssociation")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateLocalGatewayRouteTableVPCAssociation

instance NFData CreateLocalGatewayRouteTableVPCAssociation

instance ToHeaders CreateLocalGatewayRouteTableVPCAssociation where
  toHeaders = const mempty

instance ToPath CreateLocalGatewayRouteTableVPCAssociation where
  toPath = const "/"

instance ToQuery CreateLocalGatewayRouteTableVPCAssociation where
  toQuery CreateLocalGatewayRouteTableVPCAssociation' {..} =
    mconcat
      [ "Action"
          =: ("CreateLocalGatewayRouteTableVpcAssociation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "TagSpecification" <$> _clgrtvaTagSpecifications),
        "DryRun" =: _clgrtvaDryRun,
        "LocalGatewayRouteTableId" =: _clgrtvaLocalGatewayRouteTableId,
        "VpcId" =: _clgrtvaVPCId
      ]

-- | /See:/ 'createLocalGatewayRouteTableVPCAssociationResponse' smart constructor.
data CreateLocalGatewayRouteTableVPCAssociationResponse = CreateLocalGatewayRouteTableVPCAssociationResponse'
  { _clgrtvarsLocalGatewayRouteTableVPCAssociation ::
      !( Maybe
           LocalGatewayRouteTableVPCAssociation
       ),
    _clgrtvarsResponseStatus ::
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

-- | Creates a value of 'CreateLocalGatewayRouteTableVPCAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgrtvarsLocalGatewayRouteTableVPCAssociation' - Information about the association.
--
-- * 'clgrtvarsResponseStatus' - -- | The response status code.
createLocalGatewayRouteTableVPCAssociationResponse ::
  -- | 'clgrtvarsResponseStatus'
  Int ->
  CreateLocalGatewayRouteTableVPCAssociationResponse
createLocalGatewayRouteTableVPCAssociationResponse pResponseStatus_ =
  CreateLocalGatewayRouteTableVPCAssociationResponse'
    { _clgrtvarsLocalGatewayRouteTableVPCAssociation =
        Nothing,
      _clgrtvarsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
clgrtvarsLocalGatewayRouteTableVPCAssociation :: Lens' CreateLocalGatewayRouteTableVPCAssociationResponse (Maybe LocalGatewayRouteTableVPCAssociation)
clgrtvarsLocalGatewayRouteTableVPCAssociation = lens _clgrtvarsLocalGatewayRouteTableVPCAssociation (\s a -> s {_clgrtvarsLocalGatewayRouteTableVPCAssociation = a})

-- | -- | The response status code.
clgrtvarsResponseStatus :: Lens' CreateLocalGatewayRouteTableVPCAssociationResponse Int
clgrtvarsResponseStatus = lens _clgrtvarsResponseStatus (\s a -> s {_clgrtvarsResponseStatus = a})

instance NFData CreateLocalGatewayRouteTableVPCAssociationResponse
