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
-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the route table associated with a given subnet, internet gateway, or virtual private gateway in a VPC. After the operation completes, the subnet or gateway uses the routes in the new route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- You can also use this operation to change which table is the main route table in the VPC. Specify the main route table's association ID and the route table ID of the new main route table.
module Network.AWS.EC2.ReplaceRouteTableAssociation
  ( -- * Creating a Request
    replaceRouteTableAssociation,
    ReplaceRouteTableAssociation,

    -- * Request Lenses
    rrtaDryRun,
    rrtaAssociationId,
    rrtaRouteTableId,

    -- * Destructuring the Response
    replaceRouteTableAssociationResponse,
    ReplaceRouteTableAssociationResponse,

    -- * Response Lenses
    rrtarsNewAssociationId,
    rrtarsAssociationState,
    rrtarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'replaceRouteTableAssociation' smart constructor.
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
  { _rrtaDryRun ::
      !(Maybe Bool),
    _rrtaAssociationId :: !Text,
    _rrtaRouteTableId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceRouteTableAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrtaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rrtaAssociationId' - The association ID.
--
-- * 'rrtaRouteTableId' - The ID of the new route table to associate with the subnet.
replaceRouteTableAssociation ::
  -- | 'rrtaAssociationId'
  Text ->
  -- | 'rrtaRouteTableId'
  Text ->
  ReplaceRouteTableAssociation
replaceRouteTableAssociation pAssociationId_ pRouteTableId_ =
  ReplaceRouteTableAssociation'
    { _rrtaDryRun = Nothing,
      _rrtaAssociationId = pAssociationId_,
      _rrtaRouteTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rrtaDryRun :: Lens' ReplaceRouteTableAssociation (Maybe Bool)
rrtaDryRun = lens _rrtaDryRun (\s a -> s {_rrtaDryRun = a})

-- | The association ID.
rrtaAssociationId :: Lens' ReplaceRouteTableAssociation Text
rrtaAssociationId = lens _rrtaAssociationId (\s a -> s {_rrtaAssociationId = a})

-- | The ID of the new route table to associate with the subnet.
rrtaRouteTableId :: Lens' ReplaceRouteTableAssociation Text
rrtaRouteTableId = lens _rrtaRouteTableId (\s a -> s {_rrtaRouteTableId = a})

instance AWSRequest ReplaceRouteTableAssociation where
  type
    Rs ReplaceRouteTableAssociation =
      ReplaceRouteTableAssociationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ReplaceRouteTableAssociationResponse'
            <$> (x .@? "newAssociationId")
            <*> (x .@? "associationState")
            <*> (pure (fromEnum s))
      )

instance Hashable ReplaceRouteTableAssociation

instance NFData ReplaceRouteTableAssociation

instance ToHeaders ReplaceRouteTableAssociation where
  toHeaders = const mempty

instance ToPath ReplaceRouteTableAssociation where
  toPath = const "/"

instance ToQuery ReplaceRouteTableAssociation where
  toQuery ReplaceRouteTableAssociation' {..} =
    mconcat
      [ "Action" =: ("ReplaceRouteTableAssociation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _rrtaDryRun,
        "AssociationId" =: _rrtaAssociationId,
        "RouteTableId" =: _rrtaRouteTableId
      ]

-- | /See:/ 'replaceRouteTableAssociationResponse' smart constructor.
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
  { _rrtarsNewAssociationId ::
      !(Maybe Text),
    _rrtarsAssociationState ::
      !( Maybe
           RouteTableAssociationState
       ),
    _rrtarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceRouteTableAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrtarsNewAssociationId' - The ID of the new association.
--
-- * 'rrtarsAssociationState' - The state of the association.
--
-- * 'rrtarsResponseStatus' - -- | The response status code.
replaceRouteTableAssociationResponse ::
  -- | 'rrtarsResponseStatus'
  Int ->
  ReplaceRouteTableAssociationResponse
replaceRouteTableAssociationResponse pResponseStatus_ =
  ReplaceRouteTableAssociationResponse'
    { _rrtarsNewAssociationId =
        Nothing,
      _rrtarsAssociationState = Nothing,
      _rrtarsResponseStatus = pResponseStatus_
    }

-- | The ID of the new association.
rrtarsNewAssociationId :: Lens' ReplaceRouteTableAssociationResponse (Maybe Text)
rrtarsNewAssociationId = lens _rrtarsNewAssociationId (\s a -> s {_rrtarsNewAssociationId = a})

-- | The state of the association.
rrtarsAssociationState :: Lens' ReplaceRouteTableAssociationResponse (Maybe RouteTableAssociationState)
rrtarsAssociationState = lens _rrtarsAssociationState (\s a -> s {_rrtarsAssociationState = a})

-- | -- | The response status code.
rrtarsResponseStatus :: Lens' ReplaceRouteTableAssociationResponse Int
rrtarsResponseStatus = lens _rrtarsResponseStatus (\s a -> s {_rrtarsResponseStatus = a})

instance NFData ReplaceRouteTableAssociationResponse
