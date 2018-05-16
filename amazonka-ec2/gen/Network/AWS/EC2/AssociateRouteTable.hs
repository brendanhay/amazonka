{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a subnet with a route table. The subnet and route table must be in the same VPC. This association causes traffic originating from the subnet to be routed according to the routes in the route table. The action returns an association ID, which you need in order to disassociate the route table from the subnet later. A route table can be associated with multiple subnets.
--
--
-- For more information about route tables, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.AssociateRouteTable
    (
    -- * Creating a Request
      associateRouteTable
    , AssociateRouteTable
    -- * Request Lenses
    , artDryRun
    , artRouteTableId
    , artSubnetId

    -- * Destructuring the Response
    , associateRouteTableResponse
    , AssociateRouteTableResponse
    -- * Response Lenses
    , artrsAssociationId
    , artrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AssociateRouteTable.
--
--
--
-- /See:/ 'associateRouteTable' smart constructor.
data AssociateRouteTable = AssociateRouteTable'
  { _artDryRun       :: !(Maybe Bool)
  , _artRouteTableId :: !Text
  , _artSubnetId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'artRouteTableId' - The ID of the route table.
--
-- * 'artSubnetId' - The ID of the subnet.
associateRouteTable
    :: Text -- ^ 'artRouteTableId'
    -> Text -- ^ 'artSubnetId'
    -> AssociateRouteTable
associateRouteTable pRouteTableId_ pSubnetId_ =
  AssociateRouteTable'
    { _artDryRun = Nothing
    , _artRouteTableId = pRouteTableId_
    , _artSubnetId = pSubnetId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
artDryRun :: Lens' AssociateRouteTable (Maybe Bool)
artDryRun = lens _artDryRun (\ s a -> s{_artDryRun = a})

-- | The ID of the route table.
artRouteTableId :: Lens' AssociateRouteTable Text
artRouteTableId = lens _artRouteTableId (\ s a -> s{_artRouteTableId = a})

-- | The ID of the subnet.
artSubnetId :: Lens' AssociateRouteTable Text
artSubnetId = lens _artSubnetId (\ s a -> s{_artSubnetId = a})

instance AWSRequest AssociateRouteTable where
        type Rs AssociateRouteTable =
             AssociateRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateRouteTableResponse' <$>
                   (x .@? "associationId") <*> (pure (fromEnum s)))

instance Hashable AssociateRouteTable where

instance NFData AssociateRouteTable where

instance ToHeaders AssociateRouteTable where
        toHeaders = const mempty

instance ToPath AssociateRouteTable where
        toPath = const "/"

instance ToQuery AssociateRouteTable where
        toQuery AssociateRouteTable'{..}
          = mconcat
              ["Action" =: ("AssociateRouteTable" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _artDryRun,
               "RouteTableId" =: _artRouteTableId,
               "SubnetId" =: _artSubnetId]

-- | Contains the output of AssociateRouteTable.
--
--
--
-- /See:/ 'associateRouteTableResponse' smart constructor.
data AssociateRouteTableResponse = AssociateRouteTableResponse'
  { _artrsAssociationId  :: !(Maybe Text)
  , _artrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artrsAssociationId' - The route table association ID (needed to disassociate the route table).
--
-- * 'artrsResponseStatus' - -- | The response status code.
associateRouteTableResponse
    :: Int -- ^ 'artrsResponseStatus'
    -> AssociateRouteTableResponse
associateRouteTableResponse pResponseStatus_ =
  AssociateRouteTableResponse'
    {_artrsAssociationId = Nothing, _artrsResponseStatus = pResponseStatus_}


-- | The route table association ID (needed to disassociate the route table).
artrsAssociationId :: Lens' AssociateRouteTableResponse (Maybe Text)
artrsAssociationId = lens _artrsAssociationId (\ s a -> s{_artrsAssociationId = a})

-- | -- | The response status code.
artrsResponseStatus :: Lens' AssociateRouteTableResponse Int
artrsResponseStatus = lens _artrsResponseStatus (\ s a -> s{_artrsResponseStatus = a})

instance NFData AssociateRouteTableResponse where
