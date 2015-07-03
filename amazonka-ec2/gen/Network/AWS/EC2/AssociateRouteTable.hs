{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Associates a subnet with a route table. The subnet and route table must
-- be in the same VPC. This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The
-- action returns an association ID, which you need in order to
-- disassociate the route table from the subnet later. A route table can be
-- associated with multiple subnets.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateRouteTable.html>
module Network.AWS.EC2.AssociateRouteTable
    (
    -- * Request
      AssociateRouteTable
    -- ** Request constructor
    , associateRouteTable
    -- ** Request lenses
    , artDryRun
    , artSubnetId
    , artRouteTableId

    -- * Response
    , AssociateRouteTableResponse
    -- ** Response constructor
    , associateRouteTableResponse
    -- ** Response lenses
    , artrAssociationId
    , artrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateRouteTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artDryRun'
--
-- * 'artSubnetId'
--
-- * 'artRouteTableId'
data AssociateRouteTable = AssociateRouteTable'
    { _artDryRun       :: !(Maybe Bool)
    , _artSubnetId     :: !Text
    , _artRouteTableId :: !Text
    } deriving (Eq,Read,Show)

-- | 'AssociateRouteTable' smart constructor.
associateRouteTable :: Text -> Text -> AssociateRouteTable
associateRouteTable pSubnetId pRouteTableId =
    AssociateRouteTable'
    { _artDryRun = Nothing
    , _artSubnetId = pSubnetId
    , _artRouteTableId = pRouteTableId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
artDryRun :: Lens' AssociateRouteTable (Maybe Bool)
artDryRun = lens _artDryRun (\ s a -> s{_artDryRun = a});

-- | The ID of the subnet.
artSubnetId :: Lens' AssociateRouteTable Text
artSubnetId = lens _artSubnetId (\ s a -> s{_artSubnetId = a});

-- | The ID of the route table.
artRouteTableId :: Lens' AssociateRouteTable Text
artRouteTableId = lens _artRouteTableId (\ s a -> s{_artRouteTableId = a});

instance AWSRequest AssociateRouteTable where
        type Sv AssociateRouteTable = EC2
        type Rs AssociateRouteTable =
             AssociateRouteTableResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AssociateRouteTableResponse' <$>
                   (x .@? "associationId") <*> (pure (fromEnum s)))

instance ToHeaders AssociateRouteTable where
        toHeaders = const mempty

instance ToPath AssociateRouteTable where
        toPath = const "/"

instance ToQuery AssociateRouteTable where
        toQuery AssociateRouteTable'{..}
          = mconcat
              ["Action" =: ("AssociateRouteTable" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _artDryRun, "SubnetId" =: _artSubnetId,
               "RouteTableId" =: _artRouteTableId]

-- | /See:/ 'associateRouteTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artrAssociationId'
--
-- * 'artrStatus'
data AssociateRouteTableResponse = AssociateRouteTableResponse'
    { _artrAssociationId :: !(Maybe Text)
    , _artrStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'AssociateRouteTableResponse' smart constructor.
associateRouteTableResponse :: Int -> AssociateRouteTableResponse
associateRouteTableResponse pStatus =
    AssociateRouteTableResponse'
    { _artrAssociationId = Nothing
    , _artrStatus = pStatus
    }

-- | The route table association ID (needed to disassociate the route table).
artrAssociationId :: Lens' AssociateRouteTableResponse (Maybe Text)
artrAssociationId = lens _artrAssociationId (\ s a -> s{_artrAssociationId = a});

-- | FIXME: Undocumented member.
artrStatus :: Lens' AssociateRouteTableResponse Int
artrStatus = lens _artrStatus (\ s a -> s{_artrStatus = a});
