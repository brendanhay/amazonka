{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates a subnet with a route table. The subnet and route table must be
-- in the same VPC. This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The action
-- returns an association ID, which you need in order to disassociate the
-- route table from the subnet later. A route table can be associated with
-- multiple subnets. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide. Example This example
-- associates a route table with the ID rtb-e4ad488d with a subnet with the ID
-- subnet-15ad487c. https://ec2.amazonaws.com/?Action=AssociateRouteTable
-- &amp;RouteTableId=rtb-e4ad488d &amp;SubnetId=subnet-15ad487c
-- &lt;AssociateRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;associationId&gt;rtbassoc-f8ad4891&lt;/associationId&gt;
-- &lt;/AssociateRouteTableResponse&gt;.
module Network.AWS.EC2.AssociateRouteTable
    (
    -- * Request
      AssociateRouteTable
    -- ** Request constructor
    , mkAssociateRouteTable
    -- ** Request lenses
    , artSubnetId
    , artRouteTableId

    -- * Response
    , AssociateRouteTableResponse
    -- ** Response constructor
    , mkAssociateRouteTableResponse
    -- ** Response lenses
    , artrAssociationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AssociateRouteTable = AssociateRouteTable
    { _artSubnetId :: !Text
    , _artRouteTableId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateRouteTable' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetId ::@ @Text@
--
-- * @RouteTableId ::@ @Text@
--
mkAssociateRouteTable :: Text -- ^ 'artSubnetId'
                      -> Text -- ^ 'artRouteTableId'
                      -> AssociateRouteTable
mkAssociateRouteTable p1 p2 = AssociateRouteTable
    { _artSubnetId = p1
    , _artRouteTableId = p2
    }

-- | The ID of the subnet.
artSubnetId :: Lens' AssociateRouteTable Text
artSubnetId = lens _artSubnetId (\s a -> s { _artSubnetId = a })

-- | The ID of the route table.
artRouteTableId :: Lens' AssociateRouteTable Text
artRouteTableId = lens _artRouteTableId (\s a -> s { _artRouteTableId = a })

instance ToQuery AssociateRouteTable where
    toQuery = genericQuery def

newtype AssociateRouteTableResponse = AssociateRouteTableResponse
    { _artrAssociationId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateRouteTableResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AssociationId ::@ @Maybe Text@
--
mkAssociateRouteTableResponse :: AssociateRouteTableResponse
mkAssociateRouteTableResponse = AssociateRouteTableResponse
    { _artrAssociationId = Nothing
    }

-- | The route table association ID (needed to disassociate the route table).
artrAssociationId :: Lens' AssociateRouteTableResponse (Maybe Text)
artrAssociationId =
    lens _artrAssociationId (\s a -> s { _artrAssociationId = a })

instance FromXML AssociateRouteTableResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AssociateRouteTable where
    type Sv AssociateRouteTable = EC2
    type Rs AssociateRouteTable = AssociateRouteTableResponse

    request = post "AssociateRouteTable"
    response _ = xmlResponse
