{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AssociateRouteTable
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
module Network.AWS.EC2.V2014_06_15.AssociateRouteTable where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AssociateRouteTable' request.
associateRouteTable :: Text -- ^ '_artrSubnetId'
                    -> Text -- ^ '_artrRouteTableId'
                    -> AssociateRouteTable
associateRouteTable p1 p2 = AssociateRouteTable
    { _artrSubnetId = p1
    , _artrRouteTableId = p2
    , _artrDryRun = Nothing
    }

data AssociateRouteTable = AssociateRouteTable
    { _artrSubnetId :: Text
      -- ^ The ID of the subnet.
    , _artrRouteTableId :: Text
      -- ^ The ID of the route table.
    , _artrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''AssociateRouteTable

instance ToQuery AssociateRouteTable where
    toQuery = genericQuery def

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { _artsAssociationId :: Maybe Text
      -- ^ The route table association ID (needed to disassociate the route
      -- table).
    } deriving (Show, Generic)

makeLenses ''AssociateRouteTableResponse

instance FromXML AssociateRouteTableResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AssociateRouteTable where
    type Sv AssociateRouteTable = EC2
    type Rs AssociateRouteTable = AssociateRouteTableResponse

    request = post "AssociateRouteTable"
    response _ = xmlResponse
