{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- in the Amazon Virtual Private Cloud User Guide.
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
    , artRouteTableId
    , artSubnetId

    -- * Response
    , AssociateRouteTableResponse
    -- ** Response constructor
    , associateRouteTableResponse
    -- ** Response lenses
    , artrAssociationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AssociateRouteTable = AssociateRouteTable
    { _artDryRun       :: Maybe Bool
    , _artRouteTableId :: Text
    , _artSubnetId     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssociateRouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'artRouteTableId' @::@ 'Text'
--
-- * 'artSubnetId' @::@ 'Text'
--
associateRouteTable :: Text -- ^ 'artSubnetId'
                    -> Text -- ^ 'artRouteTableId'
                    -> AssociateRouteTable
associateRouteTable p1 p2 = AssociateRouteTable
    { _artSubnetId     = p1
    , _artRouteTableId = p2
    , _artDryRun       = Nothing
    }

artDryRun :: Lens' AssociateRouteTable (Maybe Bool)
artDryRun = lens _artDryRun (\s a -> s { _artDryRun = a })

-- | The ID of the route table.
artRouteTableId :: Lens' AssociateRouteTable Text
artRouteTableId = lens _artRouteTableId (\s a -> s { _artRouteTableId = a })

-- | The ID of the subnet.
artSubnetId :: Lens' AssociateRouteTable Text
artSubnetId = lens _artSubnetId (\s a -> s { _artSubnetId = a })

newtype AssociateRouteTableResponse = AssociateRouteTableResponse
    { _artrAssociationId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AssociateRouteTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artrAssociationId' @::@ 'Maybe' 'Text'
--
associateRouteTableResponse :: AssociateRouteTableResponse
associateRouteTableResponse = AssociateRouteTableResponse
    { _artrAssociationId = Nothing
    }

-- | The route table association ID (needed to disassociate the route table).
artrAssociationId :: Lens' AssociateRouteTableResponse (Maybe Text)
artrAssociationId =
    lens _artrAssociationId (\s a -> s { _artrAssociationId = a })

instance ToPath AssociateRouteTable where
    toPath = const "/"

instance ToQuery AssociateRouteTable

instance ToHeaders AssociateRouteTable

instance AWSRequest AssociateRouteTable where
    type Sv AssociateRouteTable = EC2
    type Rs AssociateRouteTable = AssociateRouteTableResponse

    request  = post "AssociateRouteTable"
    response = xmlResponse

instance FromXML AssociateRouteTableResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssociateRouteTableResponse"
