{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the route table associated with a given subnet in a VPC. After the
-- operation completes, the subnet uses the routes in the new route table it's
-- associated with. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide. You can also use
-- ReplaceRouteTableAssociation to change which table is the main route table
-- in the VPC. You just specify the main route table's association ID and the
-- route table to be the new main route table. Example This example starts
-- with a route table associated with a subnet, and a corresponding
-- association ID rtbassoc-f8ad4891. You want to associate a different route
-- table (table rtb-f9ad4890) to the subnet. The result is a new association
-- ID representing the new association.
-- https://ec2.amazonaws.com/?Action=ReplaceRouteTableAssociation
-- &amp;AssociationId=rtbassoc-f8ad4891 &amp;RouteTableId=rtb-f9ad4890
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE rtbassoc-faad4893.
module Network.AWS.EC2.V2014_06_15.ReplaceRouteTableAssociation
    (
    -- * Request
      ReplaceRouteTableAssociation
    -- ** Request constructor
    , mkReplaceRouteTableAssociation
    -- ** Request lenses
    , rrtaAssociationId
    , rrtaRouteTableId

    -- * Response
    , ReplaceRouteTableAssociationResponse
    -- ** Response lenses
    , rrtarsNewAssociationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { _rrtaAssociationId :: Text
    , _rrtaRouteTableId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceRouteTableAssociation' request.
mkReplaceRouteTableAssociation :: Text -- ^ 'rrtaAssociationId'
                               -> Text -- ^ 'rrtaRouteTableId'
                               -> ReplaceRouteTableAssociation
mkReplaceRouteTableAssociation p1 p2 = ReplaceRouteTableAssociation
    { _rrtaAssociationId = p1
    , _rrtaRouteTableId = p2
    }

-- | The association ID.
rrtaAssociationId :: Lens' ReplaceRouteTableAssociation Text
rrtaAssociationId =
    lens _rrtaAssociationId (\s a -> s { _rrtaAssociationId = a })

-- | The ID of the new route table to associate with the subnet.
rrtaRouteTableId :: Lens' ReplaceRouteTableAssociation Text
rrtaRouteTableId =
    lens _rrtaRouteTableId (\s a -> s { _rrtaRouteTableId = a })

instance ToQuery ReplaceRouteTableAssociation where
    toQuery = genericQuery def

-- | 
newtype ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { _rrtarsNewAssociationId :: Maybe Text
    } deriving (Show, Generic)

-- | The ID of the new association.
rrtarsNewAssociationId :: Lens' ReplaceRouteTableAssociationResponse (Maybe Text)
rrtarsNewAssociationId =
    lens _rrtarsNewAssociationId (\s a -> s { _rrtarsNewAssociationId = a })

instance FromXML ReplaceRouteTableAssociationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ReplaceRouteTableAssociation where
    type Sv ReplaceRouteTableAssociation = EC2
    type Rs ReplaceRouteTableAssociation = ReplaceRouteTableAssociationResponse

    request = post "ReplaceRouteTableAssociation"
    response _ = xmlResponse
