{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EC2.V2014_06_15.ReplaceRouteTableAssociation where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ReplaceRouteTableAssociation' request.
replaceRouteTableAssociation :: Text -- ^ '_rrtarAssociationId'
                             -> Text -- ^ '_rrtarRouteTableId'
                             -> ReplaceRouteTableAssociation
replaceRouteTableAssociation p1 p2 = ReplaceRouteTableAssociation
    { _rrtarAssociationId = p1
    , _rrtarRouteTableId = p2
    , _rrtarDryRun = Nothing
    }

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { _rrtarAssociationId :: Text
      -- ^ The association ID.
    , _rrtarRouteTableId :: Text
      -- ^ The ID of the new route table to associate with the subnet.
    , _rrtarDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''ReplaceRouteTableAssociation

instance ToQuery ReplaceRouteTableAssociation where
    toQuery = genericQuery def

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { _rrtasNewAssociationId :: Maybe Text
      -- ^ The ID of the new association.
    } deriving (Show, Generic)

makeLenses ''ReplaceRouteTableAssociationResponse

instance FromXML ReplaceRouteTableAssociationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ReplaceRouteTableAssociation where
    type Sv ReplaceRouteTableAssociation = EC2
    type Rs ReplaceRouteTableAssociation = ReplaceRouteTableAssociationResponse

    request = post "ReplaceRouteTableAssociation"
    response _ = xmlResponse
