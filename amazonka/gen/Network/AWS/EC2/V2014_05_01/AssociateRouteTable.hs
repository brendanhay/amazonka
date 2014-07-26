{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.AssociateRouteTable
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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;associationId&gt;rtbassoc-f8ad4891&lt;/associationId&gt;
-- &lt;/AssociateRouteTableResponse&gt;.
module Network.AWS.EC2.V2014_05_01.AssociateRouteTable where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data AssociateRouteTable = AssociateRouteTable
    { _artrRouteTableId :: Text
      -- ^ The ID of the route table.
    , _artrSubnetId :: Text
      -- ^ The ID of the subnet.
    , _artrDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery AssociateRouteTable where
    toQuery = genericToQuery def

instance AWSRequest AssociateRouteTable where
    type Sv AssociateRouteTable = EC2
    type Rs AssociateRouteTable = AssociateRouteTableResponse

    request = post "AssociateRouteTable"

    response _ = xmlResponse

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { _artsAssociationId :: Maybe Text
      -- ^ The route table association ID (needed to disassociate the route
      -- table).
    } deriving (Generic)

instance FromXML AssociateRouteTableResponse where
    fromXMLOptions = xmlOptions
