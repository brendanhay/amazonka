{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DisassociateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disassociates a subnet from a route table. After you perform this action,
-- the subnet no longer uses the routes in the route table. Instead, it uses
-- the routes in the VPC's main route table. For more information about route
-- tables, see Route Tables in the Amazon Virtual Private Cloud User Guide.
-- Example This example disassociates the specified route table from the
-- subnet it's associated to.
-- https://ec2.amazonaws.com/?Action=DisassociateRouteTable
-- &amp;AssociationId=rtbassoc-fdad4894 &amp;AUTHPARAMS
-- &lt;DisassociateRouteTableResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DisassociateRouteTableResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DisassociateRouteTable
    (
    -- * Request
      DisassociateRouteTable
    -- ** Request constructor
    , mkDisassociateRouteTableRequest
    -- ** Request lenses
    , drtuAssociationId

    -- * Response
    , DisassociateRouteTableResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisassociateRouteTable' request.
mkDisassociateRouteTableRequest :: Text -- ^ 'drtuAssociationId'
                                -> DisassociateRouteTable
mkDisassociateRouteTableRequest p1 = DisassociateRouteTable
    { _drtuAssociationId = p1
    }
{-# INLINE mkDisassociateRouteTableRequest #-}

newtype DisassociateRouteTable = DisassociateRouteTable
    { _drtuAssociationId :: Text
      -- ^ The association ID representing the current association between
      -- the route table and subnet.
    } deriving (Show, Generic)

-- | The association ID representing the current association between the route
-- table and subnet.
drtuAssociationId :: Lens' DisassociateRouteTable (Text)
drtuAssociationId = lens _drtuAssociationId (\s a -> s { _drtuAssociationId = a })
{-# INLINE drtuAssociationId #-}

instance ToQuery DisassociateRouteTable where
    toQuery = genericQuery def

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DisassociateRouteTable where
    type Sv DisassociateRouteTable = EC2
    type Rs DisassociateRouteTable = DisassociateRouteTableResponse

    request = post "DisassociateRouteTable"
    response _ = nullaryResponse DisassociateRouteTableResponse
