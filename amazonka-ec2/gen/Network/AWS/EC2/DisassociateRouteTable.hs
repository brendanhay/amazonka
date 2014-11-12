{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DisassociateRouteTable
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
module Network.AWS.EC2.DisassociateRouteTable
    (
    -- * Request
      DisassociateRouteTable
    -- ** Request constructor
    , disassociateRouteTable
    -- ** Request lenses
    , drtAssociationId
    , drtDryRun

    -- * Response
    , DisassociateRouteTableResponse
    -- ** Response constructor
    , disassociateRouteTableResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DisassociateRouteTable = DisassociateRouteTable
    { _drtAssociationId :: Text
    , _drtDryRun        :: Maybe Bool
    } (Eq, Ord, Show, Generic)

-- | 'DisassociateRouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drtAssociationId' @::@ 'Text'
--
-- * 'drtDryRun' @::@ 'Maybe' 'Bool'
--
disassociateRouteTable :: Text -- ^ 'drtAssociationId'
                       -> DisassociateRouteTable
disassociateRouteTable p1 = DisassociateRouteTable
    { _drtAssociationId = p1
    , _drtDryRun        = Nothing
    }

-- | The association ID representing the current association between the route
-- table and subnet.
drtAssociationId :: Lens' DisassociateRouteTable Text
drtAssociationId = lens _drtAssociationId (\s a -> s { _drtAssociationId = a })

drtDryRun :: Lens' DisassociateRouteTable (Maybe Bool)
drtDryRun = lens _drtDryRun (\s a -> s { _drtDryRun = a })
instance ToQuery DisassociateRouteTable

instance ToPath DisassociateRouteTable where
    toPath = const "/"

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisassociateRouteTableResponse' constructor.
disassociateRouteTableResponse :: DisassociateRouteTableResponse
disassociateRouteTableResponse = DisassociateRouteTableResponse

instance FromXML DisassociateRouteTableResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisassociateRouteTableResponse"

instance AWSRequest DisassociateRouteTable where
    type Sv DisassociateRouteTable = EC2
    type Rs DisassociateRouteTable = DisassociateRouteTableResponse

    request  = post "DisassociateRouteTable"
    response = nullaryResponse DisassociateRouteTableResponse
