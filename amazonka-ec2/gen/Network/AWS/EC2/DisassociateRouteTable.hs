{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html
-- Route Tables> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateRouteTable.html>
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
import qualified GHC.Exts

data DisassociateRouteTable = DisassociateRouteTable
    { _drtAssociationId :: Text
    , _drtDryRun        :: Maybe Bool
    } deriving (Eq, Ord, Show)

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

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisassociateRouteTableResponse' constructor.
disassociateRouteTableResponse :: DisassociateRouteTableResponse
disassociateRouteTableResponse = DisassociateRouteTableResponse

instance ToPath DisassociateRouteTable where
    toPath = const "/"

instance ToQuery DisassociateRouteTable where
    toQuery DisassociateRouteTable{..} = mconcat
        [ "associationId" =? _drtAssociationId
        , "dryRun"        =? _drtDryRun
        ]

instance ToHeaders DisassociateRouteTable

instance AWSRequest DisassociateRouteTable where
    type Sv DisassociateRouteTable = EC2
    type Rs DisassociateRouteTable = DisassociateRouteTableResponse

    request  = post "DisassociateRouteTable"
    response = nullResponse DisassociateRouteTableResponse
