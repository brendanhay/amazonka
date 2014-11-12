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

-- Module      : Network.AWS.EC2.AcceptVpcPeeringConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept a VPC peering connection request. To accept a request, the VPC
-- peering connection must be in the pending-acceptance state, and you must be
-- the owner of the peer VPC. Use the DescribeVpcPeeringConnections request to
-- view your outstanding VPC peering connection requests.
module Network.AWS.EC2.AcceptVpcPeeringConnection
    (
    -- * Request
      AcceptVpcPeeringConnection
    -- ** Request constructor
    , acceptVpcPeeringConnection
    -- ** Request lenses
    , avpcDryRun
    , avpcVpcPeeringConnectionId

    -- * Response
    , AcceptVpcPeeringConnectionResult
    -- ** Response constructor
    , acceptVpcPeeringConnectionResult
    -- ** Response lenses
    , avpcrVpcPeeringConnection
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data AcceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcDryRun                 :: Maybe Bool
    , _avpcVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AcceptVpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'avpcVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
acceptVpcPeeringConnection :: AcceptVpcPeeringConnection
acceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcDryRun                 = Nothing
    , _avpcVpcPeeringConnectionId = Nothing
    }

avpcDryRun :: Lens' AcceptVpcPeeringConnection (Maybe Bool)
avpcDryRun = lens _avpcDryRun (\s a -> s { _avpcDryRun = a })

-- | The ID of the VPC peering connection.
avpcVpcPeeringConnectionId :: Lens' AcceptVpcPeeringConnection (Maybe Text)
avpcVpcPeeringConnectionId =
    lens _avpcVpcPeeringConnectionId
        (\s a -> s { _avpcVpcPeeringConnectionId = a })

instance ToQuery AcceptVpcPeeringConnection

instance ToPath AcceptVpcPeeringConnection where
    toPath = const "/"

newtype AcceptVpcPeeringConnectionResult = AcceptVpcPeeringConnectionResult
    { _avpcrVpcPeeringConnection :: Maybe VpcPeeringConnection
    } deriving (Eq, Show, Generic)

-- | 'AcceptVpcPeeringConnectionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcrVpcPeeringConnection' @::@ 'Maybe' 'VpcPeeringConnection'
--
acceptVpcPeeringConnectionResult :: AcceptVpcPeeringConnectionResult
acceptVpcPeeringConnectionResult = AcceptVpcPeeringConnectionResult
    { _avpcrVpcPeeringConnection = Nothing
    }

-- | Information about the VPC peering connection.
avpcrVpcPeeringConnection :: Lens' AcceptVpcPeeringConnectionResult (Maybe VpcPeeringConnection)
avpcrVpcPeeringConnection =
    lens _avpcrVpcPeeringConnection
        (\s a -> s { _avpcrVpcPeeringConnection = a })

instance FromXML AcceptVpcPeeringConnectionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AcceptVpcPeeringConnectionResult"

instance AWSRequest AcceptVpcPeeringConnection where
    type Sv AcceptVpcPeeringConnection = EC2
    type Rs AcceptVpcPeeringConnection = AcceptVpcPeeringConnectionResult

    request  = post "AcceptVpcPeeringConnection"
    response = xmlResponse $ \h x -> AcceptVpcPeeringConnectionResult
        <$> x %| "vpcPeeringConnection"
