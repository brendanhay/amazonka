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

-- Module      : Network.AWS.EC2.RejectVpcPeeringConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Rejects a VPC peering connection request. The VPC peering connection must be
-- in the 'pending-acceptance' state. Use the 'DescribeVpcPeeringConnections'
-- request to view your outstanding VPC peering connection requests. To delete
-- an active VPC peering connection, or to delete a VPC peering connection
-- request that you initiated, use 'DeleteVpcPeeringConnection'.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RejectVpcPeeringConnection.html>
module Network.AWS.EC2.RejectVpcPeeringConnection
    (
    -- * Request
      RejectVpcPeeringConnection
    -- ** Request constructor
    , rejectVpcPeeringConnection
    -- ** Request lenses
    , rvpcDryRun
    , rvpcVpcPeeringConnectionId

    -- * Response
    , RejectVpcPeeringConnectionResponse
    -- ** Response constructor
    , rejectVpcPeeringConnectionResponse
    -- ** Response lenses
    , rvpcrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RejectVpcPeeringConnection = RejectVpcPeeringConnection
    { _rvpcDryRun                 :: Maybe Bool
    , _rvpcVpcPeeringConnectionId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RejectVpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvpcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rvpcVpcPeeringConnectionId' @::@ 'Text'
--
rejectVpcPeeringConnection :: Text -- ^ 'rvpcVpcPeeringConnectionId'
                           -> RejectVpcPeeringConnection
rejectVpcPeeringConnection p1 = RejectVpcPeeringConnection
    { _rvpcVpcPeeringConnectionId = p1
    , _rvpcDryRun                 = Nothing
    }

rvpcDryRun :: Lens' RejectVpcPeeringConnection (Maybe Bool)
rvpcDryRun = lens _rvpcDryRun (\s a -> s { _rvpcDryRun = a })

-- | The ID of the VPC peering connection.
rvpcVpcPeeringConnectionId :: Lens' RejectVpcPeeringConnection Text
rvpcVpcPeeringConnectionId =
    lens _rvpcVpcPeeringConnectionId
        (\s a -> s { _rvpcVpcPeeringConnectionId = a })

newtype RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcrReturn :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'RejectVpcPeeringConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvpcrReturn' @::@ 'Maybe' 'Bool'
--
rejectVpcPeeringConnectionResponse :: RejectVpcPeeringConnectionResponse
rejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcrReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
rvpcrReturn :: Lens' RejectVpcPeeringConnectionResponse (Maybe Bool)
rvpcrReturn = lens _rvpcrReturn (\s a -> s { _rvpcrReturn = a })

instance ToPath RejectVpcPeeringConnection where
    toPath = const "/"

instance ToQuery RejectVpcPeeringConnection where
    toQuery RejectVpcPeeringConnection{..} = mconcat
        [ "dryRun"                 =? _rvpcDryRun
        , "vpcPeeringConnectionId" =? _rvpcVpcPeeringConnectionId
        ]

instance ToHeaders RejectVpcPeeringConnection

instance AWSRequest RejectVpcPeeringConnection where
    type Sv RejectVpcPeeringConnection = EC2
    type Rs RejectVpcPeeringConnection = RejectVpcPeeringConnectionResponse

    request  = post "RejectVpcPeeringConnection"
    response = xmlResponse

instance FromXML RejectVpcPeeringConnectionResponse where
    parseXML x = RejectVpcPeeringConnectionResponse
        <$> x .@? "return"
