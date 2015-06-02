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

-- Module      : Network.AWS.EC2.MoveAddressToVpc
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

-- | Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC
-- platform. The Elastic IP address must be allocated to your account, and it
-- must not be associated with an instance. After the Elastic IP address is
-- moved, it is no longer available for use in the EC2-Classic platform, unless
-- you move it back using the 'RestoreAddressToClassic' request. You cannot move
-- an Elastic IP address that's allocated for use in the EC2-VPC platform to the
-- EC2-Classic platform.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MoveAddressToVpc.html>
module Network.AWS.EC2.MoveAddressToVpc
    (
    -- * Request
      MoveAddressToVpc
    -- ** Request constructor
    , moveAddressToVpc
    -- ** Request lenses
    , matvDryRun
    , matvPublicIp

    -- * Response
    , MoveAddressToVpcResponse
    -- ** Response constructor
    , moveAddressToVpcResponse
    -- ** Response lenses
    , matvrAllocationId
    , matvrStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data MoveAddressToVpc = MoveAddressToVpc
    { _matvDryRun   :: Maybe Bool
    , _matvPublicIp :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'MoveAddressToVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'matvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'matvPublicIp' @::@ 'Text'
--
moveAddressToVpc :: Text -- ^ 'matvPublicIp'
                 -> MoveAddressToVpc
moveAddressToVpc p1 = MoveAddressToVpc
    { _matvPublicIp = p1
    , _matvDryRun   = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
matvDryRun :: Lens' MoveAddressToVpc (Maybe Bool)
matvDryRun = lens _matvDryRun (\s a -> s { _matvDryRun = a })

-- | The Elastic IP address.
matvPublicIp :: Lens' MoveAddressToVpc Text
matvPublicIp = lens _matvPublicIp (\s a -> s { _matvPublicIp = a })

data MoveAddressToVpcResponse = MoveAddressToVpcResponse
    { _matvrAllocationId :: Maybe Text
    , _matvrStatus       :: Maybe Status
    } deriving (Eq, Read, Show)

-- | 'MoveAddressToVpcResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'matvrAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'matvrStatus' @::@ 'Maybe' 'Status'
--
moveAddressToVpcResponse :: MoveAddressToVpcResponse
moveAddressToVpcResponse = MoveAddressToVpcResponse
    { _matvrAllocationId = Nothing
    , _matvrStatus       = Nothing
    }

-- | The allocation ID for the Elastic IP address.
matvrAllocationId :: Lens' MoveAddressToVpcResponse (Maybe Text)
matvrAllocationId =
    lens _matvrAllocationId (\s a -> s { _matvrAllocationId = a })

-- | The status of the move of the IP address.
matvrStatus :: Lens' MoveAddressToVpcResponse (Maybe Status)
matvrStatus = lens _matvrStatus (\s a -> s { _matvrStatus = a })

instance ToPath MoveAddressToVpc where
    toPath = const "/"

instance ToQuery MoveAddressToVpc where
    toQuery MoveAddressToVpc{..} = mconcat
        [ "DryRun"   =? _matvDryRun
        , "PublicIp" =? _matvPublicIp
        ]

instance ToHeaders MoveAddressToVpc

instance AWSRequest MoveAddressToVpc where
    type Sv MoveAddressToVpc = EC2
    type Rs MoveAddressToVpc = MoveAddressToVpcResponse

    request  = post "MoveAddressToVpc"
    response = xmlResponse

instance FromXML MoveAddressToVpcResponse where
    parseXML x = MoveAddressToVpcResponse
        <$> x .@? "allocationId"
        <*> x .@? "status"
